{-# LANGUAGE UndecidableInstances #-}

-- | This modules provides a specification for our blockchain monads, in three
-- layers:
--
-- 1. MonadBlockChainBalancing provides what's needing for balancing purposes
--
-- 2. MonadBlockChainWithoutValidation adds up remaining primitives without
-- transaction validation
--
-- 3. MonadBlockChain concludes with the addition of transaction validation,
-- thus modifying the internal index of outputs
--
-- In addition, you will find here many helpers functions which can be derived
-- from the core definition of our blockchain.
module Cooked.MockChain.BlockChain
  ( MockChainError (..),
    MockChainLogEntry (..),
    MonadBlockChainBalancing (..),
    MonadBlockChainWithoutValidation (..),
    MonadBlockChain (..),
    AsTrans (..),
    currentMSRange,
    utxosFromCardanoTx,
    currentSlot,
    awaitSlot,
    getEnclosingSlot,
    awaitEnclosingSlot,
    waitNMSFromSlotLowerBound,
    waitNMSFromSlotUpperBound,
    slotRangeBefore,
    slotRangeAfter,
    slotToMSRange,
    txSkelInputScripts,
    txSkelInputValue,
    lookupUtxos,
    validateTxSkel',
    validateTxSkel_,
    txSkelDepositedValueInProposals,
    govActionDeposit,
    defineM,
    txSkelAllScripts,
    previewByRef,
    viewByRef,
    dRepDeposit,
    stakeAddressDeposit,
    stakePoolDeposit,
    txSkelDepositedValueInCertificates,
  )
where

import Cardano.Api.Ledger qualified as Cardano
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Node.Emulator qualified as Emulator
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Cooked.Pretty.Hashable
import Cooked.Pretty.Plutus ()
import Cooked.Skeleton
import Cooked.Wallet
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Ledger.Index qualified as Ledger
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import ListT
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- * Mockchain errors

-- | Errors that can be produced by the blockchain
data MockChainError
  = -- | Validation errors, either in Phase 1 or Phase 2
    MCEValidationError Ledger.ValidationPhase Ledger.ValidationError
  | -- | The balancing wallet does not have enough funds
    MCEUnbalanceable Wallet Api.Value
  | -- | The balancing wallet is required but missing
    MCEMissingBalancingWallet String
  | -- | No suitable collateral could be associated with a skeleton
    MCENoSuitableCollateral Integer Integer Api.Value
  | -- | Translating a skeleton element to its Cardano counterpart failed
    MCEToCardanoError String Ledger.ToCardanoError
  | -- | The required reference script is missing from a witness utxo
    MCEWrongReferenceScriptError Api.TxOutRef Api.ScriptHash (Maybe Api.ScriptHash)
  | -- | A UTxO is missing from the mockchain state
    MCEUnknownOutRef Api.TxOutRef
  | -- | A jump in time would result in a past slot
    MCEPastSlot Ledger.Slot Ledger.Slot
  | -- | An attempt to invoke an unsupported feature has been made
    MCEUnsupportedFeature String
  | -- | Used to provide 'MonadFail' instances.
    FailWith String
  deriving (Show, Eq)

-- * Mockchain logs

-- | This represents the specific events that should be logged when processing
-- transactions. If a new kind of event arises, then a new constructor should be
-- provided here.
data MockChainLogEntry
  = -- | Logging a Skeleton as it is submitted by the user.
    MCLogSubmittedTxSkel TxSkel
  | -- | Logging a Skeleton as it has been adjusted by the balancing mechanism,
    -- alongside fee, and possible collateral utxos and return collateral wallet.
    MCLogAdjustedTxSkel TxSkel Integer (Maybe (Set Api.TxOutRef, Wallet))
  | -- | Logging the successful validation of a new transaction, with its id and
    -- number of produced outputs.
    MCLogNewTx Api.TxId Integer
  | -- | Logging the fact that utxos provided by the user for balancing have to be
    -- discarded for a specific reason.
    MCLogDiscardedUtxos Integer String
  | -- | Logging the fact that utxos provided as collaterals will not be used
    -- because the transaction does not involve scripts. There are 2 cases,
    -- depending on whether the user has provided an explicit wallet or a set of
    -- utxos to be used as collaterals.
    MCLogUnusedCollaterals (Either Wallet (Set Api.TxOutRef))
  | -- | Logging the automatic addition of a reference script
    MCLogAddedReferenceScript TxSkelRedeemer Api.TxOutRef Script.ScriptHash
  | -- | Logging the automatic adjusment of a min ada amount
    MCLogAdjustedTxSkelOut TxSkelOut Api.Lovelace
  deriving (Show)

-- * Mockchain layers

-- | This is the first layer of our blockchain, which provides the minimal
-- subset of primitives required to perform balancing.
class (MonadFail m, MonadError MockChainError m) => MonadBlockChainBalancing m where
  -- | Returns the emulator parameters, including protocol parameters
  getParams :: m Emulator.Params

  -- | Returns a list of all UTxOs at a certain address.
  utxosAt :: (Script.ToAddress a) => a -> m [(Api.TxOutRef, TxSkelOut)]

  -- | Returns an output given a reference to it. If the output does not exist,
  -- throws a 'MCEUnknownOutRef' error.
  txSkelOutByRef :: Api.TxOutRef -> m TxSkelOut

  -- | Logs an event that occured during a BlockChain run
  logEvent :: MockChainLogEntry -> m ()

-- | This is the second layer of our blockchain, which provides all the other
-- blockchain primitives not needed for balancing, except transaction
-- validation. This layers is the one where
-- 'Cooked.MockChain.Tweak.Common.Tweak's are plugged to.
class (MonadBlockChainBalancing m) => MonadBlockChainWithoutValidation m where
  -- | Returns a list of all currently known outputs.
  allUtxos :: m [(Api.TxOutRef, TxSkelOut)]

  -- | Updates parameters
  setParams :: Emulator.Params -> m ()

  -- | Wait a certain amount of slot. Throws 'MCEPastSlot' if the input integer
  -- is negative. Returns the slot after jumping in time.
  waitNSlots :: (Integral i) => i -> m Ledger.Slot

  -- | Binds a hashable quantity of type @a@ to a variable in the mockchain,
  -- while registering its alias for printing purposes.
  define :: (ToHash a) => String -> a -> m a

  -- | Sets the current script to act as the official constitution script
  setConstitutionScript :: (ToVScript s) => s -> m ()

  -- | Gets the current official constitution script
  getConstitutionScript :: m (Maybe VScript)

  -- | Registers a staking credential with a given reward and deposit
  registerStakingCred :: (Script.ToCredential c) => c -> Integer -> Integer -> m ()

-- | The final layer of our blockchain, adding transaction validation to the
-- mix. This is the only primitive that actually modifies the ledger state.
class (MonadBlockChainWithoutValidation m) => MonadBlockChain m where
  -- | Generates, balances and validates a transaction from a skeleton. It
  -- returns the validated transaction and updates the state of the
  -- blockchain.
  validateTxSkel :: TxSkel -> m Ledger.CardanoTx

  -- | Forces the generation of utxos corresponding to certain 'TxSkelOut'
  forceOutputs :: [TxSkelOut] -> m [Api.TxOutRef]

-- * Mockchain helpers

-- | Retrieves an output and views a specific element out of it
viewByRef :: (MonadBlockChainBalancing m, Is g A_Getter) => Optic' g is TxSkelOut c -> Api.TxOutRef -> m c
viewByRef optic = (view optic <$>) . txSkelOutByRef

-- | Retrieves an output and previews a specific element out of it
previewByRef :: (MonadBlockChainBalancing m, Is af An_AffineFold) => Optic' af is TxSkelOut c -> Api.TxOutRef -> m (Maybe c)
previewByRef optic = (preview optic <$>) . txSkelOutByRef

-- | Validates a skeleton, and retuns the ordered list of produced output
-- references
validateTxSkel' :: (MonadBlockChain m) => TxSkel -> m [Api.TxOutRef]
validateTxSkel' = ((fmap fst <$>) . utxosFromCardanoTx) <=< validateTxSkel

-- | Validates a skeleton, and erases the outputs
validateTxSkel_ :: (MonadBlockChain m) => TxSkel -> m ()
validateTxSkel_ = void . validateTxSkel

-- | Retrieves the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
utxosFromCardanoTx :: (MonadBlockChainBalancing m) => Ledger.CardanoTx -> m [(Api.TxOutRef, TxSkelOut)]
utxosFromCardanoTx =
  mapM
    ( \(_, txIn) ->
        let txOutRef = Ledger.fromCardanoTxIn txIn
         in (txOutRef,) <$> txSkelOutByRef txOutRef
    )
    . Ledger.getCardanoTxOutRefs

-- | Like 'define', but binds the result of a monadic computation instead
defineM :: (MonadBlockChainWithoutValidation m, ToHash a) => String -> m a -> m a
defineM name = (define name =<<)

-- | Retrieves the required governance action deposit amount
govActionDeposit :: (MonadBlockChainBalancing m) => m Api.Lovelace
govActionDeposit = Api.Lovelace . Cardano.unCoin . Lens.view Conway.ppGovActionDepositL . Emulator.emulatorPParams <$> getParams

-- | Retrieves the required drep deposit amount
dRepDeposit :: (MonadBlockChainBalancing m) => m Api.Lovelace
dRepDeposit = Api.Lovelace . Cardano.unCoin . Lens.view Conway.ppDRepDepositL . Emulator.emulatorPParams <$> getParams

-- | Retrieves the required stake address deposit amount
stakeAddressDeposit :: (MonadBlockChainBalancing m) => m Api.Lovelace
stakeAddressDeposit = Api.Lovelace . Cardano.unCoin . Lens.view Conway.ppKeyDepositL . Emulator.emulatorPParams <$> getParams

-- | Retrieves the required stake pool deposit amount
stakePoolDeposit :: (MonadBlockChainBalancing m) => m Api.Lovelace
stakePoolDeposit = Api.Lovelace . Cardano.unCoin . Lens.view Conway.ppPoolDepositL . Emulator.emulatorPParams <$> getParams

-- | Retrieves the total amount of lovelace deposited in proposals in this
-- skeleton (equal to `govActionDeposit` times the number of proposals).
txSkelDepositedValueInProposals :: (MonadBlockChainBalancing m) => TxSkel -> m Api.Lovelace
txSkelDepositedValueInProposals TxSkel {txSkelProposals} = Api.Lovelace . (toInteger (length txSkelProposals) *) . Api.getLovelace <$> govActionDeposit

-- | Retrieves the total amount of lovelace deposited in certificates in this
-- skeleton. Note that unregistering a staking address or a dRep lead to a
-- negative deposit (a withdrawal, in fact) which means this function can return
-- a negative amount of lovelace, which is intended. The deposited amounts are
-- dictated by the current protocol parameters, and computed as such.
txSkelDepositedValueInCertificates :: (MonadBlockChainBalancing m) => TxSkel -> m Api.Lovelace
txSkelDepositedValueInCertificates txSkel = do
  sDep <- stakeAddressDeposit
  dDep <- dRepDeposit
  return $
    foldOf
      ( txSkelCertificatesL
          % traversed
          % txSkelCertificateActionAT @ReqEither
          % to
            ( \case
                StakingRegister {} -> sDep
                StakingRegisterDelegate {} -> sDep
                StakingUnRegister {} -> -sDep
                DRepRegister {} -> dDep
                DRepUnRegister {} -> -dDep
                _ -> Api.Lovelace 0
            )
      )
      txSkel

-- | Returns all scripts which guard transaction inputs
txSkelInputScripts :: (MonadBlockChainBalancing m) => TxSkel -> m [VScript]
txSkelInputScripts = fmap catMaybes . mapM (previewByRef (txSkelOutScriptAT % to Script.toVersioned)) . Map.keys . txSkelIns

-- | Returns all scripts involved in this 'TxSkel'
-- TODO: handle the case when the certificate does not need the script
txSkelAllScripts :: (MonadBlockChainBalancing m) => TxSkel -> m [VScript]
txSkelAllScripts txSkel = do
  txSkelSpendingScripts <- fmap Script.toVersioned <$> txSkelInputScripts txSkel
  return
    ( txSkelMintingScripts txSkel
        <> txSkelWithdrawingScripts txSkel
        <> txSkelProposingScripts txSkel
        <> txSkelCertifyingScripts txSkel
        <> txSkelSpendingScripts
    )

-- | Go through all of the 'Api.TxOutRef's in the list and look them up in the
-- state of the blockchain, throwing an error if one of them cannot be resolved.
lookupUtxos :: (MonadBlockChainBalancing m) => [Api.TxOutRef] -> m (Map Api.TxOutRef TxSkelOut)
lookupUtxos = foldM (\m oRef -> flip (Map.insert oRef) m <$> txSkelOutByRef oRef) Map.empty

-- | look up the UTxOs the transaction consumes, and sum their values.
txSkelInputValue :: (MonadBlockChainBalancing m) => TxSkel -> m Api.Value
txSkelInputValue = fmap mconcat . mapM (viewByRef txSkelOutValueL) . Map.keys . txSkelIns

-- * Slot and Time Management

-- $slotandtime
-- #slotandtime#
--
-- Slots are integers that monotonically increase and model the passage of
-- time. By looking at the current slot, a validator gets to know that it is
-- being executed within a certain window of wall-clock time. Things can get
-- annoying pretty fast when trying to mock traces and trying to exercise
-- certain branches of certain validators; make sure you also read the docs on
-- 'autoSlotIncrease' to be able to simulate sending transactions in parallel.

-- | Returns the current slot number
currentSlot :: (MonadBlockChainWithoutValidation m) => m Ledger.Slot
currentSlot = waitNSlots @_ @Int 0

-- | Wait for a certain slot, or throws an error if the slot is already past
awaitSlot :: (MonadBlockChainWithoutValidation m, Integral i) => i -> m Ledger.Slot
awaitSlot slot = currentSlot >>= waitNSlots . (slot -) . fromIntegral

-- | Returns the closed ms interval corresponding to the current slot
currentMSRange :: (MonadBlockChainWithoutValidation m) => m (Api.POSIXTime, Api.POSIXTime)
currentMSRange = slotToMSRange =<< currentSlot

-- | Returns the closed ms interval corresponding to the slot with the given
-- number. It holds that
--
-- > slotToMSRange (getEnclosingSlot t) == (a, b)    ==>   a <= t <= b
--
-- and
--
-- > slotToMSRange n == (a, b)   ==>   getEnclosingSlot a == n && getEnclosingSlot b == n
--
-- and
--
-- > slotToMSRange n == (a, b)   ==>   getEnclosingSlot (a-1) == n-1 && getEnclosingSlot (b+1) == n+1
slotToMSRange :: (MonadBlockChainWithoutValidation m, Integral i) => i -> m (Api.POSIXTime, Api.POSIXTime)
slotToMSRange (fromIntegral -> slot) = do
  slotConfig <- Emulator.pSlotConfig <$> getParams
  case Emulator.slotToPOSIXTimeRange slotConfig slot of
    Api.Interval
      (Api.LowerBound (Api.Finite l) leftclosed)
      (Api.UpperBound (Api.Finite r) rightclosed) ->
        return
          ( if leftclosed then l else l + 1,
            if rightclosed then r else r - 1
          )
    _ -> fail "Unexpected unbounded slot: please report a bug at https://github.com/tweag/cooked-validators/issues"

-- | Return the slot that contains the given time. See 'slotToMSRange' for
-- some satisfied equational properties.
getEnclosingSlot :: (MonadBlockChainWithoutValidation m) => Api.POSIXTime -> m Ledger.Slot
getEnclosingSlot t = (`Emulator.posixTimeToEnclosingSlot` t) . Emulator.pSlotConfig <$> getParams

-- | Waits until the current slot becomes greater or equal to the slot
--  containing the given POSIX time.  Note that that it might not wait for
--  anything if the current slot is large enough.
awaitEnclosingSlot :: (MonadBlockChainWithoutValidation m) => Api.POSIXTime -> m Ledger.Slot
awaitEnclosingSlot = awaitSlot <=< getEnclosingSlot

-- | Wait a given number of ms from the lower bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotLowerBound :: (MonadBlockChainWithoutValidation m, Integral i) => i -> m Ledger.Slot
waitNMSFromSlotLowerBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . fst

-- | Wait a given number of ms from the upper bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotUpperBound :: (MonadBlockChainWithoutValidation m, Integral i) => i -> m Ledger.Slot
waitNMSFromSlotUpperBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . snd

-- | The infinite range of slots ending before or at the given time
slotRangeBefore :: (MonadBlockChainWithoutValidation m) => Api.POSIXTime -> m Ledger.SlotRange
slotRangeBefore t = do
  n <- getEnclosingSlot t
  (_, b) <- slotToMSRange n
  -- If the given time @t@ happens to be the last ms of its slot, we can include
  -- the whole slot. Otherwise, the only way to be sure that the returned slot
  -- range contains no time after @t@ is to go to the preceding slot.
  return $ Api.to $ if t == b then n else n - 1

-- | The infinite range of slots starting after or at the given time
slotRangeAfter :: (MonadBlockChainWithoutValidation m) => Api.POSIXTime -> m Ledger.SlotRange
slotRangeAfter t = do
  n <- getEnclosingSlot t
  (a, _) <- slotToMSRange n
  return $ Api.from $ if t == a then n else n + 1

-- * Deriving further 'MonadBlockChain' instances

-- | A newtype wrapper to be used with '-XDerivingVia' to derive instances of
-- 'MonadBlockChain' for any 'MonadTransControl'.
--
-- For example, to derive 'MonadBlockChain m => MonadBlockChain (ReaderT r m)',
-- you'd write
--
-- > deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)
--
-- and avoid the trouble of defining all the class methods yourself.
newtype AsTrans t (m :: Type -> Type) a = AsTrans {getTrans :: t m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadTransControl)

instance (MonadTrans t, MonadFail m, Monad (t m)) => MonadFail (AsTrans t m) where
  fail = lift . fail

instance (MonadTransControl t, MonadError MockChainError m, Monad (t m)) => MonadError MockChainError (AsTrans t m) where
  throwError = lift . throwError
  catchError act f = liftWith (\run -> catchError (run act) (run . f)) >>= restoreT . return

instance (MonadTrans t, MonadBlockChainBalancing m, Monad (t m), MonadError MockChainError (AsTrans t m)) => MonadBlockChainBalancing (AsTrans t m) where
  getParams = lift getParams
  utxosAt = lift . utxosAt
  txSkelOutByRef = lift . txSkelOutByRef
  logEvent = lift . logEvent

instance (MonadTrans t, MonadBlockChainWithoutValidation m, Monad (t m), MonadError MockChainError (AsTrans t m)) => MonadBlockChainWithoutValidation (AsTrans t m) where
  allUtxos = lift allUtxos
  setParams = lift . setParams
  waitNSlots = lift . waitNSlots
  define name = lift . define name
  setConstitutionScript = lift . setConstitutionScript
  getConstitutionScript = lift getConstitutionScript
  registerStakingCred cred reward deposit = lift $ registerStakingCred cred reward deposit

instance (MonadTrans t, MonadBlockChain m, MonadBlockChainWithoutValidation (AsTrans t m)) => MonadBlockChain (AsTrans t m) where
  validateTxSkel = lift . validateTxSkel
  forceOutputs = lift . forceOutputs

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChainBalancing m) => MonadBlockChainBalancing (WriterT w m)

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChainWithoutValidation m) => MonadBlockChainWithoutValidation (WriterT w m)

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChain m) => MonadBlockChain (WriterT w m)

deriving via (AsTrans (ReaderT r) m) instance (MonadBlockChainBalancing m) => MonadBlockChainBalancing (ReaderT r m)

deriving via (AsTrans (ReaderT r) m) instance (MonadBlockChainWithoutValidation m) => MonadBlockChainWithoutValidation (ReaderT r m)

deriving via (AsTrans (ReaderT r) m) instance (MonadBlockChain m) => MonadBlockChain (ReaderT r m)

deriving via (AsTrans (StateT s) m) instance (MonadBlockChainBalancing m) => MonadBlockChainBalancing (StateT s m)

deriving via (AsTrans (StateT s) m) instance (MonadBlockChainWithoutValidation m) => MonadBlockChainWithoutValidation (StateT s m)

deriving via (AsTrans (StateT s) m) instance (MonadBlockChain m) => MonadBlockChain (StateT s m)

-- 'ListT' has no 'MonadTransControl' instance, so the @deriving via ...@
-- machinery is unusable here. However, there is
--
-- > MonadError e m => MonadError e (ListT m)
--
-- so I decided to go with a bit of code duplication to implement the
-- 'MonadBlockChainWithoutValidation' and 'MonadBlockChain' instances for
-- 'ListT', instead of more black magic...

instance (MonadBlockChainBalancing m) => MonadBlockChainBalancing (ListT m) where
  getParams = lift getParams
  utxosAt = lift . utxosAt
  txSkelOutByRef = lift . txSkelOutByRef
  logEvent = lift . logEvent

instance (MonadBlockChainWithoutValidation m) => MonadBlockChainWithoutValidation (ListT m) where
  allUtxos = lift allUtxos
  setParams = lift . setParams
  waitNSlots = lift . waitNSlots
  define name = lift . define name
  setConstitutionScript = lift . setConstitutionScript
  getConstitutionScript = lift getConstitutionScript
  registerStakingCred cred reward deposit = lift $ registerStakingCred cred reward deposit

instance (MonadBlockChain m) => MonadBlockChain (ListT m) where
  validateTxSkel = lift . validateTxSkel
  forceOutputs = lift . forceOutputs
