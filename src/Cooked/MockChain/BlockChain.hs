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
    currentTime,
    waitNSlots,
    utxosFromCardanoTx,
    typedDatumFromTxOutRef,
    valueFromTxOutRef,
    outputDatumFromTxOutRef,
    datumFromTxOutRef,
    resolveDatum,
    resolveTypedDatum,
    resolveValidator,
    resolveReferenceScript,
    getEnclosingSlot,
    awaitEnclosingSlot,
    awaitDurationFromLowerBound,
    awaitDurationFromUpperBound,
    slotRangeBefore,
    slotRangeAfter,
    slotToTimeInterval,
    txSkelInputUtxos,
    txSkelReferenceInputUtxos,
    txSkelInputValidators,
    txSkelInputValue,
    txSkelHashedData,
    txSkelInputDataAsHashes,
    lookupUtxos,
    validateTxSkel',
    validateTxSkel_,
    txSkelProposalsDeposit,
    govActionDeposit,
    txOutRefToTxSkelOut,
  )
where

import Cardano.Api.Ledger qualified as Cardano
import Cardano.Ledger.Conway.PParams qualified as Conway
import Cardano.Node.Emulator qualified as Emulator
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Cooked.Conversion.ToCredential
import Cooked.Conversion.ToOutputDatum
import Cooked.Conversion.ToScriptHash
import Cooked.MockChain.GenerateTx
import Cooked.Output
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
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- * MockChain errors

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError
  = -- | Validation errors, either in Phase 1 or Phase 2
    MCEValidationError Ledger.ValidationPhase Ledger.ValidationError
  | -- | Thrown when the balancing wallet does not have enough funds
    MCEUnbalanceable Wallet Api.Value TxSkel
  | -- | Thrown when not enough collateral are provided. Built upon the fee, the
    -- percentage and the expected minimal collateral value.
    MCENoSuitableCollateral Integer Integer Api.Value
  | -- | Thrown when an error occured during transaction generation
    MCEGenerationError GenerateTxError
  | -- | Thrown when an output reference is missing from the mockchain state
    MCEUnknownOutRefError String Api.TxOutRef
  | -- | Same as 'MCEUnknownOutRefError' for validators.
    MCEUnknownValidator String Script.ValidatorHash
  | -- | Same as 'MCEUnknownOutRefError' for datums.
    MCEUnknownDatum String Api.DatumHash
  | -- | Used to provide 'MonadFail' instances.
    FailWith String
  deriving (Show, Eq)

-- * MockChain logs

-- | This represents the specific events that should be logged when processing
-- transactions. If a new kind of event arises, then a new constructor should be
-- provided here.
data MockChainLogEntry
  = -- | Logging a Skeleton as it is submitted by the user.
    MCLogSubmittedTxSkel SkelContext TxSkel
  | -- | Logging a Skeleton as it has been adjusted by the balancing mechanism,
    -- alongside fee, and possible collateral utxos and return collateral wallet.
    MCLogAdjustedTxSkel SkelContext TxSkel Integer (Maybe (Set Api.TxOutRef, Wallet))
  | -- | Logging the appearance of a new transaction, after a skeleton has been
    -- successfully sent for validation.
    MCLogNewTx Api.TxId
  | -- | Logging the fact that utxos provided by the user for balancing have to be
    -- discarded for a specific reason.
    MCLogDiscardedUtxos Integer String
  | -- | Logging the fact that utxos provided as collaterals will not be used
    -- because the transaction does not involve scripts. There are 2 cases,
    -- depending on whether the user has provided an explicit wallet or a set of
    -- utxos to be used as collaterals.
    MCLogUnusedCollaterals (Either Wallet (Set Api.TxOutRef))
  | -- | Logging the automatic addition of a reference script
    MCLogAddedReferenceScript Redeemer Api.TxOutRef Script.ScriptHash

-- | Contains methods needed for balancing.
class (MonadFail m, MonadError MockChainError m) => MonadBlockChainBalancing m where
  -- | Returns the emulator parameters, including protocol parameters
  getParams :: m Emulator.Params

  -- | Returns a list of all UTxOs at a certain address.
  utxosAt :: Api.Address -> m [(Api.TxOutRef, Api.TxOut)]

  -- | Returns the datum with the given hash if present.
  datumFromHash :: Api.DatumHash -> m (Maybe Api.Datum)

  -- | Returns the full validator corresponding to hash, if that validator owns
  -- something or if it is stored in the reference script field of some UTxO.
  validatorFromHash :: Script.ValidatorHash -> m (Maybe (Script.Versioned Script.Validator))

  -- | Returns an output given a reference to it
  txOutByRef :: Api.TxOutRef -> m (Maybe Api.TxOut)

  -- | Logs an event that occured during a BlockChain run
  logEvent :: MockChainLogEntry -> m ()

class (MonadBlockChainBalancing m) => MonadBlockChainWithoutValidation m where
  -- | Returns a list of all currently known outputs.
  allUtxos :: m [(Api.TxOutRef, Api.TxOut)]

  -- | Updates parameters
  setParams :: Emulator.Params -> m ()

  -- | Returns the current slot number
  currentSlot :: m Ledger.Slot

  -- | Waits until the current slot becomes greater or equal to the given slot,
  -- and returns the current slot after waiting.
  --
  -- Note that it might not wait for anything if the current slot is large
  -- enough.
  awaitSlot :: Ledger.Slot -> m Ledger.Slot

-- | The main abstraction of the blockchain.
class (MonadBlockChainWithoutValidation m) => MonadBlockChain m where
  -- | Generates, balances and validates a transaction from a skeleton. It
  -- returns the validated transaction and updates the state of the
  -- blockchain. In 'MockChainT', this means:
  --
  -- - deletes the consumed outputs from 'mcstIndex'
  -- - adds the produced outputs to 'msctIndex'
  -- - deletes the consumed datums from 'mcstDatums'
  -- - adds the produced datums to 'mcstDatums'
  -- - adds the validators on outputs to the 'mcstValidators'.
  validateTxSkel :: TxSkel -> m Ledger.CardanoTx

-- | Validates a skeleton, and retuns the ordered list of produced output
-- references
validateTxSkel' :: (MonadBlockChain m) => TxSkel -> m [Api.TxOutRef]
validateTxSkel' = (map fst . utxosFromCardanoTx <$>) . validateTxSkel

-- | Validates a skeleton, and erases the outputs
validateTxSkel_ :: (MonadBlockChain m) => TxSkel -> m ()
validateTxSkel_ = void . validateTxSkel

-- | Retrieve the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
utxosFromCardanoTx :: Ledger.CardanoTx -> [(Api.TxOutRef, Api.TxOut)]
utxosFromCardanoTx =
  map
    ( \(txOut, txOutRef) ->
        ( Ledger.fromCardanoTxIn txOutRef,
          Ledger.fromCardanoTxOutToPV2TxInfoTxOut $ Ledger.getTxOut txOut
        )
    )
    . Ledger.getCardanoTxOutRefs

-- | Try to resolve the datum on the output: If there's an inline datum, take
-- that; if there's a datum hash, look the corresponding datum up (with
-- 'datumFromHash'), returning @Nothing@ if it can't be found; if there's no
-- datum or hash at all, return @Nothing@.
resolveDatum ::
  ( IsAbstractOutput out,
    ToOutputDatum (DatumType out),
    MonadBlockChainBalancing m
  ) =>
  out ->
  m (Maybe (ConcreteOutput (OwnerType out) Api.Datum (ValueType out) (ReferenceScriptType out)))
resolveDatum out = do
  mDatum <- case outputOutputDatum out of
    Api.OutputDatumHash datumHash -> datumFromHash datumHash
    Api.OutputDatum datum -> return $ Just datum
    Api.NoOutputDatum -> return Nothing
  return $ do
    mDat <- mDatum
    return $ (fromAbstractOutput out) {concreteOutputDatum = mDat}

-- | Like 'resolveDatum', but also tries to use 'fromBuiltinData' to extract a
-- datum of the suitable type.
resolveTypedDatum ::
  ( IsAbstractOutput out,
    ToOutputDatum (DatumType out),
    MonadBlockChainBalancing m,
    Api.FromData a
  ) =>
  out ->
  m (Maybe (ConcreteOutput (OwnerType out) a (ValueType out) (ReferenceScriptType out)))
resolveTypedDatum out = do
  mOut <- resolveDatum out
  return $ do
    out' <- mOut
    let Api.Datum datum = out' ^. outputDatumL
    dat <- Api.fromBuiltinData datum
    return $ (fromAbstractOutput out) {concreteOutputDatum = dat}

-- | Try to resolve the validator that owns an output: If the output is owned by
-- a public key, or if the validator's hash is not known (i.e. if
-- 'validatorFromHash' returns @Nothing@) return @Nothing@.
resolveValidator ::
  ( IsAbstractOutput out,
    ToCredential (OwnerType out),
    MonadBlockChainBalancing m
  ) =>
  out ->
  m (Maybe (ConcreteOutput (Script.Versioned Script.Validator) (DatumType out) (ValueType out) (ReferenceScriptType out)))
resolveValidator out =
  case toCredential (out ^. outputOwnerL) of
    Api.PubKeyCredential _ -> return Nothing
    Api.ScriptCredential (Api.ScriptHash hash) -> do
      mVal <- validatorFromHash (Script.ValidatorHash hash)
      return $ do
        val <- mVal
        return $ (fromAbstractOutput out) {concreteOutputOwner = val}

-- | Try to resolve the reference script on an output: If the output has no
-- reference script, or if the reference script's hash is not known (i.e. if
-- 'validatorFromHash' returns @Nothing@), this function will return @Nothing@.
resolveReferenceScript ::
  ( IsAbstractOutput out,
    ToScriptHash (ReferenceScriptType out),
    MonadBlockChainBalancing m
  ) =>
  out ->
  m (Maybe (ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) (Script.Versioned Script.Validator)))
resolveReferenceScript out | Just (Api.ScriptHash hash) <- outputReferenceScriptHash out = do
  mVal <- validatorFromHash (Script.ValidatorHash hash)
  return $ do
    val <- mVal
    return $ (fromAbstractOutput out) {concreteOutputReferenceScript = Just val}
resolveReferenceScript _ = return Nothing

outputDatumFromTxOutRef :: (MonadBlockChainWithoutValidation m) => Api.TxOutRef -> m (Maybe Api.OutputDatum)
outputDatumFromTxOutRef = ((outputOutputDatum <$>) <$>) . txOutByRef

datumFromTxOutRef :: (MonadBlockChainWithoutValidation m) => Api.TxOutRef -> m (Maybe Api.Datum)
datumFromTxOutRef oref = do
  mOutputDatum <- outputDatumFromTxOutRef oref
  case mOutputDatum of
    Nothing -> return Nothing
    Just Api.NoOutputDatum -> return Nothing
    Just (Api.OutputDatum datum) -> return $ Just datum
    Just (Api.OutputDatumHash datumHash) -> datumFromHash datumHash

typedDatumFromTxOutRef :: (Api.FromData a, MonadBlockChainWithoutValidation m) => Api.TxOutRef -> m (Maybe a)
typedDatumFromTxOutRef = ((>>= (\(Api.Datum datum) -> Api.fromBuiltinData datum)) <$>) . datumFromTxOutRef

valueFromTxOutRef :: (MonadBlockChainWithoutValidation m) => Api.TxOutRef -> m (Maybe Api.Value)
valueFromTxOutRef = ((outputValue <$>) <$>) . txOutByRef

txSkelInputUtxos :: (MonadBlockChainBalancing m) => TxSkel -> m (Map Api.TxOutRef Api.TxOut)
txSkelInputUtxos = lookupUtxos . Map.keys . txSkelIns

txSkelReferenceInputUtxos :: (MonadBlockChainBalancing m) => TxSkel -> m (Map Api.TxOutRef Api.TxOut)
txSkelReferenceInputUtxos = lookupUtxos . txSkelReferenceTxOutRefs

-- | Retrieves the required deposit amount for issuing governance actions.
govActionDeposit :: (MonadBlockChainBalancing m) => m Api.Lovelace
govActionDeposit = Api.Lovelace . Cardano.unCoin . Lens.view Conway.ppGovActionDepositL . Emulator.emulatorPParams <$> getParams

-- | Retrieves the total amount of lovelace deposited in proposals in this
-- skeleton (equal to `govActionDeposit` times the number of proposals).
txSkelProposalsDeposit :: (MonadBlockChainBalancing m) => TxSkel -> m Api.Lovelace
txSkelProposalsDeposit TxSkel {..} = Api.Lovelace . (toInteger (length txSkelProposals) *) . Api.getLovelace <$> govActionDeposit

-- | Helper to convert Nothing to an error
maybeErrM :: (MonadBlockChainBalancing m) => MockChainError -> (a -> b) -> m (Maybe a) -> m b
maybeErrM err f = (maybe (throwError err) (return . f) =<<)

-- | All validators which protect transaction inputs
txSkelInputValidators :: (MonadBlockChainBalancing m) => TxSkel -> m (Map Script.ValidatorHash (Script.Versioned Script.Validator))
txSkelInputValidators skel = do
  utxos <- Map.toList <$> lookupUtxos (Map.keys . txSkelIns $ skel)
  Map.fromList . catMaybes
    <$> mapM
      ( \(_oref, out) -> case outputAddress out of
          Api.Address (Api.ScriptCredential (Api.ScriptHash hash)) _ -> do
            let valHash = Script.ValidatorHash hash
            maybeErrM
              ( MCEUnknownValidator
                  "txSkelInputValidators: unknown validator hash on transaction input"
                  valHash
              )
              (Just . (valHash,))
              (validatorFromHash valHash)
          _ -> return Nothing
      )
      utxos

-- | Go through all of the 'Api.TxOutRef's in the list and look them up in the
-- state of the blockchain, throwing an error if one of them cannot be resolved.
lookupUtxos :: (MonadBlockChainBalancing m) => [Api.TxOutRef] -> m (Map Api.TxOutRef Api.TxOut)
lookupUtxos =
  (Map.fromList <$>)
    . mapM (\oRef -> (oRef,) <$> maybeErrM (MCEUnknownOutRefError "lookupUtxos: unknown TxOutRef" oRef) id (txOutByRef oRef))

-- | look up the UTxOs the transaction consumes, and sum their values.
txSkelInputValue :: (MonadBlockChainBalancing m) => TxSkel -> m Api.Value
txSkelInputValue = (foldMap Api.txOutValue <$>) . txSkelInputUtxos

-- | Looks up and resolves the hashed datums on UTxOs the transaction consumes
-- or references, which will be needed by the transaction body.
txSkelHashedData :: (MonadBlockChainBalancing m) => TxSkel -> m (Map Api.DatumHash Api.Datum)
txSkelHashedData skel = do
  (Map.elems -> inputTxOuts) <- txSkelInputUtxos skel
  (Map.elems -> refInputTxOuts) <- txSkelReferenceInputUtxos skel
  foldM
    ( \dat dHash ->
        maybeErrM
          (MCEUnknownDatum "txSkelHashedData: Transaction input with unknown datum hash" dHash)
          (\rDat -> Map.insert dHash rDat dat)
          (datumFromHash dHash)
    )
    Map.empty
    (mapMaybe (fmap (^. outputDatumL) . isOutputWithDatumHash) $ inputTxOuts <> refInputTxOuts)

-- | Looks up the data on UTxOs the transaction consumes and returns their
-- hashes. This corresponds to the keys of what should be removed from the
-- stored datums in our mockchain.  There can be duplicates, which is expected.
txSkelInputDataAsHashes :: (MonadBlockChainBalancing m) => TxSkel -> m [Api.DatumHash]
txSkelInputDataAsHashes skel = do
  let outputToDatumHashM output = case output ^. outputDatumL of
        Api.OutputDatumHash dHash ->
          maybeErrM
            (MCEUnknownDatum "txSkelInputDataAsHashes: Transaction input with unknown datum hash" dHash)
            (Just . const dHash)
            (datumFromHash dHash)
        Api.OutputDatum datum -> return $ Just $ Script.datumHash datum
        Api.NoOutputDatum -> return Nothing
  (Map.elems -> inputTxOuts) <- txSkelInputUtxos skel
  catMaybes <$> mapM outputToDatumHashM inputTxOuts

-- | This creates a payment from an existing TxOut. This is not trivial because
-- we need to reconstruct a bunch of information using data withing the blockchain
txOutRefToTxSkelOut :: (MonadBlockChainBalancing m) => Api.TxOutRef -> m TxSkelOut
txOutRefToTxSkelOut oRef = do
  Just txOut@(Api.TxOut (Api.Address cred _) _ dat refS) <- txOutByRef oRef
  target <- case cred of
    Api.PubKeyCredential pkh -> return $ Left pkh
    Api.ScriptCredential (Api.ScriptHash sh) -> do
      Just val <- validatorFromHash (Script.ValidatorHash sh)
      return $ Right val
  datum <- case dat of
    Api.NoOutputDatum -> return TxSkelOutNoDatum
    Api.OutputDatumHash hash -> do
      Just (Api.Datum dat') <- datumFromHash hash
      return $ TxSkelOutDatum dat' -- TODO: investigate between this and TxSkelOutDatumHash
    Api.OutputDatum (Api.Datum dat') -> return $ TxSkelOutInlineDatum dat'
  refScript <- case refS of
    Nothing -> return Nothing
    Just (Api.ScriptHash sh) -> validatorFromHash (Script.ValidatorHash sh)
  return $
    Pays $
      (fromAbstractOutput txOut)
        { concreteOutputOwner = target,
          concreteOutputDatum = datum,
          concreteOutputReferenceScript = refScript
        }

-- ** Slot and Time Management

-- $slotandtime
-- #slotandtime#
--
-- Slots are integers that monotonically increase and model the passage of
-- time. By looking at the current slot, a validator gets to know that it is
-- being executed within a certain window of wall-clock time. Things can get
-- annoying pretty fast when trying to mock traces and trying to exercise
-- certain branches of certain validators; make sure you also read the docs on
-- 'autoSlotIncrease' to be able to simulate sending transactions in parallel.

-- | Moves n slots fowards
waitNSlots :: (MonadBlockChainWithoutValidation m) => Integer -> m Ledger.Slot
waitNSlots n =
  if n < 0
    then fail "waitNSlots: negative argument"
    else currentSlot >>= awaitSlot . (+ fromIntegral n)

-- | Returns the closed ms interval corresponding to the current slot
currentTime :: (MonadBlockChainWithoutValidation m) => m (Api.POSIXTime, Api.POSIXTime)
currentTime = slotToTimeInterval =<< currentSlot

-- | Returns the closed ms interval corresponding to the slot with the given
-- number. It holds that
--
-- > slotToTimeInterval (getEnclosingSlot t) == (a, b)    ==>   a <= t <= b
--
-- and
--
-- > slotToTimeInterval n == (a, b)   ==>   getEnclosingSlot a == n && getEnclosingSlot b == n
--
-- and
--
-- > slotToTimeInterval n == (a, b)   ==>   getEnclosingSlot (a-1) == n-1 && getEnclosingSlot (b+1) == n+1
slotToTimeInterval :: (MonadBlockChainWithoutValidation m) => Ledger.Slot -> m (Api.POSIXTime, Api.POSIXTime)
slotToTimeInterval slot = do
  slotConfig <- Emulator.pSlotConfig <$> getParams
  case Emulator.slotToPOSIXTimeRange slotConfig slot of
    Api.Interval
      (Api.LowerBound (Api.Finite l) leftclosed)
      (Api.UpperBound (Api.Finite r) rightclosed) ->
        return
          ( if leftclosed then l else l + 1,
            if rightclosed then r else r - 1
          )
    _ -> error "The time interval corresponding to a slot should be finite on both ends."

-- | Return the slot that contains the given time. See 'slotToTimeInterval' for
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
awaitDurationFromLowerBound :: (MonadBlockChainWithoutValidation m) => Integer -> m Ledger.Slot
awaitDurationFromLowerBound duration = currentTime >>= awaitEnclosingSlot . (+ fromIntegral duration) . fst

-- | Wait a given number of ms from the upper bound of the current slot and
-- returns the current slot after waiting.
awaitDurationFromUpperBound :: (MonadBlockChainWithoutValidation m) => Integer -> m Ledger.Slot
awaitDurationFromUpperBound duration = currentTime >>= awaitEnclosingSlot . (+ fromIntegral duration) . fst

-- | The infinite range of slots ending before or at the given time
slotRangeBefore :: (MonadBlockChainWithoutValidation m) => Api.POSIXTime -> m Ledger.SlotRange
slotRangeBefore t = do
  n <- getEnclosingSlot t
  (_, b) <- slotToTimeInterval n
  -- If the given time @t@ happens to be the last ms of its slot, we can include
  -- the whole slot. Otherwise, the only way to be sure that the returned slot
  -- range contains no time after @t@ is to go to the preceding slot.
  return $ Api.to $ if t == b then n else n - 1

-- | The infinite range of slots starting after or at the given time
slotRangeAfter :: (MonadBlockChainWithoutValidation m) => Api.POSIXTime -> m Ledger.SlotRange
slotRangeAfter t = do
  n <- getEnclosingSlot t
  (a, _) <- slotToTimeInterval n
  return $ Api.from $ if t == a then n else n + 1

-- ** Deriving further 'MonadBlockChain' instances

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
  validatorFromHash = lift . validatorFromHash
  utxosAt = lift . utxosAt
  txOutByRef = lift . txOutByRef
  datumFromHash = lift . datumFromHash
  logEvent = lift . logEvent

instance (MonadTrans t, MonadBlockChainWithoutValidation m, Monad (t m), MonadError MockChainError (AsTrans t m)) => MonadBlockChainWithoutValidation (AsTrans t m) where
  allUtxos = lift allUtxos
  setParams = lift . setParams
  currentSlot = lift currentSlot
  awaitSlot = lift . awaitSlot

instance (MonadTrans t, MonadBlockChain m, MonadBlockChainWithoutValidation (AsTrans t m)) => MonadBlockChain (AsTrans t m) where
  validateTxSkel = lift . validateTxSkel

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
  validatorFromHash = lift . validatorFromHash
  utxosAt = lift . utxosAt
  txOutByRef = lift . txOutByRef
  datumFromHash = lift . datumFromHash
  logEvent = lift . logEvent

instance (MonadBlockChainWithoutValidation m) => MonadBlockChainWithoutValidation (ListT m) where
  allUtxos = lift allUtxos
  setParams = lift . setParams
  currentSlot = lift currentSlot
  awaitSlot = lift . awaitSlot

instance (MonadBlockChain m) => MonadBlockChain (ListT m) where
  validateTxSkel = lift . validateTxSkel
