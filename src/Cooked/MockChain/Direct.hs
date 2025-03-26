-- | This module provides a direct (not staged) implementation of the
-- `MonadBlockChain` specification. This rely on the emulator from
-- cardano-node-emulator for transaction validation, although we have our own
-- internal state. This choice might be revised in the future.
module Cooked.MockChain.Direct where

import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Cooked.InitialDistribution
import Cooked.MockChain.AutoReferenceScripts
import Cooked.MockChain.Balancing
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.MinAda
import Cooked.MockChain.MockChainSt
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Skeleton
import Data.Default
import Data.Map.Strict qualified as Map
import Ledger.Index qualified as Ledger
import Ledger.Orphans ()
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger

-- * Direct Emulation

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a
-- simple way to call validator scripts directly, without the need for all the
-- complexity the 'Contract' monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', a simplified view on
-- 'Ledger.UtxoIndex', which we also keep in our state.

newtype MockChainT m a = MockChainT
  {unMockChain :: (StateT MockChainSt (ExceptT MockChainError (WriterT [MockChainLogEntry] m))) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError, MonadWriter [MockChainLogEntry])

type MockChain = MockChainT Identity

-- | Custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return = pure
  MockChainT x >>= f = MockChainT $ x >>= unMockChain . f

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

instance MonadTrans MockChainT where
  lift = MockChainT . lift . lift . lift

instance (Monad m, Alternative m) => Alternative (MockChainT m) where
  empty = MockChainT $ StateT $ const $ ExceptT $ WriterT empty
  (<|>) = combineMockChainT (<|>)

combineMockChainT ::
  (forall a. m a -> m a -> m a) ->
  MockChainT m x ->
  MockChainT m x ->
  MockChainT m x
combineMockChainT f ma mb = MockChainT $
  StateT $ \s ->
    let resA = runWriterT $ runExceptT $ runStateT (unMockChain ma) s
        resB = runWriterT $ runExceptT $ runStateT (unMockChain mb) s
     in ExceptT $ WriterT $ f resA resB

type MockChainReturn a b = (Either MockChainError (a, b), [MockChainLogEntry])

mapMockChainT ::
  (m (MockChainReturn a MockChainSt) -> n (MockChainReturn b MockChainSt)) ->
  MockChainT m a ->
  MockChainT n b
mapMockChainT f = MockChainT . mapStateT (mapExceptT (mapWriterT f)) . unMockChain

runMockChainTRaw ::
  MockChainT m a ->
  m (MockChainReturn a MockChainSt)
runMockChainTRaw = runWriterT . runExceptT . flip runStateT def . unMockChain

-- | Executes a 'MockChainT' from an initial state set up with the given initial
-- value distribution. Similar to 'runMockChainT', uses the default
-- environment. Returns a 'UtxoState' instead of a 'MockChainSt'. If you need
-- the later, use 'runMockChainTRaw'
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (MockChainReturn a UtxoState)
runMockChainTFrom i0 s =
  first (right (second mcstToUtxoState))
    <$> runMockChainTRaw (mockChainSt0From i0 >>= put >> s)

-- | Executes a 'MockChainT' from the canonical initial state and environment.
-- The canonical environment uses the default 'SlotConfig'
runMockChainT :: (Monad m) => MockChainT m a -> m (MockChainReturn a UtxoState)
runMockChainT = runMockChainTFrom def

-- | See 'runMockChainTFrom'
runMockChainFrom :: InitialDistribution -> MockChain a -> MockChainReturn a UtxoState
runMockChainFrom i0 = runIdentity . runMockChainTFrom i0

-- | See 'runMockChainT'
runMockChain :: MockChain a -> MockChainReturn a UtxoState
runMockChain = runIdentity . runMockChainT

-- * Direct Interpretation of Operations

instance (Monad m) => MonadBlockChainBalancing (MockChainT m) where
  getParams = gets mcstParams
  validatorFromHash valHash = gets $ Map.lookup valHash . mcstValidators
  txOutByRef outref = gets $ Map.lookup outref . getIndex . mcstIndex
  datumFromHash datumHash = (txSkelOutUntypedDatum <=< Just . fst <=< Map.lookup datumHash) <$> gets mcstDatums
  utxosAt addr = filter ((addr ==) . outputAddress . snd) <$> allUtxos
  logEvent l = tell [l]

instance (Monad m) => MonadBlockChainWithoutValidation (MockChainT m) where
  allUtxos = gets $ Map.toList . getIndex . mcstIndex
  setParams newParams = modify (\st -> st {mcstParams = newParams})
  currentSlot = gets mcstCurrentSlot
  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

instance (Monad m) => MonadBlockChain (MockChainT m) where
  validateTxSkel skelUnbal@TxSkel {..} | TxOpts {..} <- txSkelOpts = do
    -- We log the submitted skeleton
    gets mcstToSkelContext >>= logEvent . (`MCLogSubmittedTxSkel` skelUnbal)
    -- We retrieve the current parameters
    oldParams <- getParams
    -- We compute the optionally modified parameters
    let newParams = applyEmulatorParamsModification txOptEmulatorParamsModification oldParams
    -- We change the parameters for the duration of the validation process
    setParams newParams
    -- We ensure that the outputs have the required minimal amount of ada, when
    -- requested in the skeleton options
    minAdaSkelUnbal <- toTxSkelWithMinAda skelUnbal
    -- We add reference scripts in the various redeemers of the skeleton, when
    -- they can be found in the index and are requested in the skeleton options
    minAdaRefScriptsSkelUnbal <- (if txOptAutoReferenceScripts then toTxSkelWithReferenceScripts else return) minAdaSkelUnbal
    -- We balance the skeleton when requested in the skeleton option, and get
    -- the associated fee, collateral inputs and return collateral wallet
    (skel, fee, mCollaterals) <- balanceTxSkel minAdaRefScriptsSkelUnbal
    -- We log the adjusted skeleton
    gets mcstToSkelContext >>= \ctx -> logEvent $ MCLogAdjustedTxSkel ctx skel fee mCollaterals
    -- We generate the transaction associated with the skeleton, and apply on it
    -- the modifications from the skeleton options
    cardanoTx <- Ledger.CardanoEmulatorEraTx . applyRawModOnBalancedTx txOptUnsafeModTx <$> txSkelToCardanoTx skel fee mCollaterals
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets mcstToEmulatedLedgerState
    -- We finally run the emulated validation, and we only care about the
    -- validation result, as we update our own internal state
    let (_, mValidationResult) = Emulator.validateCardanoTx newParams eLedgerState cardanoTx
    -- We retrieve our current utxo index to perform modifications associated
    -- with the validated transaction.
    utxoIndex <- gets mcstIndex
    -- We create a new utxo index with an error when validation failed
    let (newUtxoIndex, valError) = case mValidationResult of
          -- In case of a phase 1 error, we give back the same index
          Ledger.FailPhase1 _ err -> (utxoIndex, Just (Ledger.Phase1, err))
          -- In case of a phase 2 error, we retrieve the collaterals (and yes,
          -- despite its name, 'insertCollateral' actually takes the collaterals
          -- away from the index)
          Ledger.FailPhase2 _ err _ -> (Ledger.insertCollateral cardanoTx utxoIndex, Just (Ledger.Phase2, err))
          -- In case of success, we update the index with all inputs and outputs
          -- contained in the transaction
          Ledger.Success {} -> (Ledger.insert cardanoTx utxoIndex, Nothing)
    case valError of
      -- When validation failed for any reason, we throw an error. TODO: This
      -- behavior could be subject to change in the future.
      Just err -> throwError (uncurry MCEValidationError err)
      -- Otherwise, we update known validators and datums.
      Nothing -> do
        insData <- txSkelInputDataAsHashes skel
        modify'
          ( removeDatums insData
              . addDatums (txSkelDataInOutputs skel)
              . addValidators (txSkelValidatorsInOutputs skel <> txSkelReferenceScripts skel)
          )
    -- Now that we have computed a new index, we can update it
    modify' (\st -> st {mcstIndex = newUtxoIndex})
    -- We apply a change of slot when requested in the options
    when txOptAutoSlotIncrease $ modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    -- We return the parameters to their original state
    setParams oldParams
    -- We log the validated transaction
    logEvent $ MCLogNewTx (Ledger.fromCardanoTxId $ Ledger.getCardanoTxId cardanoTx)
    -- We return the validated transaction
    return cardanoTx
