-- | This module provides a direct (not staged) implementation of the
-- `MonadBlockChain` specification. This rely on the emulator from
-- cardano-node-emulator for transaction validation, although we have our own
-- internal state. This choice might be revised in the future.
module Cooked.MockChain.Direct where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.InitialDistribution
import Cooked.MockChain.Balancing
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.MinAda
import Cooked.MockChain.MockChainSt
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Skeleton
import Data.Bifunctor (bimap)
import Data.Default
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ledger.Index qualified as Ledger
import Ledger.Orphans ()
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api

-- * Direct Emulation

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a
-- simple way to call validator scripts directly, without the need for all the
-- complexity the 'Contract' monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', a simplified view on
-- 'Api.UtxoIndex', which we also keep in our state.

newtype MockChainT m a = MockChainT
  {unMockChain :: StateT MockChainSt (ExceptT MockChainError m) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError)

type MockChain = MockChainT Identity

-- | Custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return = pure
  MockChainT x >>= f = MockChainT $ x >>= unMockChain . f

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

instance MonadTrans MockChainT where
  lift = MockChainT . lift . lift

instance (Monad m, Alternative m) => Alternative (MockChainT m) where
  empty = MockChainT $ StateT $ const $ ExceptT empty
  (<|>) = combineMockChainT (<|>)

combineMockChainT ::
  (Monad m) =>
  (forall a. m a -> m a -> m a) ->
  MockChainT m x ->
  MockChainT m x ->
  MockChainT m x
combineMockChainT f ma mb = MockChainT $
  StateT $ \s ->
    let resA = runExceptT $ runStateT (unMockChain ma) s
        resB = runExceptT $ runStateT (unMockChain mb) s
     in ExceptT $ f resA resB

mapMockChainT ::
  (m (Either MockChainError (a, MockChainSt)) -> n (Either MockChainError (b, MockChainSt))) ->
  MockChainT m a ->
  MockChainT n b
mapMockChainT f = MockChainT . mapStateT (mapExceptT f) . unMockChain

-- | Executes a 'MockChainT' from some initial state; does /not/ convert the
-- 'MockChainSt' into a 'UtxoState'.
runMockChainTRaw ::
  (Monad m) =>
  MockChainSt ->
  MockChainT m a ->
  m (Either MockChainError (a, MockChainSt))
runMockChainTRaw i0 = runExceptT . flip runStateT i0 . unMockChain

-- | Executes a 'MockChainT' from an initial state set up with the given initial
-- value distribution. Similar to 'runMockChainT', uses the default
-- environment. Returns a 'UtxoState' instead of a 'MockChainSt'. If you need
-- the later, use 'runMockChainTRaw'
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom i0 = fmap (fmap $ second mcstToUtxoState) . runMockChainTRaw (mockChainSt0From i0)

-- | Executes a 'MockChainT' from the canonical initial state and environment.
-- The canonical environment uses the default 'SlotConfig' and
-- @Cooked.Wallet.wallet 1@ as the sole wallet signing transactions.
runMockChainT :: (Monad m) => MockChainT m a -> m (Either MockChainError (a, UtxoState))
runMockChainT = runMockChainTFrom def

-- | See 'runMockChainTRaw'
runMockChainRaw :: MockChain a -> Either MockChainError (a, MockChainSt)
runMockChainRaw = runIdentity . runMockChainTRaw def

-- | See 'runMockChainTFrom'
runMockChainFrom :: InitialDistribution -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainFrom i0 = runIdentity . runMockChainTFrom i0

-- | See 'runMockChainT'
runMockChain :: MockChain a -> Either MockChainError (a, UtxoState)
runMockChain = runIdentity . runMockChainT

-- * Direct Interpretation of Operations

getIndex :: Ledger.UtxoIndex -> Map Api.TxOutRef Ledger.TxOut
getIndex =
  Map.fromList
    . map (bimap Ledger.fromCardanoTxIn (Ledger.TxOut . toCtxTxTxOut))
    . Map.toList
    . Cardano.unUTxO
  where
    -- We need to convert a UTxO context TxOut to a Transaction context Tx out.
    -- It's complicated because the datum type is indexed by the context.
    toCtxTxTxOut :: Cardano.TxOut Cardano.CtxUTxO era -> Cardano.TxOut Cardano.CtxTx era
    toCtxTxTxOut (Cardano.TxOut addr val d refS) =
      let dat = case d of
            Cardano.TxOutDatumNone -> Cardano.TxOutDatumNone
            Cardano.TxOutDatumHash s h -> Cardano.TxOutDatumHash s h
            Cardano.TxOutDatumInline s sd -> Cardano.TxOutDatumInline s sd
       in Cardano.TxOut addr val dat refS

instance (Monad m) => MonadBlockChainBalancing (MockChainT m) where
  getParams = gets mcstParams
  validatorFromHash valHash = gets $ Map.lookup valHash . mcstValidators
  txOutByRefLedger outref = gets $ Map.lookup outref . getIndex . mcstIndex
  datumFromHash datumHash = (txSkelOutUntypedDatum <=< Just . fst <=< Map.lookup datumHash) <$> gets mcstDatums
  utxosAtLedger addr = filter ((addr ==) . outputAddress . txOutV2FromLedger . snd) <$> allUtxosLedger

instance (Monad m) => MonadBlockChainWithoutValidation (MockChainT m) where
  allUtxosLedger = gets $ Map.toList . getIndex . mcstIndex
  setParams newParams = modify (\st -> st {mcstParams = newParams})
  currentSlot = gets mcstCurrentSlot
  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

instance (Monad m) => MonadBlockChain (MockChainT m) where
  validateTxSkel skelUnbal = do
    -- We retrieve the current parameters
    oldParams <- getParams
    -- We compute the optionally modified parameters
    let newParams = applyEmulatorParamsModification (txOptEmulatorParamsModification . txSkelOpts $ skelUnbal) oldParams
    -- We change the parameters for the duration of the validation process
    setParams newParams
    -- We ensure that the outputs have the required minimal amount of ada, when
    -- requested in the skeleton options
    minAdaSkelUnbal <- if txOptEnsureMinAda . txSkelOpts $ skelUnbal then toTxSkelWithMinAda skelUnbal else return skelUnbal
    -- We balance the skeleton when requested in the skeleton option, and get
    -- the associated fee, collateral inputs and return collateral wallet
    (skel, fee, collateralIns, returnCollateralWallet) <- balanceTxSkel minAdaSkelUnbal
    -- We retrieve data that will be used in the transaction generation process:
    -- datums, validators and various kinds of inputs. This idea is to provide a
    -- rich-enough context for the transaction generation to succeed.
    insData <- txSkelInputData skel
    insValidators <- txSkelInputValidators skel
    insMap <- txSkelInputUtxosPl skel
    refInsMap <- txSkelReferenceInputUtxosPl skel
    collateralInsMap <- lookupUtxosPl $ Set.toList collateralIns
    -- We attempt to generate the transaction associated with the balanced
    -- skeleton and the retrieved data. This is an internal generation, there is
    -- no validation involved yet.
    cardanoTx <- case generateTx fee returnCollateralWallet collateralIns newParams insData (insMap <> refInsMap <> collateralInsMap) insValidators skel of
      Left err -> throwError . MCEGenerationError $ err
      -- We apply post-generation modification when applicable
      Right tx -> return $ Ledger.CardanoEmulatorEraTx $ applyRawModOnBalancedTx (txOptUnsafeModTx . txSkelOpts $ skelUnbal) tx
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
    -- Now that we have compute a new index, we can update it
    modify' (\st -> st {mcstIndex = newUtxoIndex})
    case valError of
      -- When validation failed for any reason, we throw an error. TODO: This
      -- behavior could be subject to change in the future.
      Just err -> throwError (uncurry MCEValidationError err)
      -- Otherwise, we update known validators and datums.
      Nothing ->
        modify'
          ( removeDatums (Map.keys insData)
              . addDatums (Map.toList $ txSkelDataInOutputs skel)
              . addValidators (txSkelValidatorsInOutputs skel <> txSkelReferenceScripts skel)
          )
    -- We apply a change of slot when requested in the options
    when (txOptAutoSlotIncrease $ txSkelOpts skel) $
      modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    -- We return the parameters to their original state
    setParams oldParams
    -- We return the validated transaction
    return cardanoTx
