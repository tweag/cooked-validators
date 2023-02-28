{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cooked.MockChain.Direct where

import qualified Cardano.Api as C
import qualified Cardano.Ledger.Shelley.API as CardanoLedger
import qualified Cardano.Node.Emulator.Params as Emulator
import qualified Cardano.Node.Emulator.Validation as Emulator
import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.MockChain.Balancing
import Cooked.MockChain.BlockChain
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Ledger.Blockchain as Ledger
import qualified Ledger.Index as Ledger
import Ledger.Orphans ()
import qualified Ledger.Slot as Ledger
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Ledger
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.Script.Utils.V2.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as PV2
import qualified Plutus.V2.Ledger.Api as Pl

-- * Direct Emulation

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a
-- simple way to call validator scripts directly, without the need for all the
-- complexity the 'Contract' monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', a simplified view on
-- 'Pl.UtxoIndex', which we also keep in our state.

mcstToUtxoState :: MockChainSt -> UtxoState
mcstToUtxoState MockChainSt {mcstIndex, mcstDatums} =
  UtxoState
    . foldr (\(address, utxoValueSet) acc -> Map.insertWith (<>) address utxoValueSet acc) Map.empty
    . mapMaybe extractPayload
    . Map.toList
    . utxoIndexToTxOutMap
    $ mcstIndex
  where
    extractPayload :: (Pl.TxOutRef, PV2.TxOut) -> Maybe (Pl.Address, UtxoPayloadSet)
    extractPayload (txOutRef, out@PV2.TxOut {PV2.txOutAddress, PV2.txOutValue, PV2.txOutDatum}) =
      do
        let mRefScript = outputReferenceScriptHash out
        txSkelOutDatum <-
          case txOutDatum of
            Pl.NoOutputDatum -> Just TxSkelOutNoDatum
            Pl.OutputDatum datum -> Map.lookup (Pl.datumHash datum) mcstDatums
            Pl.OutputDatumHash hash -> Map.lookup hash mcstDatums
        return
          ( txOutAddress,
            UtxoPayloadSet [UtxoPayload txOutRef txOutValue txSkelOutDatum mRefScript]
          )

-- | Slightly more concrete version of 'UtxoState', used to actually run the
-- simulation. We keep a map from datum hash to datum in order to display the
-- contents of the state to the user.
data MockChainSt = MockChainSt
  { mcstIndex :: Ledger.UtxoIndex,
    mcstDatums :: Map Pl.DatumHash TxSkelOutDatum,
    mcstValidators :: Map Pl.ValidatorHash (Pl.Versioned Pl.Validator),
    mcstCurrentSlot :: Ledger.Slot
  }
  deriving (Show)

instance Eq MockChainSt where
  (MockChainSt index1 datums1 validators1 currentSlot1)
    == (MockChainSt index2 datums2 validators2 currentSlot2) =
      and
        [ index1 == index2,
          datums1 == datums2,
          validators1 == validators2,
          currentSlot1 == currentSlot2
        ]

-- | The 'UtxoIndex' contains 'Ledger.Tx.Internal.TxOut's, but we want a map
-- that contains 'Plutus.V2.Ledger.Api.TxOut'.
utxoIndexToTxOutMap :: Ledger.UtxoIndex -> Map PV2.TxOutRef PV2.TxOut
utxoIndexToTxOutMap (Ledger.UtxoIndex utxoMap) = Map.map txOutV2FromLedger utxoMap

instance Default Ledger.Slot where
  def = Ledger.Slot 0

newtype MockChainEnv = MockChainEnv {mceParams :: Emulator.Params}
  deriving (Show)

instance Default MockChainEnv where
  def = MockChainEnv def

newtype MockChainT m a = MockChainT
  {unMockChain :: ReaderT MockChainEnv (StateT MockChainSt (ExceptT MockChainError m)) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError, MonadReader MockChainEnv)

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
  empty = MockChainT $ ReaderT $ const $ StateT $ const $ ExceptT empty
  (<|>) = combineMockChainT (<|>)

combineMockChainT ::
  (Monad m) =>
  (forall a. m a -> m a -> m a) ->
  MockChainT m x ->
  MockChainT m x ->
  MockChainT m x
combineMockChainT f ma mb = MockChainT $
  ReaderT $ \r ->
    StateT $ \s ->
      let resA = runExceptT $ runStateT (runReaderT (unMockChain ma) r) s
          resB = runExceptT $ runStateT (runReaderT (unMockChain mb) r) s
       in ExceptT $ f resA resB

mapMockChainT ::
  (m (Either MockChainError (a, MockChainSt)) -> n (Either MockChainError (b, MockChainSt))) ->
  MockChainT m a ->
  MockChainT n b
mapMockChainT f = MockChainT . mapReaderT (mapStateT (mapExceptT f)) . unMockChain

-- | Executes a 'MockChainT' from some initial state and environment; does /not/
-- convert the 'MockChainSt' into a 'UtxoState'.
runMockChainTRaw ::
  (Monad m) =>
  MockChainEnv ->
  MockChainSt ->
  MockChainT m a ->
  m (Either MockChainError (a, MockChainSt))
runMockChainTRaw e0 i0 =
  runExceptT
    . flip runStateT i0
    . flip runReaderT e0
    . unMockChain

-- | Executes a 'MockChainT' from an initial state set up with the given
-- initial value distribution. Similar to 'runMockChainT', uses the default
-- environment. Returns a 'UtxoState' instead of a 'MockChainSt'. If you need
-- the later, use 'runMockChainTRaw'
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom i0 =
  fmap (fmap $ second mcstToUtxoState) . runMockChainTRaw def (mockChainSt0From i0)

-- | Executes a 'MockChainT' from the canonical initial state and environment.
-- The canonical environment uses the default 'SlotConfig' and
-- @Cooked.Wallet.wallet 1@ as the sole wallet signing transactions.
runMockChainT :: (Monad m) => MockChainT m a -> m (Either MockChainError (a, UtxoState))
runMockChainT = runMockChainTFrom def

-- | See 'runMockChainTRaw'
runMockChainRaw :: MockChainEnv -> MockChainSt -> MockChain a -> Either MockChainError (a, MockChainSt)
runMockChainRaw e0 i0 = runIdentity . runMockChainTRaw e0 i0

-- | See 'runMockChainTFrom'
runMockChainFrom ::
  InitialDistribution -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainFrom i0 = runIdentity . runMockChainTFrom i0

-- | See 'runMockChainT'
runMockChain :: MockChain a -> Either MockChainError (a, UtxoState)
runMockChain = runIdentity . runMockChainT

-- * Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt utxoIndex0 Map.empty Map.empty def

mockChainSt0From :: InitialDistribution -> MockChainSt
mockChainSt0From i0 = MockChainSt (utxoIndex0From i0) Map.empty Map.empty def

instance Default MockChainSt where
  def = mockChainSt0

utxoIndex0From :: InitialDistribution -> Ledger.UtxoIndex
utxoIndex0From i0 = Ledger.initialise [[Ledger.Valid $ Ledger.EmulatorTx $ initialTxFor i0]]
  where
    -- Bootstraps an initial transaction resulting in a state where wallets
    -- possess UTxOs fitting a given 'InitialDistribution'
    initialTxFor :: InitialDistribution -> Ledger.Tx
    initialTxFor initDist =
      mempty
        { Ledger.txMint =
            fromRight'
              . Ledger.toCardanoValue
              $ mconcat (map (mconcat . snd) initDist'),
          Ledger.txOutputs = concatMap (\(w, vs) -> map (initUtxosFor w) vs) initDist'
        }
      where
        initUtxosFor w v = toPlTxOut @() (walletAddress w) v Nothing

        initDist' = Map.toList $ unInitialDistribution initDist

        toPlTxOut :: Pl.ToData a => Pl.Address -> Pl.Value -> Maybe a -> Ledger.TxOut
        toPlTxOut addr value datum = toPlTxOut' addr value datum'
          where
            datum' =
              maybe
                PV2.NoOutputDatum
                ( PV2.OutputDatumHash
                    . Pl.datumHash
                    . Pl.Datum
                    . Pl.toBuiltinData
                )
                datum

        toPlTxOut' :: Pl.Address -> Pl.Value -> PV2.OutputDatum -> Ledger.TxOut
        toPlTxOut' addr value datum = Ledger.TxOut $ toCardanoTxOut' addr value datum

        toCardanoTxOut' ::
          Pl.Address ->
          Pl.Value ->
          PV2.OutputDatum ->
          C.TxOut C.CtxTx C.BabbageEra
        toCardanoTxOut' addr value datum =
          fromRight' $
            Ledger.toCardanoTxOut
              theNetworkId
              (PV2.TxOut addr value datum Nothing)

        fromRight' :: Show e => Either e a -> a
        fromRight' x = case x of
          Left err -> error $ show err
          Right res -> res

        theNetworkId :: C.NetworkId
        theNetworkId = C.Testnet $ C.NetworkMagic 42 -- TODO PORT what's magic?

utxoIndex0 :: Ledger.UtxoIndex
utxoIndex0 = utxoIndex0From def

-- * Direct Interpretation of Operations

instance Monad m => MonadBlockChainBalancing (MockChainT m) where
  getParams = asks mceParams
  validatorFromHash valHash = gets $ Map.lookup valHash . mcstValidators
  txOutByRefLedger outref = gets $ Map.lookup outref . Ledger.getIndex . mcstIndex
  datumFromHash datumHash = (txSkelOutUntypedDatum <=< Map.lookup datumHash) <$> gets mcstDatums
  utxosAtLedger addr = filter ((addr ==) . outputAddress . txOutV2FromLedger . snd) <$> allUtxosLedger

instance Monad m => MonadBlockChainWithoutValidation (MockChainT m) where
  allUtxosLedger = gets $ Map.toList . Ledger.getIndex . mcstIndex

  currentSlot = gets mcstCurrentSlot

  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

instance Monad m => MonadBlockChain (MockChainT m) where
  validateTxSkel skelUnbal = do
    (skel, fee, collateralInputs) <- balancedTxSkel skelUnbal
    tx <- balancedTx (skel, fee, collateralInputs)
    consumedData <- txSkelInputData skel
    someCardanoTx <-
      runTransactionValidation
        tx
        (txOptUnsafeModTx $ txSkelOpts skel)
        consumedData
        (txSkelOutputData skel)
        (txSkelOutValidators skel <> txSkelOutReferenceScripts skel)
    when (txOptAutoSlotIncrease $ txSkelOpts skel) $
      modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    return (Ledger.CardanoApiTx someCardanoTx)

runTransactionValidation ::
  Monad m =>
  -- | The transaction to validate. It should already be balanced, and include
  -- appropriate fees and collateral.
  C.Tx C.BabbageEra ->
  -- | Modifications to apply to the transaction right before it is submitted.
  [RawModTx] ->
  -- | The data consumed by the transaction
  Map Pl.DatumHash Pl.Datum ->
  -- | The data produced by the transaction
  Map Pl.DatumHash TxSkelOutDatum ->
  -- | The validators protecting transaction outputs, and the validators in the
  -- reference script field of transaction outputs. The 'MockChain' will
  -- remember them.
  Map Pl.ValidatorHash (Pl.Versioned Pl.Validator) ->
  MockChainT m Ledger.SomeCardanoApiTx
runTransactionValidation cardanoTx rawModTx consumedData producedData outputValidators = do
  utxoIndex <- gets mcstIndex
  theParams <- getParams
  theSlot <- currentSlot
  let cardanoIndex :: CardanoLedger.UTxO Emulator.EmulatorEra
      cardanoIndex = either (error . show) id $ Ledger.fromPlutusIndex utxoIndex

      -- "Ledger.CardanoTx" is a plutus-apps type 'Tx BabbageEra' is a
      -- cardano-api type with the information we need. This wraps the latter
      -- inside the former.
      txWrapped :: Ledger.CardanoTx
      txWrapped = Ledger.CardanoApiTx $ Ledger.CardanoApiEmulatorEraTx cardanoTx

      mValidationError :: Either Ledger.ValidationErrorInPhase Ledger.ValidationSuccess
      mValidationError = Emulator.validateCardanoTx theParams theSlot cardanoIndex txWrapped

      newUtxoIndex :: Ledger.UtxoIndex
      newUtxoIndex = case mValidationError of
        Left (Ledger.Phase1, _) -> utxoIndex
        Left (Ledger.Phase2, _) ->
          -- Despite its name, this actually deletes the collateral UTxOs from
          -- the index
          Ledger.insertCollateral txWrapped utxoIndex
        Right _ -> Ledger.insert txWrapped utxoIndex
  case mValidationError of
    Left err -> throwError (MCEValidationError err)
    Right _ -> do
      -- Validation succeeded; now we update the UTxO index, the managed
      -- datums, and the managed Validators. The new mcstIndex is just
      -- `newUtxoIndex`; the new mcstDatums is computed by removing the datum
      -- hashes have been consumed and adding those that have been created in
      -- the transaction.
      modify'
        ( \st ->
            st
              { mcstIndex = newUtxoIndex,
                mcstDatums = (mcstDatums st Map.\\ consumedData) `Map.union` producedData,
                mcstValidators = mcstValidators st `Map.union` outputValidators
              }
        )
      return (Ledger.CardanoApiEmulatorEraTx cardanoTx)
