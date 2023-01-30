{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Use section" #-}

module Cooked.MockChain.Direct where

import qualified Cardano.Api as C
import qualified Cardano.Ledger.Shelley.API as CardanoLedger
import qualified Cardano.Node.Emulator.Params as Emulator
import qualified Cardano.Node.Emulator.TimeSlot as Emulator
import qualified Cardano.Node.Emulator.Validation as Emulator
import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.MockChain.Balancing
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.UtxoState
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import Prettyprinter (Doc)

-- * Direct Emulation

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a simple way to call
-- validator scripts directly, without the need for all the complexity the 'Contract'
-- monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', which is a map from 'Pl.Address' to
-- @(Pl.Value, Maybe Pl.Datum)@, and corresponds to the utxo mental model most people have.
-- Internally, however, we keep a 'Pl.UtxoIndex' in our state and feeding it to 'Pl.validateTx'.
-- For convenience, we also keep a map of 'Pl.Address' to 'Pl.Datum', giving is a simple
-- way of managing the current utxo state.

mcstToUtxoState :: MockChainSt -> UtxoState
mcstToUtxoState s =
  UtxoState
    . Map.fromListWith (<>)
    . map (go . snd)
    . Map.toList
    . utxoIndexToTxOutMap
    . mcstIndex
    $ s
  where
    go :: PV2.TxOut -> (Pl.Address, UtxoValueSet)
    go (PV2.TxOut addr val outputDatum _) = do
      (addr, UtxoValueSet [(val, outputDatumToUtxoDatum outputDatum)])

    outputDatumToUtxoDatum :: PV2.OutputDatum -> Maybe UtxoDatum
    outputDatumToUtxoDatum PV2.NoOutputDatum = Nothing
    outputDatumToUtxoDatum (PV2.OutputDatumHash datumHash) =
      do
        (datum, datumStr) <- Map.lookup datumHash (mcstDatums s)
        return $ UtxoDatum datum False datumStr
    outputDatumToUtxoDatum (PV2.OutputDatum datum) =
      do
        (_, datumStr) <- Map.lookup (Pl.datumHash datum) (mcstDatums s)
        return $ UtxoDatum datum True datumStr

-- | Slightly more concrete version of 'UtxoState', used to actually run the simulation.
--  We keep a map from datum hash to datum, then a map from txOutRef to datumhash
--  Additionally, we also keep a map from datum hash to the underlying value's "show" result,
--  in order to display the contents of the state to the user.
data MockChainSt = MockChainSt
  { mcstIndex :: Ledger.UtxoIndex,
    mcstDatums :: Map Pl.DatumHash (Pl.Datum, Doc ()),
    mcstValidators :: Map Pl.ValidatorHash (Pl.Versioned Pl.Validator),
    mcstCurrentSlot :: Ledger.Slot
  }
  deriving (Show)

-- | There is no natural 'Eq' instance for 'Doc' (pretty printed document)
-- which we store for each datum in the state. The 'Eq' instance for 'Doc'
-- ignores these pretty printed docs.
instance Eq MockChainSt where
  (MockChainSt index1 datums1 validators1 currentSlot1)
    == (MockChainSt index2 datums2 validators2 currentSlot2) =
      and
        [ index1 == index2,
          Map.map fst datums1 == Map.map fst datums2,
          validators1 == validators2,
          currentSlot1 == currentSlot2
        ]

-- | The 'UtxoIndex' contains 'Ledger.Tx.Internal.TxOut's, but we want a map that
-- contains 'Plutus.V2.Ledger.Api.TxOut'.
utxoIndexToTxOutMap :: Ledger.UtxoIndex -> Map PV2.TxOutRef PV2.TxOut
utxoIndexToTxOutMap (Ledger.UtxoIndex utxoMap) = Map.map txOutV2FromLedger utxoMap

instance Default Ledger.Slot where
  def = Ledger.Slot 0

data MockChainEnv = MockChainEnv
  { mceParams :: Emulator.Params,
    mceSigners :: NEList.NonEmpty Wallet
  }
  deriving (Show)

instance Default MockChainEnv where
  def = MockChainEnv def (wallet 1 NEList.:| [])

-- | The actual 'MockChainT' is a trivial combination of 'StateT' and 'ExceptT'
newtype MockChainT m a = MockChainT
  {unMockChain :: ReaderT MockChainEnv (StateT MockChainSt (ExceptT MockChainError m)) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError, MonadReader MockChainEnv)

-- | Non-transformer variant
type MockChain = MockChainT Identity

-- Custom monad instance made to increase the slot count automatically
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

-- | Executes a 'MockChainT' from an initial state set up with the given initial value distribution.
-- Similar to 'runMockChainT', uses the default environment. Returns a 'UtxoState' instead of
-- a 'MockChainSt'. If you need the later, use 'runMockChainTRaw'
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom i0 =
  fmap (fmap $ second mcstToUtxoState) . runMockChainTRaw def (mockChainSt0From i0)

-- | Executes a 'MockChainT' from the canonical initial state and environment. The canonical
--  environment uses the default 'SlotConfig' and @[Cooked.Wallet.wallet 1]@ as the sole
--  wallet signing transactions.
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

-- Canonical initial values

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

utxoIndex0 :: Ledger.UtxoIndex
utxoIndex0 = utxoIndex0From def

-- ** Direct Interpretation of Operations

instance (Monad m) => MonadBlockChainWithoutValidation (MockChainT m) where
  getParams = asks mceParams

  validatorFromHash valHash = gets $ Map.lookup valHash . mcstValidators

  txOutByRefLedger outref = gets $ Map.lookup outref . Ledger.getIndex . mcstIndex

  ownPaymentPubKeyHash = asks (walletPKHash . NEList.head . mceSigners)

  allUtxosLedger = gets $ Map.toList . Ledger.getIndex . mcstIndex

  datumFromHash datumHash = Map.lookup datumHash <$> gets mcstDatums

  currentSlot = gets mcstCurrentSlot

  currentTime = asks (Emulator.slotToEndPOSIXTime . Emulator.pSlotConfig . mceParams) <*> gets mcstCurrentSlot

  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

  awaitTime t = do
    sc <- asks $ Emulator.pSlotConfig . mceParams
    s <- awaitSlot (1 + Emulator.posixTimeToEnclosingSlot sc t)
    return $ Emulator.slotToBeginPOSIXTime sc s

instance Monad m => MonadBlockChain (MockChainT m) where
  validateTxSkel skelUnbal = do
    let balancingWallet =
          case txOptBalanceWallet . txSkelOpts $ skelUnbal of
            BalanceWithFirstSigner -> NEList.head (txSkelSigners skelUnbal)
            BalanceWith wallet -> wallet
    let collateralWallet = balancingWallet
    (skel, fee) <-
      if txOptBalance . txSkelOpts $ skelUnbal
        then
          setFeeAndBalance
            balancingWallet
            skelUnbal
        else return (skelUnbal, Fee 0)
    collateralInputs <- calcCollateral collateralWallet -- TODO: Why is it OK to balance first and then add collateral?
    params <- asks mceParams
    managedData <- gets mcstDatums
    managedTxOuts <- gets $ utxoIndexToTxOutMap . mcstIndex
    managedValidators <- gets mcstValidators
    case generateTxBodyContent def {gtpCollateralIns = collateralInputs, gtpFee = fee} params managedData managedTxOuts managedValidators skel of
      Left err -> throwError $ MCEGenerationError err
      Right txBodyContent -> do
        slot <- currentSlot
        index <- gets mcstIndex
        inputTxDatums <- Map.map fst <$> txSkelInputData skel
        someCardanoTx <-
          runTransactionValidation
            slot
            params
            index
            (NEList.toList $ txSkelSigners skel)
            txBodyContent
            inputTxDatums
            (txSkelOutputData skel)
            (txSkelOutValidators skel)
            (txOptUnsafeModTx $ txSkelOpts skel)
        when (txOptAutoSlotIncrease $ txSkelOpts skel) $
          modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
        return (Ledger.CardanoApiTx someCardanoTx)

runTransactionValidation ::
  (Monad m) =>
  -- | The current slot
  Ledger.Slot ->
  -- | The parameters of the MockChain
  Emulator.Params ->
  -- | The currently known UTxOs
  Ledger.UtxoIndex ->
  -- | List of signers that were on the 'TxSkel'. This will at least have to
  -- include all wallets that have to sign in order for the transaction to be
  -- phase 1 - valid.
  [Wallet] ->
  -- | The transaction to validate. It should already be balanced and have
  -- appropriate collateral.
  --
  -- The witnesses in script inputs must use the 'C.InlineScriptDatum'
  -- constructor. We've not really understood why this is required by
  -- 'Pl.validateCardanoTx', but if the 'C.ScriptDatumForTxIn' constructor with
  -- an explicit datum is used, we see an error about
  -- 'NonOutputSupplimentaryDatums'.
  C.TxBodyContent C.BuildTx C.BabbageEra ->
  -- | The data on transaction inputs. If the transaction is successful, these
  -- will be deleted from the 'mcstDatums'.
  Map Pl.DatumHash Pl.Datum ->
  -- | The data on transaction outputs. If the transaction is successful, these
  -- will be added to the 'mcstDatums'.
  Map Pl.DatumHash (Pl.Datum, Doc ()) ->
  -- | The validators on transaction outputs.
  Map Pl.ValidatorHash (Pl.Versioned Pl.Validator) ->
  -- | Modifications to apply to the transaction right before it is submitted.
  [RawModTx] ->
  MockChainT m Ledger.SomeCardanoApiTx
runTransactionValidation slot parms utxoIndex signers txBodyContent consumedData producedData outputValidators rawModTx =
  let cardanoIndex :: CardanoLedger.UTxO Emulator.EmulatorEra
      cardanoIndex = either (error . show) id $ Ledger.fromPlutusIndex utxoIndex

      cardanoTx, cardanoTxSigned, cardanoTxModified :: C.Tx C.BabbageEra
      cardanoTx =
        either
          (error . ("Error building Cardano Tx: " <>) . show)
          (flip C.Tx [])
          -- TODO 'makeTransactionBody' will be deprecated in newer versions of
          -- cardano-api
          -- The new name (which is more fitting as well) is
          -- 'createAndValidateTransactionBody'.
          (C.makeTransactionBody txBodyContent)
      cardanoTxSigned = L.foldl' (flip txAddSignatureAPI) cardanoTx signers
        where
          txAddSignatureAPI :: Wallet -> C.Tx C.BabbageEra -> C.Tx C.BabbageEra
          txAddSignatureAPI w tx = case signedTx of
            Ledger.CardanoApiTx (Ledger.CardanoApiEmulatorEraTx tx') -> tx'
            Ledger.EmulatorTx _ -> error "Expected CardanoApiTx but got EmulatorTx"
            -- looking at the implementation of Pl.addCardanoTxSignature
            -- it never changes the constructor used, so the above branch
            -- shall never happen
            where
              signedTx =
                Ledger.addCardanoTxSignature
                  (walletSK w)
                  (Ledger.CardanoApiTx $ Ledger.CardanoApiEmulatorEraTx tx)
      cardanoTxModified = applyRawModOnBalancedTx rawModTx cardanoTxSigned

      -- "Pl.CardanoTx" is a plutus-apps type
      -- "Tx BabbageEra" is a cardano-api type with the information we need
      -- This wraps the latter inside the former
      txWrapped :: Ledger.CardanoTx
      txWrapped = Ledger.CardanoApiTx $ Ledger.CardanoApiEmulatorEraTx cardanoTxModified

      mValidationError :: Either Ledger.ValidationErrorInPhase Ledger.ValidationSuccess
      mValidationError = Emulator.validateCardanoTx parms slot cardanoIndex txWrapped

      newUtxoIndex :: Ledger.UtxoIndex
      newUtxoIndex = case mValidationError of
        Left (Ledger.Phase1, _) -> utxoIndex
        Left (Ledger.Phase2, _) ->
          -- Despite its name, this actually deletes the collateral Utxos from
          -- the index
          Ledger.insertCollateral txWrapped utxoIndex
        Right _ -> Ledger.insert txWrapped utxoIndex
   in case mValidationError of
        Left err -> throwError (MCEValidationError err)
        Right _ -> do
          -- Validation succeeded; now we update the UTxO index, the managed
          -- datums, and the managed Validators. The new mcstIndex is just
          -- `newUtxoIndex`; the new mcstDatums is computed by removing the
          -- datum hashes have been consumed and adding those that have been
          -- created in the transaction.
          modify'
            ( \st ->
                st
                  { mcstIndex = newUtxoIndex,
                    mcstDatums = (mcstDatums st Map.\\ consumedData) `Map.union` producedData,
                    mcstValidators = mcstValidators st `Map.union` outputValidators
                  }
            )

          return (Ledger.CardanoApiEmulatorEraTx cardanoTxModified)
