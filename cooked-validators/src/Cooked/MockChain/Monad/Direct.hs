{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Use section" #-}

module Cooked.MockChain.Monad.Direct where

import qualified Cardano.Api as Api
import qualified Cardano.Api as C
import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.MockChain.Misc
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.GenerateTx
import Cooked.MockChain.UtxoPredicate
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Default
import Data.Function (on)
import Data.List
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Constraints as Pl
import Ledger.Orphans ()
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Tx.CardanoAPI as Pl hiding (makeTransactionBody)
import qualified Ledger.Validation as Pl
import qualified Ledger.Value as Value
import Optics.Core
import qualified Plutus.V2.Ledger.Tx as Pl2
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl

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
  UtxoState . Map.fromListWith (<>) . map (uncurry go1) . Map.toList . Pl.getIndex . mcstIndex $ s
  where
    go1 :: Pl.TxOutRef -> Pl.TxOut -> (Pl.Address, UtxoValueSet)
    go1 _ (Pl.TxOut (Pl.fromCardanoTxOutToPV2TxInfoTxOut -> Pl2.TxOut addr val mdh _)) = do
      (addr, UtxoValueSet [(val, od2dh mdh >>= dh2datum)])

    od2dh :: Pl2.OutputDatum -> Maybe Pl.DatumHash
    od2dh Pl2.NoOutputDatum = Nothing
    od2dh (Pl2.OutputDatumHash dh) = Just dh
    od2dh (Pl2.OutputDatum datum) = Just $ Pl.datumHash datum

    dh2datum :: Pl.DatumHash -> Maybe UtxoDatum
    dh2datum datumHash = do
      datumStr <- Map.lookup datumHash (mcstStrDatums s)
      datum <- Map.lookup datumHash (mcstDatums s)
      return $ UtxoDatum datum datumStr

-- | Slightly more concrete version of 'UtxoState', used to actually run the simulation.
--  We keep a map from datum hash to datum, then a map from txOutRef to datumhash
--  Additionally, we also keep a map from datum hash to the underlying value's "show" result,
--  in order to display the contents of the state to the user.
data MockChainSt = MockChainSt
  { mcstIndex :: Pl.UtxoIndex,
    mcstDatums :: Map Pl.DatumHash Pl.Datum,
    mcstStrDatums :: Map Pl.DatumHash String,
    mcstCurrentSlot :: Pl.Slot
  }
  deriving (Show, Eq)

instance Default Pl.Slot where
  def = Pl.Slot 0

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError
  = MCEValidationError Pl.ValidationErrorInPhase
  | MCETxError Pl.MkTxError
  | MCEUnbalanceable String BalanceStage TxSkel
  | MCENoSuitableCollateral
  | MCEGenerationError GenerateTxError
  | MCECalcFee MockChainError
  | FailWith String
  deriving (Show, Eq)

-- | Describes us which stage of the balancing process are we at. This is needed
--  to distinguish the successive calls to balancing while computing fees from
--  the final call to balancing
data BalanceStage
  = BalCalcFee
  | BalFinalizing
  deriving (Show, Eq)

data MockChainEnv = MockChainEnv
  { mceParams :: Pl.Params,
    mceSigners :: NE.NonEmpty Wallet
  }
  deriving (Show)

instance Default MockChainEnv where
  def = MockChainEnv def (wallet 1 NE.:| [])

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
--  environment uses the default 'SlotConfig' and @[Cooked.MockChain.Wallet.wallet 1]@ as the sole
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

utxoIndex0From :: InitialDistribution -> Pl.UtxoIndex
utxoIndex0From i0 = Pl.initialise [[Pl.Valid $ Pl.EmulatorTx $ initialTxFor i0]]

utxoIndex0 :: Pl.UtxoIndex
utxoIndex0 = utxoIndex0From def

-- ** Direct Interpretation of Operations

instance (Monad m) => MonadBlockChain (MockChainT m) where
  validateTxSkel skelUnbal = do
    -- TODO We use the first signer as a default wallet for fees and
    -- collaterals. These should become parameters of validateTxSkel in the
    -- future
    firstSigner NE.:| _ <- askSigners
    let balancingWallet = firstSigner
    let balancingWalletPkh = walletPKHash firstSigner
    let collateralWallet = firstSigner
    skel <-
      if balance . txSkelOpts $ skelUnbal
        then
          setFeeAndBalance
            balancingWalletPkh
            -- We need to add the balancing wallet to the signers before
            -- calculating the fees. The fees depend on the number of signers: they
            -- make the transaction heavier.
            ( skelUnbal
                & txSkelRequiredSignersL
                %~ (<> Set.singleton balancingWalletPkh)
            )
        else return skelUnbal
    collateralInputs <- calcCollateral collateralWallet
    params <- askParams
    managedData <- gets mcstDatums
    -- See the comment at the arguments of 'runTransactionValidation' on why we
    -- have to use 'generateTxBodyContentWithoutInputDatums' here. The
    -- documentation of the 'generateTxBodyContent*' functions might also be
    -- informative.
    case generateTxBodyContent withDatums {gtpCollateralIns = collateralInputs} params managedData skel of
      Left err -> throwError $ MCEGenerationError err
      Right txBodyContent -> do
        slot <- currentSlot
        someCardanoTx <-
          runTransactionValidation
            slot
            params
            (Pl.UtxoIndex $ txSkelUtxoIndex skel <> Map.fromList (map (sOutTxOutRef &&& sOutTxOut) $ Set.toList collateralInputs)) -- These are exactly the UTxOs that will be (maybe, in the case of collateral) consumed by the transaction
            [balancingWallet, collateralWallet]
            txBodyContent
            (txSkelInputData skel)
            (txSkelOutputData skel)
            (unsafeModTx $ txSkelOpts skel)
        when (autoSlotIncrease $ txSkelOpts skel) $
          modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
        return (Pl.CardanoApiTx someCardanoTx)

  txOutByRef outref = gets (Map.lookup outref . Pl.getIndex . mcstIndex)

  ownPaymentPubKeyHash = asks (walletPKHash . NE.head . mceSigners)

  utxosSuchThat = utxosSuchThat'

  utxosSuchThisAndThat = utxosSuchThisAndThat'

  datumFromHash datumHash = Map.lookup datumHash <$> gets mcstDatums

  currentSlot = gets mcstCurrentSlot

  currentTime = asks (Pl.slotToEndPOSIXTime . Pl.pSlotConfig . mceParams) <*> gets mcstCurrentSlot

  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

  awaitTime t = do
    sc <- slotConfig
    s <- awaitSlot (1 + Pl.posixTimeToEnclosingSlot sc t)
    return $ Pl.slotToBeginPOSIXTime sc s

instance (Monad m) => MonadMockChain (MockChainT m) where
  signingWith ws = local $ \env -> env {mceSigners = ws}

  askSigners = asks mceSigners

  askParams = asks mceParams

  localParams f = local (\e -> e {mceParams = f (mceParams e)})

runTransactionValidation ::
  (Monad m) =>
  -- | The current slot
  Pl.Slot ->
  -- | The parameters of the MockChain
  Pl.Params ->
  -- | the input UTxOs of the transaction, and the collateral UTxO(s)
  Pl.UtxoIndex ->
  -- | List of signers that have to be on the transaction in order for it to be
  -- Phase 1 - valid. This will be the list that contains the ballancing wallet
  -- and the collateral wallet.
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
  Map Pl.DatumHash Pl.Datum ->
  -- | Modifications to apply to the transaction right before it is submitted.
  [RawModTx] ->
  MockChainT m Pl.SomeCardanoApiTx
runTransactionValidation slot parms inputUtxoIndex signers txBodyContent consumedData producedData rawModTx =
  let cardanoIndex :: Pl.UTxO Pl.EmulatorEra
      cardanoIndex = either (error . show) id $ Pl.fromPlutusIndex inputUtxoIndex

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
            Pl.CardanoApiTx (Pl.CardanoApiEmulatorEraTx tx') -> tx'
            Pl.EmulatorTx _ -> error "Expected CardanoApiTx but got EmulatorTx"
            -- looking at the implementation of Pl.addCardanoTxSignature
            -- it never changes the constructor used, so the above branch
            -- shall never happen
            where
              signedTx =
                Pl.addCardanoTxSignature
                  (walletSK w)
                  (Pl.CardanoApiTx $ Pl.CardanoApiEmulatorEraTx tx)
      cardanoTxModified = applyRawModOnBalancedTx rawModTx cardanoTxSigned

      -- "Pl.CardanoTx" is a plutus-apps type
      -- "Tx BabbageEra" is a cardano-api type with the information we need
      -- This wraps the latter inside the former
      txWrapped :: Pl.CardanoTx
      txWrapped = Pl.CardanoApiTx $ Pl.CardanoApiEmulatorEraTx cardanoTxModified

      mValidationError :: Maybe Pl.ValidationErrorInPhase
      mValidationError = Pl.validateCardanoTx parms slot cardanoIndex txWrapped
   in case mValidationError of
        Just err@(Pl.Phase1, _) -> throwError (MCEValidationError err)
        Just err@(Pl.Phase2, _) -> do
          -- Script failure -- keep the collateral inputs and throw the error.
          modify'
            ( \st ->
                st
                  { mcstIndex =
                      -- Despite its name, this actually deletes the collateral Utxos from
                      -- the index
                      Pl.insertCollateral txWrapped (mcstIndex st)
                  }
            )
          throwError (MCEValidationError err)
        Nothing -> do
          -- Validation succeeded -- insert the transaction in the UTxO index
          -- and update the managed datums by deleting input datums and adding
          -- output datums
          modify'
            ( \st ->
                st
                  { mcstIndex = Pl.insert txWrapped (mcstIndex st),
                    mcstDatums = (mcstDatums st Map.\\ consumedData) `Map.union` producedData
                  }
            )

          return (Pl.CardanoApiEmulatorEraTx cardanoTxModified)

utxosSuchThisAndThat' ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  (Pl.Address -> Bool) ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThisAndThat' addrPred datumPred = do
  ix <- gets (Pl.getIndex . mcstIndex)
  let ix' = Map.filter (addrPred . Pl.txOutAddress) ix
  -- TODO: Make this less ugly
  catMaybes
    <$> mapM
      (\(oref, out) -> (first (SpendableOut oref) <$>) <$> go oref out)
      (Map.toList ix')
  where
    go :: Pl.TxOutRef -> Pl.TxOut -> MockChainT m (Maybe (Pl.ChainIndexTxOut, Maybe a))
    go oref out@(Pl.TxOut (Api.TxOut _ _ cDatum _)) =
      pure (cTxOutToCito out)
        >>= \case
          Nothing -> pure Nothing
          Just cito -> do
            mDatum <- extractDatum oref cito $ Pl.fromCardanoTxOutDatumHash cDatum
            pure $
              if datumPred mDatum (Pl._ciTxOutValue cito)
                then Just (cito, mDatum)
                else Nothing

    extractDatum :: Pl.TxOutRef -> Pl.ChainIndexTxOut -> Maybe Pl.DatumHash -> MockChainT m (Maybe a)
    extractDatum oref cito Nothing
      | isScriptCito cito = fail $ "ScriptCredential with no datum hash: " ++ show oref
      | otherwise = pure Nothing
    extractDatum oref _ (Just dh) = do
      managedDatums <- gets mcstDatums
      Just <$> case dh `Map.lookup` managedDatums of
        -- TODO PORT previously this was an error only for script datums,
        -- but I think it should be an error for any datum hash
        -- that's mentioned but cannot be found in managedDatums.
        Nothing -> fail $ "Unmanaged datum with hash: " ++ show dh ++ " at: " ++ show oref
        Just datum -> maybe failBadConvert pure (Pl.fromBuiltinData $ Pl.getDatum datum)
      where
        failBadConvert = fail $ "Can't convert from builtin data at: " ++ show oref ++ "; are you sure this is the right type?"

-- | Check 'utxosSuchThat' for details
utxosSuchThat' ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  Pl.Address ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThat' addr = utxosSuchThisAndThat' (== addr)

ensureTxSkelOutsMinAda :: TxSkel -> TxSkel
ensureTxSkelOutsMinAda = txSkelOutsL % traversed % outValueL %~ ensureHasMinAda

-- | Sets the '_txSkelFee' according to our environment. The transaction fee
-- gets set realistically, based on a fixpoint calculation taken from
-- /plutus-apps/.
setFeeAndBalance :: (Monad m) => Pl.PubKeyHash -> TxSkel -> MockChainT m TxSkel
setFeeAndBalance balancePK skel0 = do
  let skel =
        if adjustUnbalTx $ txSkelOpts skel0
          then ensureTxSkelOutsMinAda skel0
          else skel0
  utxos <- map (sOutTxOutRef &&& sOutTxOut) <$> pkUtxos balancePK
  mockChainParams <- asks mceParams
  case Pl.fromPlutusIndex $ Pl.UtxoIndex $ txSkelUtxoIndex skel <> Map.fromList utxos of
    Left err -> throwError $ FailWith $ "setFeeAndValidRange: " ++ show err
    Right cUtxoIndex -> do
      -- We start with a high startingFee, but theres a chance that 'w' doesn't have enough funds
      -- so we'll see an unbalanceable error; in that case, we switch to the minimum fee and try again.
      -- That feels very much like a hack, and it is. Maybe we should witch to starting with a small
      -- fee and then increasing, but that might require more iterations until its settled.
      -- For now, let's keep it just like the folks from plutus-apps did it.
      let startingFee = 3000000
      calcFee 5 startingFee cUtxoIndex mockChainParams skel
        `catchError` \case
          -- Impossible to balance the transaction
          MCEUnbalanceable _ BalCalcFee _ ->
            -- WARN
            -- "Pl.minFee" takes an actual Tx but we no longer provide it
            -- since we work on "TxSkel". However, for now, the
            -- implementation of "Pl.minFee" is a constant of 10 lovelace.
            -- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Index.hs#L116
            let minFee = 10 -- forall tx. Pl.minFee tx = 10 lovelace
             in calcFee 5 minFee cUtxoIndex mockChainParams skel
          -- Impossible to generate the Cardano transaction at all
          e -> throwError e
  where
    -- Inspired by https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-contract/src/Wallet/Emulator/Wallet.hs#L329

    calcFee ::
      (Monad m) =>
      Int ->
      Integer ->
      Pl.UTxO Pl.EmulatorEra ->
      Pl.Params ->
      TxSkel ->
      MockChainT m TxSkel
    calcFee n fee cUtxoIndex parms skel = do
      let skelWithFee = skel & txSkelFeeL .~ fee
          bPol = balanceOutputPolicy $ txSkelOpts skel
      attemptedSkel <- balanceTxFromAux bPol BalCalcFee balancePK skelWithFee
      manageData <- gets mcstDatums
      case estimateTxSkelFee parms cUtxoIndex manageData attemptedSkel of
        -- necessary to capture script failure for failed cases
        Left err@MCEValidationError {} -> throwError err
        Left err -> throwError $ MCECalcFee err
        Right newFee
          | newFee == fee -> do
            -- Debug.Trace.traceM "Reached fixpoint:"
            -- Debug.Trace.traceM $ "- fee = " <> show fee
            -- Debug.Trace.traceM $ "- skeleton = " <> show (attemptedSkel {_txSkelFee = fee})
            pure attemptedSkel {txSkelFee = fee} -- reached fixpoint
          | n == 0 -> do
            -- Debug.Trace.traceM $ "Max iteration reached: newFee = " <> show newFee
            pure attemptedSkel {txSkelFee = max newFee fee} -- maximum number of iterations
          | otherwise -> do
            -- Debug.Trace.traceM $ "New iteration: newfee = " <> show newFee
            calcFee (n - 1) newFee cUtxoIndex parms skel

-- | This funcion is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee :: Pl.Params -> Pl.UTxO Pl.EmulatorEra -> Map Pl.DatumHash Pl.Datum -> TxSkel -> Either MockChainError Integer
estimateTxSkelFee params utxo managedData skel = do
  txBodyContent <- left MCEGenerationError $ generateTxBodyContent withDatums params managedData skel
  let nkeys = C.estimateTransactionKeyWitnessCount txBodyContent
  txBody <-
    left
      ( \case
          Left err -> MCEValidationError err
          Right err -> MCEGenerationError (ToCardanoError "makeTransactionBody" err)
      )
      $ Pl.makeTransactionBody params utxo (Pl.CardanoBuildTx txBodyContent)
  case C.evaluateTransactionFee (Pl.pProtocolParams params) txBody nkeys 0 of
    C.Lovelace fee -> pure fee

-- | Calculates the collateral for a transaction
calcCollateral :: (Monad m) => Wallet -> MockChainT m (Set SpendableOut)
calcCollateral w = do
  souts <- pkUtxosSuchThat @Void (walletPKHash w) (noDatum .&& valueSat hasOnlyAda)
  when (null souts) $
    throwError MCENoSuitableCollateral
  -- TODO We only keep one element of the list because we are limited on
  -- how many collateral inputs a transaction can have. Should this be
  -- investigated further for a better approach?
  return $ Set.fromList $ take 1 (fst <$> souts)

balanceTxFromAux :: (Monad m) => BalanceOutputPolicy -> BalanceStage -> Pl.PubKeyHash -> TxSkel -> MockChainT m TxSkel
balanceTxFromAux utxoPolicy stage balancePK txskel = do
  bres <- calcBalanceTx stage balancePK txskel
  case applyBalanceTx balancePK bres txskel of
    Just txskel' -> return txskel'
    Nothing -> throwError $ MCEUnbalanceable (show bres) stage txskel

data BalanceTxRes = BalanceTxRes
  { -- | Inputs that need to be added in order to cover the value in the
    -- transaction outputs
    newInputs :: Set SpendableOut,
    -- | The 'newInputs' will add _at least_ the missing value to cover the
    -- outputs, this is the difference @inputvalue with 'newInputs' -
    -- outputValue@. This value must be nonnegative in every asset class.
    returnValue :: Pl.Value,
    -- | Some additional UTxOs that could be used as extra inputs. These all
    -- belong to the same wallet that was passed to 'calcBalanceTx' as an
    -- argument, and are sorted in decreasing order of their Ada value.
    availableUtxos :: [SpendableOut]
  }
  deriving (Eq, Show)

-- | Calculate the changes needed to balance a transaction with money from a
-- given wallet.  Every transaction that is sent to the chain must be balanced,
-- that is: @inputs + mints == outputs + fee + burns@.
calcBalanceTx :: Monad m => BalanceStage -> Pl.PubKeyHash -> TxSkel -> MockChainT m BalanceTxRes
calcBalanceTx balanceStage balancePK skel = do
  -- Get all Utxos that belong to the given wallet, and that are not yet being
  -- consumed on the transaction.
  --
  -- These UTxOs are sorted in decreasing order of their Ada value, which will
  -- make 'selectNewInputs' will more likely select additional inputs that
  -- contain a lot of Ada. The hope behind this heuristic is that it'll
  -- therefore become less likely for the 'returnValue' to be less than the
  -- minimum Ada amount required for each output. See this comment for context:
  -- https://github.com/tweag/plutus-libs/issues/71#issuecomment-1016406041
  candidateUtxos <-
    sortBy (flip compare `on` Pl.fromValue . sOutValue)
      . filter (`notElem` inUtxos)
      <$> pkUtxos balancePK
  case selectNewInputs candidateUtxos Set.empty initialExcess missingValue of
    Nothing ->
      throwError $
        MCEUnbalanceable
          ("The wallet " ++ show balancePK ++ " does not own enough funds to pay for balancing.")
          balanceStage
          skel
    Just bTxRes -> return bTxRes
  where
    inUtxos = toListOf consumedOutputsF skel -- The Utxos consumed by the given transaction
    inValue = txSkelInputValue skel -- inputs + mints
    outValue = txSkelOutputValue skel -- outputs + fee + burns
    difference = outValue <> Pl.negate inValue

    -- This is what must still be paid by 'wallet'
    missingValue = positivePart difference

    -- This is the part of the input that already excesses the ouput. We'll
    -- have to pay this to 'wallet' in any case. (Note that 'negativePart' is a
    -- function that returns a stricly (componentwise) positive 'Pl.Value'!)
    initialExcess = negativePart difference

    selectNewInputs ::
      [SpendableOut] ->
      Set SpendableOut ->
      Pl.Value ->
      Pl.Value ->
      Maybe BalanceTxRes
    selectNewInputs available chosen excess missing =
      case view flattenValueI missing of
        [] -> Just $ BalanceTxRes chosen excess available
        (ac, _) : _ ->
          -- Find the first UTxO belonging to the wallet that contains at least
          -- one token of the required asset class (The hope is that it'll
          -- contain at least @n@ such tokens, but we can't yet fail if there are
          -- fewer; we might need to add several UTxOs):
          case findIndex ((`Value.geq` Value.assetClassValue ac 1) . sOutValue) available of
            Nothing -> Nothing -- The wallet owns nothing of the required asset class. We can't balance with this wallet.
            Just i ->
              let (left, theChosenUtxo : right) = splitAt i available
                  available' = left ++ right
                  chosen' = chosen <> Set.singleton theChosenUtxo
                  theChosenValue = sOutValue theChosenUtxo
                  theChosenDifference = missing <> Pl.negate theChosenValue
                  excess' = excess <> negativePart theChosenDifference
                  missing' = positivePart theChosenDifference
               in -- A remark on why the following line should not lead to an
                  -- infinite recursion: The value described by @missing'@ is
                  -- strictly smaller than the value described by @missing@,
                  -- because there was at least one token of the asset class @ac@
                  -- in @theChosenValue@.
                  selectNewInputs available' chosen' excess' missing'

-- | Once we calculated what is needed to balance a transaction @skel@, we still
-- need to apply those changes to @skel@. Because of the 'Pl.minAdaTxOut'
-- constraint, this might not be possible: imagine the leftover is less than
-- 'Pl.minAdaTxOut', but the transaction has no output addressed to the
-- balancing wallet. If we just create a new ouput for the balancing wallet and
-- place the leftover there, the resulting transaction will fail to validate
-- with "LessThanMinAdaPerUTxO" error. Instead, we need to consume yet another
-- UTxO belonging to the wallet to then create the output with the proper
-- leftover. If the wallet has no UTxO, then there's no way to balance this
-- transaction.
applyBalanceTx :: Pl.PubKeyHash -> BalanceTxRes -> TxSkel -> Maybe TxSkel
applyBalanceTx balancePK (BalanceTxRes newInputs returnValue availableUtxos) skel = do
  -- Here we'll try a few things, in order, until one of them succeeds:
  --
  -- 1. If allowed by the balanceOutputPolicy, pick out the best possible output
  --    to adjust and adjust it as long as it remains with more than
  --    'Pl.minAdaTxOut'. No need for additional inputs apart from the
  --    @newInputs@. The "best possible" here means the most valuable ada-only
  --    output without any datum that will be paid to the given wallet. If the
  --    policy doesn't allow modifying an existing utxo or no such utxo exists,
  --    we move on to the next option;
  --
  -- 2. If the leftover is more than 'Pl.minAdaTxOut' and (1) wasn't possible,
  --    create a new output to return leftover. No need for additional inputs
  --    besides the @newInputs@.
  --
  -- 3. Attempt to consume other possible utxos from 'w' in order to combine
  --    them and return the leftover.

  -- TODO: Mustn't every UTxO belongign to the wallet contain at least minAda?
  -- In that case, we could forget about adding several additional inputs. If
  -- one isn't enough, there's nothing we can do, no?
  let outs = txSkelOuts skel
      ins = txSkelIns skel
  (newIns, newOuts) <-
    case findIndex
      ( \out ->
          Just balancePK == out ^? recipientPubKeyHashAT
            && Value.isAdaOnlyValue (outValue out)
            && isNothing (out ^? txSkelOutDatumAT)
      )
      outs of
      Just i ->
        let (left, bestOutput : right) = splitAt i outs
         in case balanceOutputPolicy $ txSkelOpts skel of
              AdjustExistingOutput ->
                let bestOutputValue = outValue bestOutput
                    adjustedValue = bestOutputValue <> returnValue
                 in if adjustedValue `Value.geq` Pl.toValue Pl.minAdaTxOut
                      then
                        Just -- (1)
                          ( ins <> Map.fromSet (const SpendsPK) newInputs,
                            left ++ (bestOutput & outValueL .~ adjustedValue) : right
                          )
                      else tryAdditionalInputs ins outs availableUtxos returnValue
              DontAdjustExistingOutput -> tryAdditionalOutput ins outs
      Nothing ->
        -- There's no "best possible transaction output" in the sense described
        -- above.
        tryAdditionalOutput ins outs
  return skel {txSkelIns = newIns, txSkelOuts = newOuts}
  where
    tryAdditionalOutput ::
      Map SpendableOut TxSkelIn ->
      [TxSkelOut] ->
      Maybe (Map SpendableOut TxSkelIn, [TxSkelOut])
    tryAdditionalOutput ins outs =
      if Pl.fromValue returnValue >= Pl.minAdaTxOut
        then
          Just -- (2)
            ( ins <> Map.fromSet (const SpendsPK) newInputs,
              outs ++ [PaysPK balancePK Nothing (Nothing @()) returnValue]
            )
        else tryAdditionalInputs ins outs availableUtxos returnValue

    tryAdditionalInputs ::
      Map SpendableOut TxSkelIn ->
      [TxSkelOut] ->
      [SpendableOut] ->
      Pl.Value ->
      Maybe (Map SpendableOut TxSkelIn, [TxSkelOut])
    tryAdditionalInputs ins outs available return =
      case available of
        [] -> Nothing
        additionalUtxo : newAvailable ->
          let additionalValue = sOutValue additionalUtxo
              newReturn = additionalValue <> return
              newIns =
                ins
                  <> Map.fromSet (const SpendsPK) newInputs
                  <> Map.singleton additionalUtxo SpendsPK
              newOuts = outs ++ [PaysPK balancePK Nothing (Nothing @()) newReturn]
           in if newReturn `Value.geq` Pl.toValue Pl.minAdaTxOut
                then Just (newIns, newOuts) -- (3)
                else tryAdditionalInputs newIns newOuts newAvailable newReturn
