{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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
import Cooked.MockChain.Monad.GenerateTx (GenerateTxError, generateTxBodyContent)
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
import GHC.Stack
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Fee as Pl
import Ledger.Orphans ()
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Tx.CardanoAPI as Pl hiding (makeTransactionBody)
import qualified Ledger.Validation as Pl
import qualified Ledger.Value as Pl (flattenValue)
import qualified Ledger.Value as Value
import Optics.Core
import qualified Plutus.V2.Ledger.Tx as Pl2
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl
import qualified PlutusTx.Prelude as PlutusTx

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
  validateTxSkel skel = do
    -- TODO balance the TxSkel
    params <- params
    managedData <- gets mcstDatums
    case generateCardanoBuildTx params managedData skel of
      Left err -> throwError $ MCEGenerationError err
      Right cardanoBuildTx -> do
        someCardanoTx <- validateTx' [] (txSkelData skel) cardanoBuildTx
        when (autoSlotIncrease $ skel ^. txSkelOpts) $ modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
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

  params = asks mceParams

  localParams f = local (\e -> e {mceParams = f (mceParams e)})

-- | This validates a given 'Pl.Tx' in its proper context; this is a very tricky thing to do. We're basing
--  ourselves off from how /plutus-apps/ is doing it.
--
--  TL;DR: we need to use "Ledger.Index" to compute the new 'Pl.UtxoIndex', but we neet to
--  rely on "Ledger.Validation" to run the validation akin to how it happens on-chain, with
--  proper checks on transactions fees and signatures.
--
--  For more details, check the following relevant pointers:
--
--  1. https://github.com/tweag/plutus-libs/issues/92
--  2. https://github.com/input-output-hk/plutus-apps/blob/03ba6b7e8b9371adf352ffd53df8170633b6dffa/plutus-ledger/src/Ledger/Tx.hs#L126
--  3. https://github.com/input-output-hk/plutus-apps/blob/03ba6b7e8b9371adf352ffd53df8170633b6dffa/plutus-contract/src/Wallet/Emulator/Chain.hs#L209
--  4. https://github.com/input-output-hk/plutus-apps/blob/03ba6b7e8b9371adf352ffd53df8170633b6dffa/plutus-contract/src/Wallet/Emulator/Wallet.hs#L314
--
-- Finally; because 'Pl.fromPlutusTx' doesn't preserve signatures, we need the list of signers
-- around to re-sign the transaction.
runTransactionValidation ::
  Pl.Slot ->
  Pl.Params ->
  Pl.UtxoIndex ->
  -- | List of required signers
  [Pl.PaymentPubKeyHash] ->
  -- | List of signers
  [Wallet] ->
  Pl.CardanoBuildTx ->
  (Pl.UtxoIndex, Maybe Pl.ValidationErrorInPhase, Pl.SomeCardanoApiTx)
runTransactionValidation s parms ix reqSigners signers tx =
  let -- Now we'll convert the emulator datastructures into their Cardano.API equivalents.
      -- This should not go wrong and if it does, its unrecoverable, so we stick with `error`
      -- to keep this function pure.
      cardanoIndex = either (error . show) id $ Pl.fromPlutusIndex ix
      cardanoTx =
        either (error . ("Error building cardano tx: " <>) . show) id $
          Pl.makeAutoBalancedTransaction parms cardanoIndex tx (walletAddress $ head signers)
      cardanoTxSigned = L.foldl' (flip txAddSignatureAPI) cardanoTx signers

      txn = Pl.CardanoApiTx $ Pl.CardanoApiEmulatorEraTx cardanoTxSigned
      e = Pl.validateCardanoTx parms s cardanoIndex txn

      -- Now we compute the new index
      idx' = case e of
        Just (Pl.Phase1, _) -> ix
        Just (Pl.Phase2, _) -> Pl.insertCollateral txn ix
        Nothing -> Pl.insert txn ix
   in (idx', e, Pl.CardanoApiEmulatorEraTx cardanoTxSigned)

-- | Check 'validateTx' for details; we pass the list of required signatories since
-- that is only truly available from the unbalanced tx, so we bubble that up all the way here.
validateTx' :: (Monad m) => [Pl.PaymentPubKeyHash] -> Map Pl.DatumHash Pl.Datum -> Pl.CardanoBuildTx -> MockChainT m Pl.SomeCardanoApiTx
validateTx' reqSigs txData tx = do
  s <- currentSlot
  ix <- gets mcstIndex
  ps <- asks mceParams
  signers <- askSigners
  let (ix', status, someCardanoTx) = runTransactionValidation s ps ix reqSigs (NE.toList signers) tx
  -- case trace (show $ snd res) $ fst res of
  case status of
    Just err -> throwError (MCEValidationError err)
    Nothing -> do
      -- Validation succeeded; now we update the indexes and the managed datums.
      -- The new mcstIndex is just `ix'`; the new mcstDatums is computed by
      -- removing the datum hashes have been consumed and adding
      -- those that have been created in `tx`
      modify'
        ( \st ->
            st
              { mcstIndex = ix',
                mcstDatums = mcstDatums st `Map.union` txData -- TODO: remove the consumed datum hashes
              }
        )
  return someCardanoTx

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

myAdjustUnbalTx :: Pl.Params -> Pl.UnbalancedTx -> Pl.UnbalancedTx
myAdjustUnbalTx parms utx =
  case Pl.adjustUnbalancedTx parms utx of
    Left err -> error (show err)
    Right (_, res) -> res

errorExpectedEmulatorTx :: HasCallStack => a
errorExpectedEmulatorTx = error "expected emulator tx, got cardano tx"

-- -- | Check 'generateTx' for details
{-
generateTx' :: (Monad m) => TxSkel -> MockChainT m ([Pl.PaymentPubKeyHash], Pl.Tx)
generateTx' skel@(TxSkel _ _ constraintsSpec) = do
  modify $ updateDatumStr skel
  signers <- askSigners
  slotCfg <- slotConfig
  let parms = def {Pl.pSlotConfig = slotCfg}
  case generateUnbalTx skel of
    Left err -> throwError err
    Right ubtx -> do
      let adjust = if adjustUnbalTx opts then myAdjustUnbalTx parms else id
      let (_ :=>: outputConstraints) = toConstraints constraintsSpec
      let reorderedUbtx =
            if forceOutputOrdering opts
              then applyTxOutConstraintOrder outputConstraints ubtx
              else ubtx
      -- optionally apply a transformation before balancing
      let modifiedUbtx = applyRawModOnUnbalancedTx (unsafeModTx opts) reorderedUbtx
      (reqSigs, balancedTx) <- balanceTxFrom (balanceOutputPolicy opts) (not $ balance opts) (collateral opts) (NE.head signers) (adjust modifiedUbtx)
      return . (reqSigs,) $
        foldl
          (flip txAddSignature)
          -- optionally apply a transformation to a balanced tx before sending it in.
          (applyRawModOnBalancedTx (unsafeModTx opts) balancedTx)
          (NE.toList signers)
  where
    opts = txOpts skel

    -- Update the map of pretty printed representations in the mock chain state
    updateDatumStr :: TxSkel -> MockChainSt -> MockChainSt
    updateDatumStr TxSkel {txConstraints} st@MockChainSt {mcstStrDatums} =
      st
        { mcstStrDatums =
            M.union mcstStrDatums . extractDatumStr . toConstraints $ txConstraints
        }

    -- Order outputs according to the order of output constraints
    applyTxOutConstraintOrder :: [OutConstraint] -> Pl.UnbalancedTx -> Pl.UnbalancedTx
    applyTxOutConstraintOrder ocs (Pl.UnbalancedEmulatorTx tx reqs ui) =
      Pl.UnbalancedEmulatorTx tx {Pl.txOutputs = txOuts'} reqs ui
      where
        txOuts' = orderTxOutputs ocs . Pl.txOutputs $ tx
    applyTxOutConstraintOrder ocs Pl.UnbalancedCardanoTx {} = errorExpectedEmulatorTx
 -}

-- | Sets the '_txSkelFee' according to our environment. The transaction fee
-- gets set realistically, based on a fixpoint calculation taken from
-- /plutus-apps/.
setFee :: (Monad m) => Wallet -> TxSkel -> MockChainT m TxSkel
setFee wallet skel = do
  return undefined
  where
    -- utxos <- pkUtxos' (walletPKHash w)
    -- let requiredSigners = S.toList reqSigs0
    -- ps <- asks mceParams
    -- case Pl.fromPlutusIndex $ Pl.UtxoIndex $ uindex <> Map.fromList utxos of
    --   Left err -> throwError $ FailWith $ "setFeeAndValidRange: " ++ show err
    --   Right cUtxoIndex -> do
    --     -- We start with a high startingFee, but theres a chance that 'w' doesn't have enough funds
    --     -- so we'll see an unbalanceable error; in that case, we switch to the minimum fee and try again.
    --     -- That feels very much like a hack, and it is. Maybe we should witch to starting with a small
    --     -- fee and then increasing, but that might require more iterations until its settled.
    --     -- For now, let's keep it just like the folks from plutus-apps did it.
    --     let startingFee = Pl.lovelaceValueOf 3000000
    --     fee <-
    --       calcFee 5 startingFee requiredSigners cUtxoIndex ps tx
    --         `catchError` \case
    --           MCEUnbalanceable BalCalcFee _ _ -> calcFee 5 Pl.minFee tx requiredSigners cUtxoIndex ps tx
    --           e -> throwError e
    --     return $ tx {Pl.txFee = fee}

    -- Inspired by https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-contract/src/Wallet/Emulator/Wallet.hs#L329
    calcFee ::
      (Monad m) =>
      Int ->
      Pl.Value ->
      Pl.UTxO Pl.EmulatorEra ->
      Pl.Params ->
      TxSkel ->
      MockChainT m Pl.Value
    calcFee n fee cUtxoIndex parms skel = do
      let skelWithFee = skel & txSkelFee .~ fee
      attemptedSkel <- balanceTxFromAux bPol BalCalcFee wallet skelWithFee
      case estimateTxSkelFee parms cUtxoIndex attemptedSkel of
        -- necessary to capture script failure for failed cases
        Left (Left err@(Pl.Phase2, Pl.ScriptFailure _)) -> throwError $ MCEValidationError err
        Left err -> throwError $ FailWith $ "calcFee: " ++ show err
        Right newFee
          | newFee == fee -> pure newFee -- reached fixpoint
          | n == 0 -> pure (newFee PlutusTx.\/ fee) -- maximum number of iterations
          | otherwise -> calcFee (n - 1) newFee reqSigs cUtxoIndex parms skel

-- | This funcion is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee :: Pl.Params -> Pl.UTxO Pl.EmulatorEra -> Map Pl.DatumHash Pl.Datum -> TxSkel -> Either MockChainError Pl.Value
estimateTxSkelFee params utxo managedData skel = do
  txBodyContent <- undefined $ generateTxBodyContent params managedData skel
  let nkeys = C.estimateTransactionKeyWitnessCount (Pl.getCardanoBuildTx txBodyContent)
  txBody <- left undefined $ Pl.makeTransactionBody params utxo txBodyContent
  case C.evaluateTransactionFee (Pl.pProtocolParams params) txBody nkeys 0 of
    C.Lovelace fee -> pure $ Pl.lovelaceValueOf fee

balanceTxFrom ::
  (Monad m) =>
  BalanceOutputPolicy ->
  Bool ->
  Collateral ->
  Wallet ->
  Pl.UnbalancedTx ->
  MockChainT m ([Pl.PaymentPubKeyHash], Pl.Tx)
balanceTxFrom = undefined

-- balanceTxFrom bPol skipBalancing col w (Pl.UnbalancedEmulatorTx ubtx' reqs ui) = do
--   let requiredSigners = S.toList reqs
--   colTxIns <- calcCollateral w col
--   tx <-
--     setFeeAndValidRange bPol w $ Pl.UnbalancedEmulatorTx ubtx' {Pl.txCollateral = colTxIns} reqs ui
--   (requiredSigners,)
--     <$> if skipBalancing
--       then return tx
--       else balanceTxFromAux bPol BalFinalizing w tx
-- balanceTxFrom _ _ _ _ Pl.UnbalancedCardanoTx {} = errorExpectedEmulatorTx

{-
-- | Calculates the collateral for a transaction
calcCollateral :: (Monad m) => Wallet -> Collateral -> MockChainT m (Set SpendableOut)
calcCollateral w col = do
  orefs <- case col of
    -- We're given a specific utxo to use as collateral
    CollateralUtxos r -> return r
    -- We must pick them; we'll first select
    CollateralAuto -> do
      souts <- pkUtxosSuchThat @Void (walletPKHash w) (noDatum .&& valueSat hasOnlyAda)
      when (null souts) $
        throwError MCENoSuitableCollateral
      return $ Set.singleton $ (^. spOutTxOutRef) $ fst $ head souts
  pure $ map (`Pl.TxInput` Pl.TxConsumePublicKeyAddress) $ Set.toList orefs
-}

{-
balanceTxFromAux :: (Monad m) => BalanceOutputPolicy -> BalanceStage -> Wallet -> Pl.Tx -> MockChainT m Pl.Tx
balanceTxFromAux utxoPolicy stage w tx = do
  bres <- calcBalanceTx w tx
  case applyBalanceTx utxoPolicy w bres tx of
    Just tx' -> return tx'
    Nothing -> throwError $ MCEUnbalanceable stage tx bres
    -}

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
calcBalanceTx :: Monad m => BalanceStage -> Wallet -> TxSkel -> MockChainT m BalanceTxRes
calcBalanceTx balanceStage wallet skel = do
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
    sortBy (flip compare `on` Pl.fromValue . (^. spOutValue))
      . filter (`notElem` inUtxos)
      <$> pkUtxos (walletPKHash wallet)
  case selectNewInputs candidateUtxos Set.empty initialExcess missingValue of
    Nothing ->
      throwError $
        MCEUnbalanceable
          ("The wallet " ++ show wallet ++ " does not own enough funds to pay for balancing.")
          balanceStage
          skel
    Just bTxRes -> return bTxRes
  where
    inUtxos = toListOf (txSkelIns % folded % input) skel -- The Utxos consumed by the given transaction
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
          case findIndex ((`Value.geq` Value.assetClassValue ac 1) . (^. spOutValue)) available of
            Nothing -> Nothing -- The wallet owns nothing of the required asset class. We can't balance with this wallet.
            Just i ->
              let (left, theChosenUtxo : right) = splitAt i available
                  available' = left ++ right
                  chosen' = chosen <> Set.singleton theChosenUtxo
                  theChosenValue = theChosenUtxo ^. spOutValue
                  theChosenDifference = missing <> Pl.negate theChosenValue
                  excess' = negativePart theChosenDifference
                  missing' = positivePart theChosenDifference
               in -- A remark on why the following line should not lead to an
                  -- infinite recursion: The value described by @missing'@ is
                  -- strictly smaller than the value described by @missing@,
                  -- because there was at least one token of the asset class @ac@
                  -- in @theChosenValue@.
                  selectNewInputs available' chosen' excess' missing'

-- | Once we calculated what is needed to balance a transaction @skel@, we still
-- need to apply those changes to @skel@. Because of the 'Ledger.minAdaTxOut'
-- constraint, this might not be possible: imagine the leftover is less than
-- 'Ledger.minAdaTxOut', but the transaction has no output addressed to the
-- sending wallet. If we just create a new ouput for the balancing wallet and
-- place the leftover there, the resulting transaction will fail to validate
-- with "LessThanMinAdaPerUTxO" error. Instead, we need to consume yet another
-- UTxO belonging to the wallet to then create the output with the proper leftover. If
-- the wallet has no UTxO, then there's no way to balance this transaction.
applyBalanceTx :: Wallet -> BalanceTxRes -> TxSkel -> Maybe TxSkel
applyBalanceTx wallet (BalanceTxRes newInputs returnValue availableUtxos) skel = do
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
  let outs = skel ^. txSkelOuts
      ins = skel ^. txSkelIns
  (newIns, newOuts) <-
    case findIndex
      ( \out ->
          Just (walletPKHash wallet) == out ^? recipientPubKeyHash
            && Value.isAdaOnlyValue (out ^. outValue)
            && isNothing (out ^? outConstraintDatum)
      )
      outs of
      Just i ->
        let (left, bestOutput : right) = splitAt i outs
         in case balanceOutputPolicy $ skel ^. txSkelOpts of
              AdjustExistingOutput ->
                let bestOutputValue = bestOutput ^. outValue
                    adjustedValue = bestOutputValue <> returnValue
                 in if adjustedValue `Value.geq` Pl.toValue Pl.minAdaTxOut
                      then
                        Just -- (1)
                          ( ins <> Set.map SpendsPK newInputs,
                            left ++ (bestOutput & outValue .~ adjustedValue) : right
                          )
                      else tryAdditionalInputs ins outs availableUtxos returnValue
              DontAdjustExistingOutput -> tryAdditionalOutput ins outs
      Nothing ->
        -- There's no "best possible transaction output" in the sense described
        -- above.
        tryAdditionalOutput ins outs
  return skel {_txSkelIns = newIns, _txSkelOuts = newOuts}
  where
    tryAdditionalOutput :: Set InConstraint -> [OutConstraint] -> Maybe (Set InConstraint, [OutConstraint])
    tryAdditionalOutput ins outs =
      if Pl.fromValue returnValue >= Pl.minAdaTxOut
        then
          Just -- (2)
            ( ins <> Set.map SpendsPK newInputs,
              outs ++ [PaysPK (walletPKHash wallet) Nothing (Nothing @()) returnValue]
            )
        else tryAdditionalInputs ins outs availableUtxos returnValue

    tryAdditionalInputs :: Set InConstraint -> [OutConstraint] -> [SpendableOut] -> Pl.Value -> Maybe (Set InConstraint, [OutConstraint])
    tryAdditionalInputs ins outs available return =
      case available of
        [] -> Nothing
        additionalUtxo : newAvailable ->
          let additionalValue = additionalUtxo ^. spOutValue
              newReturn = additionalValue <> return
              newIns = ins <> Set.map SpendsPK newInputs <> Set.singleton (SpendsPK additionalUtxo)
              newOuts = outs ++ [PaysPK (walletPKHash wallet) Nothing (Nothing @()) newReturn]
           in if newReturn `Value.geq` Pl.toValue Pl.minAdaTxOut
                then Just (newIns, newOuts) -- (3)
                else tryAdditionalInputs newIns newOuts newAvailable newReturn

-- * Utilities

-- | returns public key hash when txout contains only ada tokens and that no datum hash is specified.
onlyAdaPkTxOut :: Pl.TxOut -> Maybe Pl.PubKeyHash
onlyAdaPkTxOut (Pl.TxOut to) = case Pl.fromCardanoTxOutToPV2TxInfoTxOut to of
  Pl2.TxOut (Pl.Address (Pl.PubKeyCredential pkh) _) v Pl2.NoOutputDatum _ ->
    case Pl.flattenValue v of
      [(cs, tn, _)] | cs == Pl.adaSymbol && tn == Pl.adaToken -> Just pkh
      _ -> Nothing
  _ -> Nothing

addressIsPK :: Pl.Address -> Maybe Pl.PubKeyHash
addressIsPK addr = case Pl.addressCredential addr of
  Pl.PubKeyCredential pkh -> Just pkh
  _ -> Nothing

rstr :: (Monad m) => (a, m b) -> m (a, b)
rstr (a, mb) = (a,) <$> mb

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (a, (b, c)) = ((a, b), c)
