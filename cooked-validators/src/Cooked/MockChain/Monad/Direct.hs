{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Direct where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.MockChain.Monad
import Cooked.MockChain.UtxoPredicate
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Balance
import Cooked.Tx.Constraints
import Data.Bifunctor (Bifunctor (first, second))
import Data.Default
import Data.Foldable (asum)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Fee as Pl
import Ledger.Orphans ()
import qualified Ledger.Scripts as Pl
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Validation as Pl
import qualified Ledger.Value as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Lattice as PlutusTx
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
  UtxoState . M.fromListWith (<>) . map (uncurry go1) . M.toList . Pl.getIndex . mcstIndex $ s
  where
    go1 :: Pl.TxOutRef -> Pl.TxOut -> (Pl.Address, UtxoValueSet)
    go1 _ (Pl.TxOut addr val mdh) = do
      (addr, UtxoValueSet [(val, mdh >>= go2)])

    go2 :: Pl.DatumHash -> Maybe UtxoDatum
    go2 datumHash = do
      datumStr <- M.lookup datumHash (mcstStrDatums s)
      datum <- M.lookup datumHash (mcstDatums s)
      return $ UtxoDatum datum datumStr

-- | Slightly more concrete version of 'UtxoState', used to actually run the simulation.
--  We keep a map from datum hash to datum, then a map from txOutRef to datumhash
--  Additionally, we also keep a map from datum hash to the underlying value's "show" result,
--  in order to display the contents of the state to the user.
data MockChainSt = MockChainSt
  { mcstIndex :: Pl.UtxoIndex,
    mcstDatums :: M.Map Pl.DatumHash Pl.Datum,
    mcstStrDatums :: M.Map Pl.DatumHash String,
    mcstCurrentSlot :: Pl.Slot
  }
  deriving (Show, Eq)

instance Default Pl.Slot where
  def = Pl.Slot 0

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError
  = MCEValidationError Pl.ValidationErrorInPhase
  | MCETxError Pl.MkTxError
  | MCEUnbalanceable BalanceStage Pl.Tx BalanceTxRes
  | MCENoSuitableCollateral
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
mockChainSt0 = MockChainSt utxoIndex0 M.empty M.empty def

mockChainSt0From :: InitialDistribution -> MockChainSt
mockChainSt0From i0 = MockChainSt (utxoIndex0From i0) M.empty M.empty def

instance Default MockChainSt where
  def = mockChainSt0

utxoIndex0From :: InitialDistribution -> Pl.UtxoIndex
utxoIndex0From i0 = Pl.initialise [[Pl.Valid $ Pl.EmulatorTx $ initialTxFor i0]]

utxoIndex0 :: Pl.UtxoIndex
utxoIndex0 = utxoIndex0From def

-- ** Direct Interpretation of Operations

instance (Monad m) => MonadBlockChain (MockChainT m) where
  validateTxSkel skel = do
    (reqSigs, tx) <- generateTx' skel
    _ <- validateTx' reqSigs tx
    when (autoSlotIncrease $ txOpts skel) $ modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    return (Pl.EmulatorTx tx)

  txOutByRef outref = gets (M.lookup outref . Pl.getIndex . mcstIndex)

  ownPaymentPubKeyHash = asks (walletPKHash . NE.head . mceSigners)

  utxosSuchThat = utxosSuchThat'

  utxosSuchThisAndThat = utxosSuchThisAndThat'

  datumFromTxOut Pl.PublicKeyChainIndexTxOut {} = pure Nothing
  datumFromTxOut (Pl.ScriptChainIndexTxOut _ _ (Right d) _) = pure $ Just d
  -- datum is always present in the nominal case, guaranteed by chain-index
  datumFromTxOut (Pl.ScriptChainIndexTxOut _ _ (Left dh) _) =
    M.lookup dh <$> gets mcstDatums

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
  Pl.Tx ->
  (Pl.UtxoIndex, Maybe Pl.ValidationErrorInPhase)
runTransactionValidation s parms ix reqSigners signers tx =
  let -- Now we'll convert the emulator datastructures into their Cardano.API equivalents.
      -- This should not go wrong and if it does, its unrecoverable, so we stick with `error`
      -- to keep this function pure.
      cardanoIndex = either (error . show) id $ Pl.fromPlutusIndex parms ix
      cardanoTx = either (error . show) id $ Pl.fromPlutusTx parms cardanoIndex reqSigners tx

      txn = Pl.EmulatorTx $ Pl.CardanoApiEmulatorEraTx tx
      e1 = Pl.validateCardanoTx params s cardanoIndex txn

      -- Finally, we get to check that the Cardano.API equivalent of 'tx' has no validation errors
      e2 =
        Pl.hasValidationErrors
          parms
          (fromIntegral s)
          cardanoIndex
          (L.foldl' (flip txAddSignatureAPI) cardanoTx signers)

      -- Now we compute the new index
      e = e1 <|> e2
      idx' = case e of
        Just (Pl.Phase1, _) -> ix
        Just (Pl.Phase2, _) -> Pl.insertCollateral txn ix
        Nothing -> Pl.insert txn ix
   in (idx', e)

-- | Check 'validateTx' for details; we pass the list of required signatories since
-- that is only truly available from the unbalanced tx, so we bubble that up all the way here.
validateTx' :: (Monad m) => [Pl.PaymentPubKeyHash] -> Pl.Tx -> MockChainT m Pl.TxId
validateTx' reqSigs tx = do
  s <- currentSlot
  ix <- gets mcstIndex
  ps <- asks mceParams
  signers <- askSigners
  let (ix', status) = runTransactionValidation s ps ix reqSigs (NE.toList signers) tx
  -- case trace (show $ snd res) $ fst res of
  case status of
    Just err -> throwError (MCEValidationError err)
    Nothing -> do
      -- Validation succeeded; now we update the indexes and the managed datums.
      -- The new mcstIndex is just `ix'`; the new mcstDatums is computed by
      -- removing the datum hashes have been consumed and adding
      -- those that have been created in `tx`.
      let consumedIns = map Pl.txInRef $ S.toList (Pl.txInputs tx) ++ S.toList (Pl.txCollateral tx)
      consumedDHs <- catMaybes <$> mapM (fmap Pl.txOutDatumHash . outFromOutRef) consumedIns
      let consumedDHs' = M.fromList $ zip consumedDHs (repeat ())
      modify'
        ( \st ->
            st
              { mcstIndex = ix',
                mcstDatums = (mcstDatums st `M.difference` consumedDHs') `M.union` Pl.txData tx
              }
        )
      return $ Pl.txId tx

utxosSuchThisAndThat' ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  (Pl.Address -> Bool) ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThisAndThat' addrPred datumPred = do
  ix <- gets (Pl.getIndex . mcstIndex)
  let ix' = M.filter (addrPred . Pl.txOutAddress) ix
  mapMaybe (fmap assocl . rstr) <$> mapM (\(oref, out) -> (oref,) <$> go oref out) (M.toList ix')
  where
    go :: Pl.TxOutRef -> Pl.TxOut -> MockChainT m (Maybe (Pl.ChainIndexTxOut, Maybe a))
    go oref (Pl.TxOut oaddr@(Pl.Address ocred _) val mdatumH) = do
      -- We begin by attempting to lookup the given datum hash in our map of managed datums.
      managedDatums <- gets mcstDatums
      let mdatum = mdatumH >>= (`M.lookup` managedDatums)
      -- Now, depending on whether we're looking at a utxo that belongs to a pk or a script,
      -- there's a slight difference in treatment:
      case ocred of
        -- PubKey outputs are not required to have a datum, hence, we don't care if mdatum is Nothing.
        Pl.PubKeyCredential _ -> do
          let ma = mdatum >>= Pl.fromBuiltinData . Pl.getDatum
          if datumPred ma val
            then return . Just $ (Pl.PublicKeyChainIndexTxOut oaddr val, ma)
            else return Nothing
        -- Script addresses, on the other hand, /must/ have a datum present. Hence, we check that mdatum
        -- is a just. If this happens, it probably means there's a bug in cooked and we lost some datum.
        -- Therefore, we check a few different things in order to provide a better debugging experience.
        Pl.ScriptCredential (Pl.ValidatorHash vh) -> do
          datumH <- maybe (fail $ "ScriptCredential with no datum hash: " ++ show oref) return mdatumH
          datum <- maybe (fail $ "Unmanaged datum with hash: " ++ show datumH ++ " at: " ++ show oref) return mdatum
          a <-
            maybe
              (fail $ "Can't convert from builtin data at: " ++ show oref ++ "; are you sure this is the right type?")
              return
              (Pl.fromBuiltinData (Pl.getDatum datum))
          if datumPred (Just a) val
            then return . Just $ (Pl.ScriptChainIndexTxOut oaddr (Left $ Pl.ValidatorHash vh) (Right datum) val, Just a)
            else return Nothing

-- | Check 'utxosSuchThat' for details
utxosSuchThat' ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  Pl.Address ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThat' addr = utxosSuchThisAndThat' (== addr)

-- | Generates an unbalanced transaction from a skeleton; A
--  transaction is unbalanced whenever @inputs + mints != outputs + fees@.
--  In order to submit a transaction, it must be balanced, otherwise
--  we will see a @ValueNotPreserved@ error.
--
--  See "Cooked.Tx.Balance" for balancing capabilities or stick to
--  'generateTx', which generates /and/ balances a transaction.
generateUnbalTx :: TxSkel -> Either MockChainError Pl.UnbalancedTx
generateUnbalTx TxSkel {txConstraints} =
  let (lkups, constrs) = toLedgerConstraint @Constraints @Void (toConstraints txConstraints)
   in first MCETxError $ Pl.mkTx lkups constrs

myAdjustUnbalTx :: Pl.Params -> Pl.UnbalancedTx -> Pl.UnbalancedTx
myAdjustUnbalTx parms utx =
  case Pl.adjustUnbalancedTx parms utx of
    Left err -> error (show err)
    Right (_, res) -> res

-- | Check 'generateTx' for details
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
    applyTxOutConstraintOrder ocs tx =
      let Right tx' = Pl.unBalancedTxTx tx
          txOuts' = orderTxOutputs ocs . Pl.txOutputs $ tx'
       in tx {Pl.unBalancedTxTx = Right $ tx' {Pl.txOutputs = txOuts'}}

-- | Sets the 'Pl.txFee' and 'Pl.txValidRange' according to our environment. The transaction
-- fee gets set realistically, based on a fixpoint calculation taken from /plutus-apps/,
-- see https://github.com/input-output-hk/plutus-apps/blob/03ba6b7e8b9371adf352ffd53df8170633b6dffa/plutus-contract/src/Wallet/Emulator/Wallet.hs#L314
setFeeAndValidRange :: (Monad m) => BalanceOutputPolicy -> Wallet -> Pl.UnbalancedTx -> MockChainT m Pl.Tx
setFeeAndValidRange _ _ Pl.UnbalancedCardanoTx {} =
  error "Impossible: we have a CardanoBuildTx"
setFeeAndValidRange bPol w (Pl.UnbalancedEmulatorTx tx0 reqSigs0 uindex {- slotRange -}) = do
  utxos <- pkUtxos' (walletPKHash w)
  let requiredSigners = S.toList reqSigs0
  ps <- asks mceParams
  case Pl.fromPlutusIndex ps $ Pl.UtxoIndex $ uindex <> M.fromList utxos of
    Left err -> throwError $ FailWith $ "setFeeAndValidRange: " ++ show err
    Right cUtxoIndex -> do
      config <- slotConfig
      -- We start with a high startingFee, but theres a chance that 'w' doesn't have enough funds
      -- so we'll see an unbalanceable error; in that case, we switch to the minimum fee and try again.
      -- That feels very much like a hack, and it is. Maybe we should witch to starting with a small
      -- fee and then increasing, but that might require more iterations until its settled.
      -- For now, let's keep it just like the folks from plutus-apps did it.
      let startingFee = Pl.lovelaceValueOf 3000000
      let tx = tx0 {Pl.txValidRange = Pl.posixTimeRangeToContainedSlotRange config _slotRange}
      fee <-
        calcFee 5 startingFee requiredSigners cUtxoIndex ps tx
          `catchError` \case
            MCEUnbalanceable BalCalcFee _ _ -> calcFee 5 (Pl.minFee tx0) requiredSigners cUtxoIndex ps tx
            e -> throwError e
      return $ tx {Pl.txFee = fee}
  where
    -- Inspired by https://github.com/input-output-hk/plutus-apps/blob/03ba6b7e8b9371adf352ffd53df8170633b6dffa/plutus-contract/src/Wallet/Emulator/Wallet.hs#L314
    calcFee ::
      (Monad m) =>
      Int ->
      Pl.Value ->
      [Pl.PaymentPubKeyHash] ->
      Pl.UTxO Pl.EmulatorEra ->
      Pl.Params ->
      Pl.Tx ->
      MockChainT m Pl.Value
    calcFee n fee reqSigs cUtxoIndex parms tx = do
      let tx1 = tx {Pl.txFee = fee}
      attemptedTx <- balanceTxFromAux bPol BalCalcFee w tx1
      case Pl.estimateTransactionFee parms cUtxoIndex reqSigs attemptedTx of
        -- necessary to capture script failure for failed cases
        Left (Left err@(Pl.Phase2, Pl.ScriptFailure _)) -> throwError $ MCEValidationError err
        Left err -> throwError $ FailWith $ "calcFee: " ++ show err
        Right newFee
          | newFee == fee -> pure newFee -- reached fixpoint
          | n == 0 -> pure (newFee PlutusTx.\/ fee) -- maximum number of iterations
          | otherwise -> calcFee (n - 1) newFee reqSigs cUtxoIndex parms tx

balanceTxFrom ::
  (Monad m) =>
  BalanceOutputPolicy ->
  Bool ->
  Collateral ->
  Wallet ->
  Pl.UnbalancedTx ->
  MockChainT m ([Pl.PaymentPubKeyHash], Pl.Tx)
balanceTxFrom bPol skipBalancing col w ubtx = do
  let requiredSigners = S.toList (Pl.unBalancedTxRequiredSignatories ubtx)
  colTxIns <- calcCollateral w col
  let Right ubtx' = Pl.unBalancedTxTx ubtx
  tx <-
    setFeeAndValidRange bPol w $
      ubtx {Pl.unBalancedTxTx = Right $ ubtx' {Pl.txCollateral = colTxIns}}
  (requiredSigners,)
    <$> if skipBalancing
      then return tx
      else balanceTxFromAux bPol BalFinalizing w tx

-- | Calculates the collateral for a some transaction
calcCollateral :: (Monad m) => Wallet -> Collateral -> MockChainT m (S.Set Pl.TxIn)
calcCollateral w col = do
  orefs <- case col of
    -- We're given a specific utxo to use as collateral
    CollateralUtxos r -> return r
    -- We must pick them; we'll first select
    CollateralAuto -> do
      souts <- pkUtxosSuchThat @Void (walletPKHash w) (noDatum .&& valueSat hasOnlyAda)
      when (null souts) $
        throwError MCENoSuitableCollateral
      return $ (: []) $ fst $ fst $ head souts
  let txIns = map (`Pl.TxIn` Just Pl.ConsumePublicKeyAddress) orefs
  return $ S.fromList txIns

balanceTxFromAux :: (Monad m) => BalanceOutputPolicy -> BalanceStage -> Wallet -> Pl.Tx -> MockChainT m Pl.Tx
balanceTxFromAux utxoPolicy stage w tx = do
  bres <- calcBalanceTx w tx
  case applyBalanceTx utxoPolicy w bres tx of
    Just tx' -> return tx'
    Nothing -> throwError $ MCEUnbalanceable stage tx bres

data BalanceTxRes = BalanceTxRes
  { newInputs :: [Pl.TxOutRef],
    returnValue :: Pl.Value,
    remainderUtxos :: [(Pl.TxOutRef, Pl.TxOut)]
  }
  deriving (Eq, Show)

-- | Calculate the changes needed to balance a transaction with money from a given wallet.
-- Every transaction that is sent to the chain must be balanced, that is: @inputs + mint == outputs + fee@.
calcBalanceTx :: (Monad m) => Wallet -> Pl.Tx -> MockChainT m BalanceTxRes
calcBalanceTx w tx = do
  -- We start by gathering all the inputs and summing it
  lhsInputs <- mapM (outFromOutRef . Pl.txInRef) (S.toList (Pl.txInputs tx))
  let lhs = mappend (mconcat $ map Pl.txOutValue lhsInputs) (Pl.txMint tx)
  let rhs = mappend (mconcat $ map Pl.txOutValue $ Pl.txOutputs tx) (Pl.txFee tx)
  let wPKH = walletPKHash w
  let usedInTxIns = S.map Pl.txInRef (Pl.txInputs tx)
  allUtxos <- pkUtxos' wPKH
  -- It is important that we only consider utxos that have not been spent in the transaction as "available"
  let availableUtxos = filter ((`S.notMember` usedInTxIns) . fst) allUtxos
  let (usedUTxOs, leftOver, excess) = balanceWithUTxOs (rhs Pl.- lhs) availableUtxos
  return $
    BalanceTxRes
      { -- Now, we will add the necessary utxos to the transaction,
        newInputs = usedUTxOs,
        -- Pay to wPKH whatever is leftOver from newTxIns and whatever was excessive to begin with
        returnValue = leftOver <> excess,
        -- We also return the remainder utxos that could still be used in case
        -- we can't 'applyBalanceTx' this 'BalanceTxRes'.
        remainderUtxos = filter ((`L.notElem` usedUTxOs) . fst) availableUtxos
      }

-- | Once we calculated what is needed to balance a transaction @tx@, we still need to
-- apply those changes to @tx@. Because of the 'Ledger.minAdaTxOut' constraint, this
-- might not be possible: imagine the leftover is less than 'Ledger.minAdaTxOut', but
-- the transaction has no output addressed to the sending wallet. If we just
-- create a new ouput for @w@ and place the leftover there the resulting tx will fail to validate
-- with "LessThanMinAdaPerUTxO" error. Instead, we need to consume yet another UTxO belonging to @w@ to
-- then create the output with the proper leftover. If @w@ has no UTxO, then there's no
-- way to balance this transaction.
applyBalanceTx :: BalanceOutputPolicy -> Wallet -> BalanceTxRes -> Pl.Tx -> Maybe Pl.Tx
applyBalanceTx utxoPolicy w (BalanceTxRes newTxIns leftover remainders) tx = do
  -- Here we'll try a few things, in order, until one of them succeeds:
  --   1. If allowed by the utxoPolicy, pick out the best possible output to adjust and adjust it as long as it remains with
  --      more than 'Pl.minAdaTxOut'. No need for additional inputs. The "best possible" here means the ada-only
  --      utxo with the most ada and without any datum hash. If the policy doesn't allow modifying an
  --      existing utxo or no such utxo exists, we move on to the next option;
  --   2. if the leftover is more than 'Pl.minAdaTxOut' and (1) wasn't possible, create a new output
  --      to return leftover. No need for additional inputs.
  --   3. Attempt to consume other possible utxos from 'w' in order to combine them
  --      and return the leftover.

  let adjustOutputs = case utxoPolicy of
        DontAdjustExistingOutput -> empty
        AdjustExistingOutput -> wOutsBest >>= fmap ([],) . adjustOutputValueAt (<> leftover) (Pl.txOutputs tx)

  (txInsDelta, txOuts') <-
    asum $
      [ adjustOutputs, -- 1.
        guard (isAtLeastMinAda leftover) >> return ([], Pl.txOutputs tx ++ [mkOutWithVal leftover]) -- 2.
      ]
        ++ map (fmap (second (Pl.txOutputs tx ++)) . consumeRemainder) (sortByMoreAda remainders) -- 3.
  let newTxIns' = S.fromList $ map (`Pl.TxIn` Just Pl.ConsumePublicKeyAddress) (newTxIns ++ txInsDelta)
  return $
    tx
      { Pl.txInputs = Pl.txInputs tx <> newTxIns',
        Pl.txOutputs = txOuts'
      }
  where
    wPKH = walletPKHash w
    mkOutWithVal v = Pl.TxOut (Pl.Address (Pl.PubKeyCredential wPKH) Nothing) v Nothing

    -- The best output to attempt and modify, if any, is the one with the most ada,
    -- which is at the head of wOutsIxSorted:
    wOutsBest = fst <$> L.uncons wOutsIxSorted

    -- The indexes of outputs belonging to w sorted by amount of ada.
    wOutsIxSorted :: [Int]
    wOutsIxSorted =
      map fst $
        sortByMoreAda $
          filter ((== Just wPKH) . onlyAdaPkTxOut . snd) $
            zip [0 ..] (Pl.txOutputs tx)

    sortByMoreAda :: [(a, Pl.TxOut)] -> [(a, Pl.TxOut)]
    sortByMoreAda = L.sortBy (flip compare `on` (adaVal . Pl.txOutValue . snd))

    adaVal :: Pl.Value -> Integer
    adaVal = Pl.getLovelace . Pl.fromValue

    isAtLeastMinAda :: Pl.Value -> Bool
    isAtLeastMinAda v = adaVal v >= Pl.getLovelace Pl.minAdaTxOut

    adjustOutputValueAt :: (Pl.Value -> Pl.Value) -> [Pl.TxOut] -> Int -> Maybe [Pl.TxOut]
    adjustOutputValueAt f xs i =
      let (pref, Pl.TxOut addr val stak : rest) = L.splitAt i xs
          val' = f val
       in guard (isAtLeastMinAda val') >> return (pref ++ Pl.TxOut addr val' stak : rest)

    -- Given a list of available utxos; attept to consume them if they would enable the returning
    -- of the leftover.
    consumeRemainder :: (Pl.TxOutRef, Pl.TxOut) -> Maybe ([Pl.TxOutRef], [Pl.TxOut])
    consumeRemainder (remRef, remOut) =
      let v = leftover <> Pl.txOutValue remOut
       in guard (isAtLeastMinAda v) >> return ([remRef], [mkOutWithVal v])

-- * Utilities

-- | returns public key hash when txout contains only ada tokens and that no datum hash is specified.
onlyAdaPkTxOut :: Pl.TxOut -> Maybe Pl.PubKeyHash
onlyAdaPkTxOut (Pl.TxOut (Pl.Address (Pl.PubKeyCredential pkh) _) v Nothing) =
  case Pl.flattenValue v of
    [(cs, tn, _)] | cs == Pl.adaSymbol && tn == Pl.adaToken -> Just pkh
    _ -> Nothing
onlyAdaPkTxOut _ = Nothing

addressIsPK :: Pl.Address -> Maybe Pl.PubKeyHash
addressIsPK addr = case Pl.addressCredential addr of
  Pl.PubKeyCredential pkh -> Just pkh
  _ -> Nothing

rstr :: (Monad m) => (a, m b) -> m (a, b)
rstr (a, mb) = (a,) <$> mb

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (a, (b, c)) = ((a, b), c)
