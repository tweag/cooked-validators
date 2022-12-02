{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Direct where

import qualified Cardano.Api as Api
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.MockChain.Misc
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.GenerateTx
import qualified Cooked.MockChain.Monad.GenerateTx as GenTx
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Bifunctor (Bifunctor (first, second))
import Data.Default
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Fee as Pl
import Ledger.Orphans ()
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Tx.CardanoAPI as Pl
import qualified Ledger.Tx.CardanoAPI.Internal as Pl
import qualified Ledger.Validation as Pl
import qualified Ledger.Value as Pl (flattenValue)
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
  UtxoState . M.fromListWith (<>) . map (uncurry go1) . M.toList . Pl.getIndex . mcstIndex $ s
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
  | MCETxError GenTx.GenerateTxError
  | -- | MCEUnbalanceable BalanceStage Pl.Tx BalanceTxRes
    MCENoSuitableCollateral
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
  validateTxSkel = validateTxSkel'

  txOutByRef outref = gets (M.lookup outref . Pl.getIndex . mcstIndex)

  ownPaymentPubKeyHash = asks (walletPKHash . NE.head . mceSigners)

  utxosSuchThat = utxosSuchThat'

  utxosSuchThisAndThat = utxosSuchThisAndThat'

  datumFromHash dh = M.lookup dh <$> gets mcstDatums

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

-- | Make sure that every transaction output contains the required minimum Ada
-- amount, and that the transaction inputs contain at least as much value as the
-- outputs.
--
-- This is extremely simplistic for now, only chosing the first output of the
-- first signer that is greater than the deficit.
adjustTxSkel :: Monad m => TxSkel -> MockChainT m TxSkel
adjustTxSkel skel =
  let deficit = removeNegativeEntries $ txSkelOutValue skel <> Pl.negate (txSkelInValue skel)
   in if deficit `Value.geq` mempty
        then do
          firstSigner NE.:| _ <- askSigners
          spOut : _ <- pkUtxosSuchThatValue (walletPKHash firstSigner) (`Value.geq` deficit)
          return $ over txSkelIns (\ins -> ins <> Set.singleton (SpendsPK spOut)) skel
        else return skel

generateCardanoBuildTx :: Monad m => TxSkel -> MockChainT m Pl.CardanoBuildTx
generateCardanoBuildTx skel = do
  slotCfg <- asks $ Pl.pSlotConfig . mceParams
  managedData <- gets mcstDatums
  parms <- asks mceParams
  let Right (Pl.UnbalancedEmulatorTx tx _ _) = generateUnbalTx slotCfg managedData skel
      signerPaymentPKHs = Pl.PaymentPubKeyHash <$> toListOf (txSkelRequiredSigners % folded) skel
  case Pl.toCardanoTxBodyContent parms signerPaymentPKHs tx of
    Left err -> fail $ "in generateCardanoBuildTx, toCardanoTxBodyContent: " ++ show err
    Right cBuildTx -> return cBuildTx

validateTxSkel' :: Monad m => TxSkel -> MockChainT m Pl.CardanoTx
validateTxSkel' skel = do
  adjSkel <- adjustTxSkel skel
  cBuildTx <- generateCardanoBuildTx adjSkel
  parms <- asks mceParams
  let changeAddress = walletAddress $ wallet 1 -- TODO
      txSkelInputs = either (error . show) id . Pl.fromPlutusIndex $ undefined
  case Pl.makeAutoBalancedTransaction parms txSkelInputs cBuildTx changeAddress of
    Left err -> fail $ "in validateTxSkel', makeAutoBalancedTransaction: " ++ show err
    Right cApiTx -> do
      now <- currentSlot
      pUtxo <- gets mcstIndex
      let cApiUtxo = either (error . show) id $ Pl.fromPlutusIndex pUtxo
      case Pl.hasValidationErrors parms (fromIntegral now) cApiUtxo cApiTx of
        Nothing -> undefined -- insert the transaction
        Just err@(Pl.Phase1, _) -> fail $ "in validateTxSkel', hasValidationErrors:" ++ show err
        Just (Pl.Phase2, _) -> undefined -- insert only the collateral (has it already been set?) with Pl.insertCollateral

-- balancing and fees
-- validate

utxosSuchThisAndThat' ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  (Pl.Address -> Bool) ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThisAndThat' addrPred datumPred = do
  ix <- gets (Pl.getIndex . mcstIndex)
  let ix' = M.filter (addrPred . Pl.txOutAddress) ix
  catMaybes
    <$> mapM
      (\(oref, out) -> (first (SpendableOut oref) <$>) <$> go oref out)
      (M.toList ix')
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
      Just <$> case dh `M.lookup` managedDatums of
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
