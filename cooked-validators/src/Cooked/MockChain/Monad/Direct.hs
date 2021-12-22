{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Direct where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.MockChain.Monad
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Balance
import Cooked.Tx.Constraints
import Data.Default
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import qualified Ledger as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Credential as Pl
import Ledger.Orphans ()
import qualified Ledger.TimeSlot as Pl
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
  UtxoState . M.fromListWith (++) . map (uncurry go1) . M.toList . Pl.getIndex . mcstIndex $ s
  where
    go1 :: Pl.TxOutRef -> Pl.TxOut -> (Pl.Address, [(Pl.Value, Maybe UtxoDatum)])
    go1 _ (Pl.TxOut addr val mdh) = do
      (addr, [(val, mdh >>= go2)])

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
  deriving (Show)

instance Default Pl.Slot where
  def = Pl.Slot 0

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError
  = MCEValidationError Pl.ValidationErrorInPhase
  | MCETxError Pl.MkTxError
  | FailWith String
  deriving (Show, Eq)

newtype MockChainEnv = MockChainEnv
  { mceSlotConfig :: Pl.SlotConfig
  }
  deriving (Show)

instance Default MockChainEnv where
  def = MockChainEnv def

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

-- | Executes a 'MockChainT' from some initial state.
runMockChainTRaw ::
  (Monad m) =>
  MockChainSt ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTRaw i0 =
  runExceptT
    . fmap (second mcstToUtxoState)
    . flip runStateT i0
    . flip runReaderT def
    . unMockChain

runMockChainRaw :: MockChainSt -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainRaw i0 = runIdentity . runMockChainTRaw i0

-- | Executes a 'MockChainT' from the cannonical initial state.
runMockChainT :: (Monad m) => MockChainT m a -> m (Either MockChainError (a, UtxoState))
runMockChainT = runMockChainTRaw mockChainSt0

runMockChain :: MockChain a -> Either MockChainError (a, UtxoState)
runMockChain = runIdentity . runMockChainT

-- | Executes a 'MockChainT' from an initial state with the given initial value distribution.
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom distribution = runMockChainTRaw mockChainSt0'
  where
    mockChainSt0' :: MockChainSt
    mockChainSt0' = MockChainSt utxoIndex0' M.empty M.empty def
    utxoIndex0' :: Pl.UtxoIndex
    utxoIndex0' = utxoIndex0From distribution

-- | Executes a 'MockChain' from an initial state with the given initial value distribution.
runMockChainFrom ::
  InitialDistribution -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainFrom distribution =
  runIdentity . runMockChainTFrom distribution

-- Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt utxoIndex0 M.empty M.empty def

instance Default MockChainSt where
  def = mockChainSt0

utxoIndex0From :: InitialDistribution -> Pl.UtxoIndex
utxoIndex0From i0 = Pl.initialise [[Pl.Valid $ initialTxFor i0]]

utxoIndex0 :: Pl.UtxoIndex
utxoIndex0 = utxoIndex0From initialDistribution

-- ** Direct Interpretation of Operations

instance (Monad m) => MonadMockChain (MockChainT m) where
  validateTxSkelOpts opts skel = do
    tx <- generateTx' opts skel
    txId <- validateTx' tx
    when (autoSlotIncrease opts) $ modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    return txId

  txOutByRef outref = gets (M.lookup outref . Pl.getIndex . mcstIndex)

  utxosSuchThat = utxosSuchThat'

  currentSlot = gets mcstCurrentSlot

  currentTime = asks (Pl.slotToBeginPOSIXTime . mceSlotConfig) <*> gets mcstCurrentSlot

  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

  awaitTime t = do
    sc <- asks mceSlotConfig
    s <- awaitSlot (1 + Pl.posixTimeToEnclosingSlot sc t)
    return $ Pl.slotToBeginPOSIXTime sc s

-- | Check 'validateTx' for details
validateTx' :: (Monad m) => Pl.Tx -> MockChainT m Pl.TxId
validateTx' tx = do
  s <- currentSlot
  ix <- gets mcstIndex
  slotCfg <- asks mceSlotConfig
  let res = Pl.runValidation (Pl.validateTransaction s tx) (Pl.ValidationCtx ix slotCfg)
  -- case trace (show $ snd res) $ fst res of
  case fst res of
    (Just err, _) -> throwError (MCEValidationError err)
    (Nothing, ix') -> do
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

-- | Check 'utxosSuchThat' for details
utxosSuchThat' ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  Pl.Address ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThat' addr datumPred = do
  ix <- gets (Pl.getIndex . mcstIndex)
  let ix' = M.filter ((== addr) . Pl.txOutAddress) ix
  mapMaybe (fmap assocl . rstr) <$> mapM (\(oref, out) -> (oref,) <$> go oref out) (M.toList ix')
  where
    go :: Pl.TxOutRef -> Pl.TxOut -> MockChainT m (Maybe (Pl.ChainIndexTxOut, Maybe a))
    go oref (Pl.TxOut oaddr val mdatumH) =
      case Pl.addressCredential oaddr of
        -- A PK credential has no datum; just check whether we want to select this output or not.
        Pl.PubKeyCredential _ ->
          if datumPred Nothing val
            then return . Just $ (Pl.PublicKeyChainIndexTxOut oaddr val, Nothing)
            else return Nothing
        -- A script credential, on the other hand, must have a datum. Hence, we'll go look on our map of
        -- managed datum for a relevant datum, try to convert it to a value of type @a@ then see
        -- if the user wants to select said output.
        Pl.ScriptCredential (Pl.ValidatorHash vh) -> do
          managedDatums <- gets mcstDatums
          datumH <- maybe (fail $ "ScriptCredential with no datum hash: " ++ show oref) return mdatumH
          datum <-
            maybe
              (fail $ "Unmanaged datum with hash: " ++ show datumH ++ " at: " ++ show oref)
              return
              $ M.lookup datumH managedDatums
          a <-
            maybe
              (fail $ "Can't convert from builtin data at: " ++ show oref ++ "; are you sure this is the right type?")
              return
              (Pl.fromBuiltinData (Pl.getDatum datum))
          if datumPred (Just a) val
            then return . Just $ (Pl.ScriptChainIndexTxOut oaddr (Left $ Pl.ValidatorHash vh) (Right datum) val, Just a)
            else return Nothing

-- | Check 'generateTx' for details
generateTx' :: (Monad m) => ValidateTxOpts -> TxSkel -> MockChainT m Pl.Tx
generateTx' opts skel = do
  modify $ updateDatumStr skel
  case generateUnbalTx skel of
    Left err -> throwError $ MCETxError err
    Right (ubtx, allSigners) -> do
      let adjust = if adjustUnbalTx opts then Pl.adjustUnbalancedTx else id
      balancedTx <- balanceTxFrom (txMainSigner skel) (adjust ubtx)
      return $ foldl (flip txAddSignature) balancedTx allSigners
  where
    -- Update the map of pretty printed representations in the mock chain state
    updateDatumStr :: TxSkel -> MockChainSt -> MockChainSt
    updateDatumStr TxSkel {txConstraints} st@MockChainSt {mcstStrDatums} =
      st
        { mcstStrDatums =
            M.unions $
              mcstStrDatums : (extractDatumStrFromConstraint <$> txConstraints)
        }

-- | Balances a transaction with money from a given wallet. For every transaction,
--  it must be the case that @inputs + mint == outputs + fee@.
balanceTxFrom :: (Monad m) => Wallet -> Pl.UnbalancedTx -> MockChainT m Pl.Tx
balanceTxFrom w (Pl.UnbalancedTx tx0 _reqSigs _uindex slotRange) = do
  -- We start by gathering all the inputs and summing it
  let tx = tx0 {Pl.txFee = Pl.minFee tx0}
  lhsInputs <- mapM (outFromOutRef . Pl.txInRef) (S.toList (Pl.txInputs tx))
  let lhs = mappend (mconcat $ map Pl.txOutValue lhsInputs) (Pl.txMint tx)
  let rhs = mappend (mconcat $ map Pl.txOutValue $ Pl.txOutputs tx) (Pl.txFee tx)
  let wPKH = walletPKHash w
  (usedUTxOs, leftOver) <- balanceWithUTxOsOf (rhs Pl.- lhs) wPKH
  -- All the UTxOs signed by the sender of the transaction and useful to balance it
  -- are added to the inputs.
  let txIns' = map (`Pl.TxIn` Just Pl.ConsumePublicKeyAddress) usedUTxOs
  -- A new output is opened with the leftover of the added inputs.
  let txOut' = Pl.TxOut (Pl.Address (Pl.PubKeyCredential wPKH) Nothing) leftOver Nothing
  config <- asks mceSlotConfig
  return
    tx
      { Pl.txInputs = Pl.txInputs tx <> S.fromList txIns',
        Pl.txOutputs = Pl.txOutputs tx ++ [txOut'],
        Pl.txValidRange = Pl.posixTimeRangeToContainedSlotRange config slotRange
      }

-- * Utilities

rstr :: (Monad m) => (a, m b) -> m (a, b)
rstr (a, mb) = (a,) <$> mb

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (a, (b, c)) = ((a, b), c)
