{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Cooked.MockChain.Monad.Direct where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import Cooked.MockChain.Monad
import Cooked.MockChain.Time
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Balance
import Cooked.Tx.Constraints
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import qualified Ledger as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Credential as Pl
import Ledger.Orphans ()
import qualified PlutusTx as Pl

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
    mcstSlotCtr :: SlotCounter
  }
  deriving (Show)

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError
  = MCEValidationError Pl.ValidationErrorInPhase
  | MCETxError Pl.MkTxError
  | FailWith String
  deriving (Show, Eq)

-- | The actual 'MockChainT' is a trivial combination of 'StateT' and 'ExceptT'
newtype MockChainT m a = MockChainT
  {unMockChain :: StateT MockChainSt (ExceptT MockChainError m) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError)

-- | Non-transformer variant
type MockChain = MockChainT Identity

-- Custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return = pure
  MockChainT x >>= f =
    MockChainT $ do
      xres <- x
      modify' (\st -> st {mcstSlotCtr = scIncrease (mcstSlotCtr st)})
      unMockChain (f xres)

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

onSlot :: (SlotCounter -> SlotCounter) -> MockChainSt -> MockChainSt
onSlot f mcst = mcst {mcstSlotCtr = f (mcstSlotCtr mcst)}

mapMockChainT ::
  (m (Either MockChainError (a, MockChainSt)) -> n (Either MockChainError (b, MockChainSt))) ->
  MockChainT m a ->
  MockChainT n b
mapMockChainT f = MockChainT . mapStateT (mapExceptT f) . unMockChain

-- | Executes a 'MockChainT' from some initial state.
runMockChainTFrom ::
  (Monad m) =>
  MockChainSt ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom i0 = runExceptT . fmap (second mcstToUtxoState) . flip runStateT i0 . unMockChain

runMockChainFrom :: MockChainSt -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainFrom i0 = runIdentity . runMockChainTFrom i0

-- | Executes a 'MockChainT' from the cannonical initial state.
runMockChainT :: (Monad m) => MockChainT m a -> m (Either MockChainError (a, UtxoState))
runMockChainT = runMockChainTFrom mockChainSt0

runMockChain :: MockChain a -> Either MockChainError (a, UtxoState)
runMockChain = runIdentity . runMockChainT

-- Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt utxoIndex0 M.empty M.empty slotCounter0

utxoIndex0From :: InitialDistribution -> Pl.UtxoIndex
utxoIndex0From i0 = Pl.initialise [[Pl.Valid $ initialTxFor i0]]

utxoIndex0 :: Pl.UtxoIndex
utxoIndex0 = utxoIndex0From initialDistribution

-- ** Direct Interpretation of Operations

instance (Monad m) => MonadMockChain (MockChainT m) where
  validateTxSkel = validateTx' <=< generateTx'
  index = gets (Pl.getIndex . mcstIndex)
  slotCounter = gets mcstSlotCtr
  modifySlotCounter f = modify (\st -> st {mcstSlotCtr = f $ mcstSlotCtr st})
  utxosSuchThat = utxosSuchThat'

-- | Check 'validateTx' for details
validateTx' :: (Monad m) => Pl.Tx -> MockChainT m ()
validateTx' tx = do
  s <- slot
  ix <- gets mcstIndex
  slotCfg <- gets (slotConfig . mcstSlotCtr)
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
                mcstDatums =
                  (mcstDatums st `M.difference` consumedDHs')
                    `M.union` Pl.txData tx
              }
        )

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
generateTx' :: (Monad m) => TxSkel -> MockChainT m Pl.Tx
generateTx' skel = do
  modify $ updateDatumStr skel
  case generateUnbalTx skel of
    Left err -> throwError $ MCETxError err
    Right (ubtx, allSigners) -> do
      balancedTx <- balanceTxFrom (txMainSigner skel) ubtx
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

-- | Returns the current internal slot count.
slot :: (Monad m) => MockChainT m Pl.Slot
slot = gets (Pl.Slot . currentSlot . mcstSlotCtr)

-- * Utilities

rstr :: (Monad m) => (a, m b) -> m (a, b)
rstr (a, mb) = (a,) <$> mb

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (a, (b, c)) = ((a, b), c)
