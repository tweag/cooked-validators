{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Cooked.MockChain.Base where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import Cooked.MockChain.Time
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import qualified Data.Map.Strict as M
import qualified Ledger.Address as Pl
import qualified Ledger.Blockchain as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Contexts as Pl
import qualified Ledger.Index as Pl
import Ledger.Orphans ()
import qualified Ledger.Scripts as Pl
import qualified Ledger.Value as Pl

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
  { mcstIndex :: Pl.UtxoIndex
  , mcstDatums :: M.Map Pl.DatumHash Pl.Datum
  , mcstStrDatums :: M.Map Pl.DatumHash String
  , mcstSlotCtr :: SlotCounter
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

onSlot :: (SlotCounter -> SlotCounter) -> MockChainSt -> MockChainSt
onSlot f mcst = mcst {mcstSlotCtr = f (mcstSlotCtr mcst)}

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
