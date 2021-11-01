{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Cooked.MockChain.Base where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
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

--

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a simple way to call
-- validator scripts directly, without the need for all the complexity tha the 'Contract'
-- monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', which is a map from 'Pl.Address' to
-- @(Pl.Value, Maybe Pl.Datum)@, and corresponds to the utxo mental model most people have.
-- Internally, however, we keep a 'Pl.UtxoIndex' in our state and feeding it to 'Pl.validateTx'.
-- For convenience, we also keep a map of 'Pl.Address' to 'Pl.Datum', giving is a simple
-- way of managing the current utxo state.

-- | A 'UtxoState' provides us with the mental picture of the state of the UTxO graph.
--  It includes a pretty printed representation alongside each datum for human
--  readable output during simulations.
type UtxoState = M.Map Pl.Address [(Pl.Value, Maybe (Pl.Datum, String))]

mcstToUtxoState :: MockChainSt -> UtxoState
mcstToUtxoState s =
  M.fromListWith (++) . map (uncurry go1) . M.toList . Pl.getIndex . mcstIndex $ s
  where
    go1 :: Pl.TxOutRef -> Pl.TxOut -> (Pl.Address, [(Pl.Value, Maybe (Pl.Datum, String))])
    go1 _ (Pl.TxOut addr val mdh) = do
      (addr, [(val, mdh >>= go2)])
    go2 :: Pl.DatumHash -> Maybe (Pl.Datum, String)
    go2 datumHash = do
      datumStr <- M.lookup datumHash (mcstStrDatums s)
      datum <- M.lookup datumHash (mcstDatums s)
      return (datum, datumStr)

-- | Slightly more concrete version of 'UtxoState', used to actually run the monster.
--  We keep a map from datum hash to datum, then a map from txOutRef to datumhash
--  For pretty printing purposes, we keep a map from datum hash to string representation
data MockChainSt = MockChainSt
  { mcstIndex :: Pl.UtxoIndex,
    mcstDatums :: M.Map Pl.DatumHash Pl.Datum,
    mcstStrDatums :: M.Map Pl.DatumHash String,
    mcstSlotCtr :: MockChainSlotCounter
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

-- custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return = pure
  MockChainT x >>= f =
    MockChainT $ do
      xres <- x
      modify' (\st -> st {mcstSlotCtr = mcscIncrease (mcstSlotCtr st)})
      unMockChain (f xres)

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

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

-- TODO: make the IO versions that pretty print the resulting UtxoState. It should
-- look something like:
--
-- pubkey abc1#8741:
--   - utxoRef1: val1
--   - utxoRef2: val2
-- script fff#1234:
--   - utxoRef3: val3
--               datum1
--

data MockChainSlotCounter = MockChainSlotCounter
  { mcscAutoIncrease :: Bool,
    mcscCurrentSlot :: Integer
  }
  deriving (Eq, Show)

mcscIncrease :: MockChainSlotCounter -> MockChainSlotCounter
mcscIncrease mcsc =
  let auto = mcscAutoIncrease mcsc
   in mcsc {mcscCurrentSlot = (if auto then (+ 1) else id) $ mcscCurrentSlot mcsc}

-- Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt utxoIndex0 M.empty M.empty (MockChainSlotCounter True 0)

utxoIndex0From :: InitialDistribution -> Pl.UtxoIndex
utxoIndex0From i0 = Pl.initialise [[Pl.Valid $ initialTxFor i0]]

utxoIndex0 :: Pl.UtxoIndex
utxoIndex0 = utxoIndex0From initialDistribution
