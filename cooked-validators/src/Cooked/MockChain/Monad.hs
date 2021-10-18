{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cooked.MockChain.Base where

import qualified Data.Map as M
import           Control.Monad.Except
import           Control.Monad.State

import qualified Ledger.Contexts    as Pl
import qualified Ledger.Index       as Pl
import qualified Ledger.Scripts     as Pl
import qualified Ledger.Constraints as Pl
import           Ledger.Orphans     ()


-- * Direct Emulation
--
-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a simple way to call
-- validator scripts directly, without the need for all the complexity tha the 'Contract'
-- monad introduces.
--
-- To do so, we keep a 'L.UtxoIndex' in our state and feeding it to 'L.validateTx'.
-- For convenience, we also keep a map of 'L.Address' to 'L.Datum', giving is a simple
-- way of managing the current utxo state.


-- |The actual 'MockChainT' is a trivial combination of 'StateT' and 'ExceptT'.
newtype MockChainT m a = MockChainT
    { unMockChain :: StateT MockChainSt (ExceptT MockChainError m) a }
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError)

data MockChainError
  = MCEValidationError Pl.ValidationErrorInPhase
  | MCETxError         Pl.MkTxError
  | FailWith           String
  deriving (Show, Eq)

data MockChainSt = MockChainSt
  { mcUtxo    :: Pl.UtxoIndex
  , mcDatums  :: M.Map Pl.TxOutRef Pl.Datum
  , mcSlotCtr :: MockChainSlotCounter
  } deriving (Show)

data MockChainSlotCounter = MockChainSlotCounter
  { mcscAutoIncrease :: Bool
  , mcscCurrentSlot  :: Integer
  } deriving (Eq, Show)

mcscIncrease :: MockChainSlotCounter -> MockChainSlotCounter
mcscIncrease mcsc =
  let auto = mcscAutoIncrease mcsc
   in mcsc { mcscCurrentSlot = (if auto then (+1) else id) $ mcscCurrentSlot mcsc }

-- custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return  = pure
  MockChainT x >>= f =
    MockChainT $ do
      xres <- x
      modify (\st -> st { mcSlotCtr = mcscIncrease (mcSlotCtr st) })
      unMockChain (f xres)

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

type MockChain = MockChainT Identity

runMockChainT :: (Monad m) => MockChainSt -> MockChainT m a -> m (Either MockChainError (a, UtxoIndex))
runMockChainT i0 = runExceptT . fmap (second utxo) . flip runStateT i0 . unMockChain

runMockChain :: MockChainSt -> MockChain a -> Either MockChainError (a, UtxoIndex)
runMockChain i0 = runIdentity . runMockChainT i0

runMockChainIO :: MockChainSt -> MockChain a -> IO (Either MockChainError a)
runMockChainIO i0 f = case runMockChain i0 f of
                        Left err      -> return (Left err)
                        Right (r, ix) -> printUtxoIndex ix >> return (Right r)
