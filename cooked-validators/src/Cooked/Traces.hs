{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Cooked.Traces where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator
import Data.Typeable
import Test.QuickCheck

type TxSkelGenT m a b = a -> MockChainT m (TxSkel, b)

type TxSkelGen a b = a -> MockChain (TxSkel, b)

modifyTxSkelGen ::
  (Monad m) =>
  (TxSkel -> TxSkel) ->
  TxSkelGenT m a b ->
  TxSkelGenT m a b
modifyTxSkelGen f g = fmap (first f) . g

data Tr m a b where
  Empty :: Tr m a a
  Step :: TxSkelGenT m a b -> Tr m b c -> Tr m a c
  Stutter :: Tr m a b -> Tr m a b

modifyTr :: (Monad m) => (TxSkel -> TxSkel) -> Tr m a b -> Tr m a b
modifyTr _ Empty = Empty
modifyTr f (Step g tr) = Step (modifyTxSkelGen f g) (modifyTr f tr)
modifyTr f (Stutter tr) = Stutter (modifyTr f tr)

executeTr :: (Monad m) => Tr m a b -> a -> MockChainT m b
executeTr Empty a = return a
executeTr (Stutter tr) a = executeTr tr a
executeTr (Step g tr) a = do
  (txSkel, b) <- g a
  validateTxFromSkeleton txSkel
  executeTr tr b

--------------------------------

-- Actions are whatever type can be mapped into a transaction skeleton
-- generator.
class Actions act where
  txSkel :: act -> MockChain TxSkel

-- Problems with this: some actions might generate certain utxos
-- which should be consumed in later actions...
--
-- classic example: some initial startup transaction that also returns
-- the system parameters
data Formula act
  = Aft act (Formula act)
  | Alt (Formula act) (Formula act)
  | Forall (Int -> Formula act)
  | Skip -- does nothing but terminates;
  | Block -- does nothing but doesn't terminate.

data TestStep
  = PerformAction act
  | WitnessForall Int

genSteps :: Formula -> Gen [TestStep]
denom :: [TestStep] -> MockChain ()
generate :: Gen a -> IO a
randomState :: Formula -> IO UtxoState
randomState f = do
  steps <- generate (genSteps f)
  return $ runMockChain $ denom steps

{-

instance Show act => Show (Formula act) where
  show (Aft act f) = "(Aft " ++ show act ++ " " ++ show f ++ ")"
  show (Alt l r) = "(Alt " ++ show l ++ " " ++ show r ++ ")"
  show (Forall f) = "(All " ++ show (f 3) ++ ")"
  show Skip = "Skip"
  show Block = "Block"

newtype F act a = F { unF :: (a -> Formula act) -> Formula act }
    deriving (Functor)

instance Applicative (F act) where
    pure x = F $ \k -> k x
    (<*>)  = ap

instance Monad (F act) where
    return = pure
    F h >>= j = F $ \ k -> h $ \ x -> unF (j x) k

instance Alternative (F s) where
    empty = F $ const Skip
    F h <|> F j = F $ \ k -> h k `Alt` j k

runF :: F act x -> Formula act
runF f = unF f $ const Skip

action :: act -> F act ()
action a = F $ \k -> Aft a (k ())

forAllInt :: F act Int
forAllInt = F $ \k -> Forall $ \i -> k i

example1 :: F String ()
example1 = do
  action "Startup"
  i <- forAllInt
  (action ("Go: " ++ show i)
     <|> action ("Go: " ++ show (i+1)))

-}

{-
data Step act
  = Do act
  | forall a. (Eq a, Show a, Typeable a) => Witness a

sem :: (Actions act) => Formula act -> Gen (MockChain ())
sem (Aft act fact) = do
  let txAct = txSkel act >>= validateTxFromSkeleton
  _

sem (Alt l_fact) = _
sem Skip = _
sem Block = _
-}
