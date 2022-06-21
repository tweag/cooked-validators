{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Cooked.LtlTests (tests) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Cooked.Ltl
import Data.Maybe (isJust, mapMaybe)
import Debug.Trace (trace)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- The builtins are the events on the trace we wanna modify
data IntegerBuiltin a where
  EmitInteger :: Integer -> IntegerBuiltin ()

-- The type of modifications. Here we consider that modifications
-- can actually fail, hence the inclusion of Maybe
type DoubleHalveMods = (Integer, [String]) -> Maybe (Integer, [String])

type DoubleHalveOp = LtlOp DoubleHalveMods IntegerBuiltin

-- This doubles integers
doubleInteger :: DoubleHalveMods
doubleInteger (n, l) = return (2 * n, "double" : l)

-- This halves integers when then are even
halveInteger :: DoubleHalveMods
halveInteger (n, l) = if even n then Just (div n 2, "halve" : l) else Nothing

-- Couple an integer with an empty list of modifications
initInteger :: Integer -> (Integer, [String])
initInteger n = (n, [])

instance {-# OVERLAPS #-} Semigroup DoubleHalveMods where
  a <> b = a >=> b

-- mempty does something in that it encapsulates the value
instance {-# OVERLAPS #-} Monoid DoubleHalveMods where
  mempty = return

instance
  MonadPlus m =>
  InterpLtl
    -- We interpret in terms of modifications that double or halve integers
    DoubleHalveMods
    -- The events from the system, here only emitting integers
    IntegerBuiltin
    -- Since the modifications can fail, we mark states are being modified
    -- By coupling them with the names of the functions that have modified them
    (WriterT [(Integer, [String])] m)
  where
  -- So basically here i is the current integer in the input states and
  -- by calling "get" we retrieve the formula that has to be applied to it
  interpBuiltin (EmitInteger i) =
    get
      >>= msum
        . map (\(u, v) -> tell [u] <* put v)
        . mapMaybe (\(now, later) -> (,later) <$> now (initInteger i))
        . nowLaterList

-- The staged actions, here there is only one
emitInteger :: Integer -> Staged (LtlOp DoubleHalveMods IntegerBuiltin) ()
emitInteger i = Instr (Builtin (EmitInteger i)) Return

-- Interprets all the layers into a final result (a list of modified traces)
go :: Staged DoubleHalveOp a -> [[(Integer, [String])]]
go = execWriterT . flip execStateT [] . interpLtl

-- The type of traces that will eventually be run in tests
type DoubleHalveTrace = Staged DoubleHalveOp ()

-- A helper function to generate a trace from a list of integers
generateTrace :: [Integer] -> DoubleHalveTrace
generateTrace = foldl (\acc -> (acc >>) . emitInteger) (Return ())

-- Some test traces
traceEven, traceOdd, traceAll :: DoubleHalveTrace
traceEven = generateTrace [2, 4, 6]
traceOdd = generateTrace [5, 9, 3]
traceAll = generateTrace [8, 7, 1, 0]

-- Retrieved the indexes of the values that were modified by a given function
getIndexesModifiedBy ::
  String ->
  [(a, [String])] ->
  [Integer]
getIndexesModifiedBy s =
  map fst . filter (elem s . snd) . zip [0 ..] . map snd

-- Retrieves all the indexes
getAllIndexes :: [a] -> [Integer]
getAllIndexes = zipWith const [0 ..]

tests :: [TestTree]
tests =
  [ testGroup
      "DoubleHalve tests"
      [ testCase "all elements of traceEven can be halved" $
          assertBool "only 1 possible trace is expected" $
            let computations = go $ everywhere halveInteger traceEven
             in length computations == 1
      ]
  ]
