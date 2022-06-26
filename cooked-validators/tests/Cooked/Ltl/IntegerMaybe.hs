{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cooked.Ltl.IntegerMaybe (integerMaybeTests) where

import Control.Monad (MonadPlus, msum, mzero)
import Control.Monad.Identity (Identity)
import Control.Monad.State (execStateT, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Cooked.Ltl
import Cooked.Ltl.Structure (Mod (Mod), ModExt, lift, toLabelled, Labelled)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- The type of modifications on integers that can fail
type IntegerMaybeMods = ModExt Integer Maybe

-- The builtins with a single observable event
data EmitInteger a where
  EmitInteger :: Integer -> EmitInteger ()

instance
  MonadPlus m =>
  InterpLtl
    -- We interpret in terms of modifications that double or halve integers
    IntegerMaybeMods
    -- The events from the system, here only emitting integers
    EmitInteger
    -- Since the modifications can fail, we mark states are being modified
    -- By coupling them with the names of the functions that have modified them
    (WriterT [(Integer, [String])] m)
  where
  -- So basically here i is the current integer in the input states and
  -- by calling "get" we retrieve the formula that has to be applied to it
  interpBuiltin (EmitInteger i) =
    get
      >>= msum
        . map
          ( \(now, later) -> case now (toLabelled i) of
              Nothing -> mzero
              Just res -> tell [res] <* put later
          )
        . nowLaterList


-- Operators built from the emission of integers that can fail
type IntegerMaybeOp = LtlOp IntegerMaybeMods EmitInteger

-- The staged actions, here there is only one
emitInteger :: Integer -> Staged IntegerMaybeOp ()
emitInteger i = Instr (Builtin (EmitInteger i)) Return

-- Interprets all the layers into a final result (a list of modified traces)
go :: Staged IntegerMaybeOp a -> [[Labelled Integer]]
go = execWriterT . flip execStateT [] . interpLtl

-- Some modifications
-- - The modification that always double integers,
-- - The modification that halves integers when possible
doubleInteger = lift $ Mod "double" (return . (2 *))
halveInteger = lift $ Mod "halve" (\n -> if even n then Just (div n 2) else Nothing)

-- A function returning all indexes of a list
getAllIndexes = zipWith const [0 ..]

-- A function that returns all indexes in a list that were modified by a given name
getIndexesModifiedBy :: String -> [(a, [String])] -> [Integer]
getIndexesModifiedBy s = map fst . filter (elem s . snd) . zip [0 ..] . map snd

-- A function that generates a trace from a list of integers
generateTrace = foldl (\acc -> (acc >>) . emitInteger) (Return ())

-- Some test traces
[subTraceEven, subTraceOdd, subTraceAll] = [[2, 4, 6], [5, 9, 3], [8, 7, 1, 0, 2, 6]]

[traceEven, traceOdd, traceAll] = map generateTrace [subTraceEven, subTraceOdd, subTraceAll]

integerMaybeTests :: [TestTree]
integerMaybeTests =
  [ testGroup
      "Even tests"
      ( let computations = go $ everywhere halveInteger traceEven
            hcomputations = head computations
         in [ testCase "Applying everywhere leads to a single computation" $
                assertBool "only 1 possible trace is expected" $
                  length computations == 1,
              testCase "All elements were marked as halved" $
                assertBool "all elements should be halved" $
                  getAllIndexes subTraceEven == getIndexesModifiedBy "halve" hcomputations,
              testCase "All elements were indeed halved" $
                assertBool "some elements were not halved" $
                  map (`div` 2) subTraceEven == map fst hcomputations
            ]
      ),
    testGroup
      "Odd tests"
      ( let sComputations = go $ somewhere halveInteger traceOdd
            dComputations = go $ somewhere doubleInteger traceOdd
         in [ testCase "Odd numbers are never be halved" $
                assertBool "No trace should be computed" $
                  null sComputations,
              testCase "Any odd number can possibly be halved" $
                assertBool "There are missing traces" $
                  length dComputations == 3,
              testCase "Only 1 odd number was marked doubled in each computation" $
                assertBool "Not a single odd number was marked doubled in some computation" $
                  all (\comp -> length (getIndexesModifiedBy "double" comp) == 1) dComputations,
              testCase "A number was indeed doubled in each computation" $
                assertBool "No number were actually doubled" $
                  all (\comp -> any (\(a, b) -> a == 2 * b) (zip (map fst comp) subTraceOdd)) dComputations
            ]
      ),
    testGroup
      "Even and Odd tests"
      ( let sComputations = go $ somewhere halveInteger traceAll
            aComputations = go $ everywhere halveInteger traceAll
            numberOfEven = length $ filter even subTraceAll
            eComputations = go $ somewhere doubleInteger traceAll
            fComputations = go $ modifyLtl (LtlTruth `LtlUntil` (LtlFalsity `LtlRelease` LtlAtom halveInteger)) traceAll
         in [ testCase "The right number of halved are done in singletons" $
                assertBool "There is a wrong number of cases" $
                  length sComputations == numberOfEven,
              testCase "There is no computations where all elements are halved" $
                assertBool "There are unsound computations" $
                  null aComputations,
              testCase "The right number of doubled are done in singletons" $
                assertBool "There is a wrong number of cases" $
                  length eComputations == length subTraceAll,
              testCase "There eventually is a serie of even numbers in 3 cases" $
                assertBool "Wrong number of sequences" $
                  length fComputations == 3
            ]
      )
  ]
