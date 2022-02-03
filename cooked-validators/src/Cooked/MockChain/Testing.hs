{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.MockChain.Testing where

import qualified Control.Exception as E
import Control.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Data.Default
import qualified Test.HUnit.Lang as HU
import qualified Test.QuickCheck as QC

-- | This module provides a common interface for HUnit and QuickCheck tests.
-- We do so by abstracting uses of 'HU.Assertion' and 'QC.Property' for @(IsProp prop) => prop@,
-- then provide instances for both @HU.Asserton@ and @QC.Property@.
class IsProp prop where
  -- | Displays the string to the user in case of failure
  testCounterexample :: String -> prop -> prop

  -- | Conjunction of a number of results
  testConjoin :: [prop] -> prop

  -- | Disjunction of a number of results
  testDisjoin :: [prop] -> prop

  -- | Flags a failure
  testFailure :: prop
  testFailure = testDisjoin []

  -- | Flags a success
  testSuccess :: prop
  testSuccess = testConjoin []

  -- | Flags a failure with a message
  testFailureMsg :: String -> prop
  testFailureMsg msg = testCounterexample msg testFailure

testAll :: (IsProp prop) => (a -> prop) -> [a] -> prop
testAll f = testConjoin . map f

-- | Ensuprop that all results produced by the staged mockchain /succeed/, starting
-- from the default initial distribution
testSucceeds :: (IsProp prop) => StagedMockChain a -> prop
testSucceeds = testSucceedsFrom def

-- | Ensuprop that all results produced by the staged mockchain /fail/ starting
-- from the default initial distribution
testFails :: (IsProp prop, Show a) => StagedMockChain a -> prop
testFails = testFailsFrom def

-- | Ensuprop that all results produced by the staged mockchain succeed starting
-- from some initial distribution
testSucceedsFrom ::
  (IsProp prop) =>
  InitialDistribution ->
  StagedMockChain a ->
  prop
testSucceedsFrom = testAllSatisfiesFrom (either (testFailureMsg . show) (const testSuccess))

-- | Ensuprop that all results produced by the staged mockchain /succeed/ starting
-- from some initial distribution
testFailsFrom ::
  (IsProp prop, Show a) =>
  InitialDistribution ->
  StagedMockChain a ->
  prop
testFailsFrom = testAllSatisfiesFrom (either (const testSuccess) (testFailureMsg . show))

-- | Ensure that all results produced by the set of traces encoded by the 'StagedMockChain'
-- satisfy the given predicate. If you wish to build custom predicates
-- you can use 'testSatisfiesFrom'' directly and see 'testBinaryRelatedBy' as an example.
testAllSatisfiesFrom ::
  forall prop a.
  (IsProp prop) =>
  (Either MockChainError (a, UtxoState) -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testAllSatisfiesFrom f = testSatisfiesFrom' (testAll go)
  where
    go :: (Either MockChainError (a, UtxoState), TraceDescr) -> prop
    go (prop, tr) = testCounterexample (show tr) (f prop)

-- | Asserts that the given 'StagedMockChain' produces exactly two outcomes, both of which
-- are successful and have their resulting states related by a given predicate. A typical
-- usage would look like:
--
-- > testBinaryRelatedBy equalModuloAda myInitDistr $ do
-- >   x <- trPrepare
-- >   execOption1 x <|> execOption2 x
testBinaryRelatedBy ::
  (IsProp prop) =>
  (UtxoState -> UtxoState -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testBinaryRelatedBy rel = testSatisfiesFrom' $ \case
  [(ra, ta), (rb, tb)] -> case (ra, rb) of
    (Right resA, Right resB) -> rel (snd resA) (snd resB)
    (Left errA, Right _) ->
      testFailureMsg $ concat ["Expected two outcomes, the first failed with:", show errA, "\n", show ta]
    (Right _, Left errB) ->
      testFailureMsg $ concat ["Expected two outcomes, the second failed with:", show errB, "\n", show tb]
    (Left errA, Left errB) ->
      testFailureMsg $
        concat
          [ "Expected two outcomes, the both with:",
            show errA,
            "; ",
            show errB,
            "\n First: ",
            show ta,
            "\nSecond: ",
            show tb
          ]
  xs -> testFailureMsg $ "Expected exactly two outcomes, received: " ++ show (length xs)

-- | Generalizes 'testBinaryRelatedBy', asserting that the given 'StagedMockChain' produces
-- more than two outcomes, say @[x,y,z,w]@, all of which are successful (i.e. are not a 'MockChainError')
-- and these states are in the same equivalence class of (~); that is, they satisfy:
--
-- > x ~ y && x ~ z && x ~ z && x ~ w
--
-- Because @(~)@ should be symmetric and transitive we can estabilish that these states all belong
-- to the same equivalence class. This function does /not/ check each pointwise case.
testOneEquivClass ::
  (IsProp prop) =>
  (UtxoState -> UtxoState -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testOneEquivClass rel = testSatisfiesFrom' $ \case
  [] -> testFailureMsg "Expected two of more outcomes, received: 0"
  [_] -> testFailureMsg "Expected two of more outcomes, received: 1"
  ((Left errX, tx) : _) -> testFailureMsg $ concat ["First outcome is a failure: ", show errX, "\n", show tx]
  ((Right resX, _) : xs) -> go (snd resX) xs
  where
    -- we can flag a success here because 'xs' above is guarnateed to have at least
    -- one element since we ruled out the empty and the singleton lists in the \case
    go _resX [] = testSuccess
    go _resX ((Left errY, ty) : _) = testFailureMsg $ concat ["An outcome is a failure: ", show errY, "\n", show ty]
    go resX ((Right (_, resY), _) : ys) = testConjoin [rel resX resY, go resX ys]

-- | Asserts that the results produced by running the given 'StagedMockChain' from
-- some speficied 'InitialDistribution' satisfy a given assertion. In this case,
-- the predicate gets the trace descriptions that led to each potential outcome
-- and is responsible for calling 'testCounterexample' communicate these to the user.
--
-- Although this function is mainly used internally, as a building block for the simpler predicates,
-- it can be useful in building some custom predicates. Check 'testAllSatisfiesFrom'
-- or 'testBinaryRelatedBy' for examples on using this.
testSatisfiesFrom' ::
  ([(Either MockChainError (a, UtxoState), TraceDescr)] -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testSatisfiesFrom' predi i0 = predi . interpretAndRunWith (runMockChainTFrom i0)

-- * 'TestResult' instances

-- Catches a HUnit test failure, if the test fails.
assertionToMaybe :: HU.Assertion -> IO (Maybe HU.HUnitFailure)
assertionToMaybe = flip E.catches [E.Handler $ return . Just] . (>> return Nothing)

instance IsProp HU.Assertion where
  testCounterexample msg = maybe testSuccess (E.throw . adjustMsg) <=< assertionToMaybe
    where
      joinMsg :: String -> String
      joinMsg rest = msg ++ "; " ++ rest

      adjustMsg :: HU.HUnitFailure -> HU.HUnitFailure
      adjustMsg (HU.HUnitFailure loc (HU.Reason txt)) =
        HU.HUnitFailure loc (HU.Reason $ joinMsg txt)
      adjustMsg (HU.HUnitFailure loc (HU.ExpectedButGot pref x y)) =
        HU.HUnitFailure loc (HU.ExpectedButGot (maybe (Just msg) (Just . joinMsg) pref) x y)

  testFailure = HU.assertFailure ""
  testFailureMsg = HU.assertFailure

  testConjoin = sequence_

  testDisjoin [] = testFailure
  testDisjoin (x : xs) = assertionToMaybe x >>= maybe (testDisjoin xs) E.throw

  testSuccess = return ()

instance IsProp QC.Property where
  testCounterexample = QC.counterexample
  testFailure = QC.property False
  testSuccess = QC.property True
  testConjoin = QC.conjoin
  testDisjoin = QC.disjoin
