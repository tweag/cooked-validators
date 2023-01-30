{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.MockChain.Testing where

import qualified Control.Exception as E
import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Cooked.MockChain.Staged
import Cooked.MockChain.UtxoState
import Cooked.Wallet
import Data.Default
import qualified Data.Text as T
import Debug.Trace
import Ledger.Index (ValidationError (ScriptFailure))
import Ledger.Scripts (ScriptError (EvaluationError))
import qualified Test.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

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

testBool :: (IsProp prop) => Bool -> prop
testBool True = testSuccess
testBool False = testFailure

testAll :: (IsProp prop) => (a -> prop) -> [a] -> prop
testAll f = testConjoin . map f

infix 4 .==.

(.==.) :: (IsProp prop, Eq a) => a -> a -> prop
a .==. b = testBool $ a == b

infixr 3 .&&.

(.&&.) :: (IsProp prop) => prop -> prop -> prop
a .&&. b = testConjoin [a, b]

infixr 2 .||.

(.||.) :: (IsProp prop) => prop -> prop -> prop
a .||. b = testDisjoin [a, b]

-- | Ensure that all results produced by the staged mockchain /succeed/, starting
-- from the default initial distribution
testSucceeds :: (IsProp prop) => StagedMockChain a -> prop
testSucceeds = testSucceedsFrom def

-- | Ensure that all results produced by the staged mockchain /fail/ starting
-- from the default initial distribution
testFails :: (IsProp prop, Show a) => StagedMockChain a -> prop
testFails = testFailsFrom def

-- | Ensure that all results produced by the staged mockchain succeed starting
-- from some initial distribution but doesn't impose any additional condition on success.
-- Use 'testSucceedsFrom'' for that.
testSucceedsFrom ::
  (IsProp prop) =>
  InitialDistribution ->
  StagedMockChain a ->
  prop
testSucceedsFrom = testSucceedsFrom' (\_ _ -> testSuccess)

-- | Ensure that all results produced by the staged mockchain succeed starting
-- from some initial distribution. Additionally impose a condition over the
-- resulting state and value.
testSucceedsFrom' ::
  (IsProp prop) =>
  (a -> UtxoState -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testSucceedsFrom' prop = testAllSatisfiesFrom (either (testFailureMsg . show) (uncurry prop))

-- | Ensure that all results produced by the staged mockchain /fail/ starting
-- from some initial distribution
testFailsFrom ::
  (IsProp prop, Show a) =>
  InitialDistribution ->
  StagedMockChain a ->
  prop
testFailsFrom = testFailsFrom' (const testSuccess)

-- | Ensure that all results produced by the staged mockchain /fail/ starting
-- from some initial distribution, moreover, ensures that a certain predicate
-- over the error holds.
testFailsFrom' ::
  (IsProp prop, Show a) =>
  (MockChainError -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testFailsFrom' predi = testAllSatisfiesFrom (either predi (testFailureMsg . show))

-- | Is satisfied when the given 'MockChainError' is wrapping a @CekEvaluationFailure@.
-- This is particularly important when writing negative tests. For example, if we are simulating
-- an attack and writing a test with 'testFailsFrom', we might have made a mistake in the attack,
-- yielding a test that fails for reasons such as @ValueLessThanMinAda@ or @ValueNotPreserved@, which
-- does not rule out the attack being caught by the validator script. For these scenarios it is
-- paramount to rely on @testFailsFrom' isCekEvaluationFailure@ instead.
isCekEvaluationFailure :: (IsProp prop) => MockChainError -> prop
isCekEvaluationFailure (MCEValidationError (_, ScriptFailure _)) = testSuccess
isCekEvaluationFailure e = testFailureMsg $ "Expected 'CekEvaluationFailure', got: " ++ show e

-- | Similar to 'isCekEvaluationFailure', but enables us to check for a specific error message in the error.
isCekEvaluationFailureWithMsg :: (IsProp prop) => (String -> Bool) -> MockChainError -> prop
isCekEvaluationFailureWithMsg f (MCEValidationError (_, ScriptFailure (EvaluationError msgs _)))
  | any (f . T.unpack) msgs = testSuccess
isCekEvaluationFailureWithMsg _ e = testFailureMsg $ "Expected 'CekEvaluationFailure' with specific messages, got: " ++ show e

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
          [ "Expected two outcomes, both failed with:",
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
      joinMsg rest = msg ++ ";\n" ++ rest

      adjustMsg :: HU.HUnitFailure -> HU.HUnitFailure
      adjustMsg (HU.HUnitFailure loc txt) =
        HU.HUnitFailure loc (joinMsg txt)

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

-- | Here we provide our own universsal quantifier instead of 'QC.forAll', so we can monomorphize
--  it to returning a 'QC.Property'
forAll :: (Show a) => QC.Gen a -> (a -> QC.Property) -> QC.Property
forAll = QC.forAll

-- TODO: Discuss this instance; its here to enable us to easily
-- run things in a repl but I'm not sure whether to ignore the counterexample
-- messages or not.
instance IsProp Bool where
  testCounterexample msg False = trace msg False
  testCounterexample _ True = True
  testConjoin = and
  testDisjoin = or
