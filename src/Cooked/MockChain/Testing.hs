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
import Cooked.Pretty
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
testSucceeds :: (IsProp prop) => PrettyCookedOpts -> StagedMockChain a -> prop
testSucceeds pcOpts = testSucceedsFrom pcOpts def

-- | Ensure that all results produced by the staged mockchain /fail/ starting
-- from the default initial distribution
testFails :: (IsProp prop, Show a) => PrettyCookedOpts -> StagedMockChain a -> prop
testFails pcOpts = testFailsFrom pcOpts def

-- | Ensure that all results produced by the staged mockchain succeed starting
-- from some initial distribution but doesn't impose any additional condition on success.
-- Use 'testSucceedsFrom'' for that.
testSucceedsFrom ::
  (IsProp prop) =>
  PrettyCookedOpts ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testSucceedsFrom pcOpts = testSucceedsFrom' pcOpts (\_ _ -> testSuccess)

-- | Ensure that all results produced by the staged mockchain succeed starting
-- from some initial distribution. Additionally impose a condition over the
-- resulting state and value.
testSucceedsFrom' ::
  (IsProp prop) =>
  PrettyCookedOpts ->
  (a -> UtxoState -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testSucceedsFrom' pcOpts prop = testAllSatisfiesFrom pcOpts (either (testFailureMsg . renderString (prettyCookedOpt pcOpts)) (uncurry prop))

-- | Ensure that all results produced by the staged mockchain /fail/ starting
-- from some initial distribution
testFailsFrom ::
  (IsProp prop, Show a) =>
  PrettyCookedOpts ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testFailsFrom pcOpts = testFailsFrom' pcOpts (const testSuccess)

-- | Ensure that all results produced by the staged mockchain /fail/ starting
-- from some initial distribution, moreover, ensures that a certain predicate
-- over the error holds.
testFailsFrom' ::
  (IsProp prop, Show a) =>
  PrettyCookedOpts ->
  (MockChainError -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testFailsFrom' pcOpts predi = testAllSatisfiesFrom pcOpts (either predi (testFailureMsg . renderString (prettyCookedOpt pcOpts)))

-- | Is satisfied when the given 'MockChainError' is wrapping a @CekEvaluationFailure@.
-- This is particularly important when writing negative tests. For example, if we are simulating
-- an attack and writing a test with 'testFailsFrom', we might have made a mistake in the attack,
-- yielding a test that fails for reasons such as @ValueLessThanMinAda@ or @ValueNotPreserved@, which
-- does not rule out the attack being caught by the validator script. For these scenarios it is
-- paramount to rely on @testFailsFrom' isCekEvaluationFailure@ instead.
isCekEvaluationFailure :: (IsProp prop) => PrettyCookedOpts -> MockChainError -> prop
isCekEvaluationFailure _ (MCEValidationError (_, ScriptFailure _)) = testSuccess
isCekEvaluationFailure pcOpts e = testFailureMsg $ "Expected 'CekEvaluationFailure', got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Similar to 'isCekEvaluationFailure', but enables us to check for a specific error message in the error.
isCekEvaluationFailureWithMsg :: (IsProp prop) => PrettyCookedOpts -> (String -> Bool) -> MockChainError -> prop
isCekEvaluationFailureWithMsg _ f (MCEValidationError (_, ScriptFailure (EvaluationError msgs _)))
  | any (f . T.unpack) msgs = testSuccess
isCekEvaluationFailureWithMsg pcOpts _ e = testFailureMsg $ "Expected 'CekEvaluationFailure' with specific messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Ensure that all results produced by the set of traces encoded by the 'StagedMockChain'
-- satisfy the given predicate. If you wish to build custom predicates
-- you can use 'testSatisfiesFrom'' directly and see 'testBinaryRelatedBy' as an example.
testAllSatisfiesFrom ::
  forall prop a.
  (IsProp prop) =>
  PrettyCookedOpts ->
  (Either MockChainError (a, UtxoState) -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testAllSatisfiesFrom pcOpts f = testSatisfiesFrom' (testAll go)
  where
    go :: (Either MockChainError (a, UtxoState), MockChainLog) -> prop
    go (prop, mcLog) = testCounterexample (renderString (prettyCookedOpt pcOpts) mcLog) (f prop)

-- | Asserts that the given 'StagedMockChain' produces exactly two outcomes, both of which
-- are successful and have their resulting states related by a given predicate. A typical
-- usage would look like:
--
-- > testBinaryRelatedBy equalModuloAda myInitDistr $ do
-- >   x <- trPrepare
-- >   execOption1 x <|> execOption2 x
testBinaryRelatedBy ::
  (IsProp prop) =>
  PrettyCookedOpts ->
  (UtxoState -> UtxoState -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testBinaryRelatedBy pcOpts rel = testSatisfiesFrom' $ \case
  [(ra, ta), (rb, tb)] -> case (ra, rb) of
    (Right resA, Right resB) -> rel (snd resA) (snd resB)
    (Left errA, Right _) ->
      testFailureMsg $ concat ["Expected two outcomes, the first failed with:", renderString (prettyCookedOpt pcOpts) errA, "\n", renderString (prettyCookedOpt pcOpts) ta]
    (Right _, Left errB) ->
      testFailureMsg $ concat ["Expected two outcomes, the second failed with:", renderString (prettyCookedOpt pcOpts) errB, "\n", renderString (prettyCookedOpt pcOpts) tb]
    (Left errA, Left errB) ->
      testFailureMsg $
        concat
          [ "Expected two outcomes, both failed with:",
            renderString (prettyCookedOpt pcOpts) errA,
            "; ",
            renderString (prettyCookedOpt pcOpts) errB,
            "\n First: ",
            renderString (prettyCookedOpt pcOpts) ta,
            "\nSecond: ",
            renderString (prettyCookedOpt pcOpts) tb
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
  PrettyCookedOpts ->
  (UtxoState -> UtxoState -> prop) ->
  InitialDistribution ->
  StagedMockChain a ->
  prop
testOneEquivClass pcOpts rel = testSatisfiesFrom' $ \case
  [] -> testFailureMsg "Expected two of more outcomes, received: 0"
  [_] -> testFailureMsg "Expected two of more outcomes, received: 1"
  ((Left errX, tx) : _) -> testFailureMsg $ concat ["First outcome is a failure: ", renderString (prettyCookedOpt pcOpts) errX, "\n", renderString (prettyCookedOpt pcOpts) tx]
  ((Right resX, _) : xs) -> go (snd resX) xs
  where
    -- we can flag a success here because 'xs' above is guarnateed to have at least
    -- one element since we ruled out the empty and the singleton lists in the \case
    go _resX [] = testSuccess
    go _resX ((Left errY, ty) : _) = testFailureMsg $ concat ["An outcome is a failure: ", renderString (prettyCookedOpt pcOpts) errY, "\n", renderString (prettyCookedOpt pcOpts) ty]
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
  ([(Either MockChainError (a, UtxoState), MockChainLog)] -> prop) ->
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
