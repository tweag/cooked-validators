-- | This modules provides primitives to run tests over mockchain executions and
-- to give expectation on the result of these runs.
module Cooked.MockChain.Testing where

import Control.Exception qualified as E
import Control.Monad
import Cooked.InitialDistribution
import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Cooked.MockChain.Staged
import Cooked.MockChain.UtxoState
import Cooked.Pretty
import Data.Default
import Data.Text qualified as T
import Debug.Trace
import Ledger qualified
import Test.QuickCheck qualified as QC
import Test.Tasty.HUnit qualified as HU

-- | This module provides a common interface for HUnit and QuickCheck tests. We
-- do so by abstracting uses of 'HU.Assertion' and 'QC.Property' for @(IsProp
-- prop) => prop@, then provide instances for both @HU.Asserton@ and
-- @QC.Property@.
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

data Test a prop where
  Test ::
    { testRun :: StagedMockChain a,
      testInitDist :: InitialDistribution,
      testErrorProp :: PrettyCookedOpts -> MockChainError -> [MockChainLogEntry] -> prop,
      testResultProp :: PrettyCookedOpts -> a -> UtxoState -> [MockChainLogEntry] -> prop,
      testPrettyOpts :: PrettyCookedOpts
    } ->
    Test a prop

mustSucceedTest :: (IsProp prop) => StagedMockChain a -> Test a prop
mustSucceedTest run =
  Test
    { testRun = run,
      testInitDist = def,
      testErrorProp = \opts res _ -> testFailureMsg $ renderString (prettyCookedOpt opts) res,
      testResultProp = \_ _ _ _ -> testSuccess,
      testPrettyOpts = def
    }

mustFailTest :: (IsProp prop, Show a) => StagedMockChain a -> Test a prop
mustFailTest run =
  Test
    { testRun = run,
      testInitDist = def,
      testErrorProp = \_ _ _ -> testSuccess,
      testResultProp = \opts a res _ -> testFailureMsg $ renderString (prettyCookedOpt opts) (a, res),
      testPrettyOpts = def
    }

emptyTest :: (IsProp prop) => StagedMockChain a -> Test a prop
emptyTest run =
  Test
    { testRun = run,
      testInitDist = def,
      testErrorProp = \_ _ _ -> testSuccess,
      testResultProp = \_ _ _ _ -> testSuccess,
      testPrettyOpts = def
    }

infixl 5 ==>

class AddToTest a prop b where
  (==>) :: Test a prop -> b -> Test a prop

instance AddToTest a prop InitialDistribution where
  test ==> initDist = test {testInitDist = initDist}

instance AddToTest a prop PrettyCookedOpts where
  test ==> opts = test {testPrettyOpts = opts}

instance (IsProp prop) => AddToTest a prop ([MockChainLogEntry] -> prop) where
  test ==> journalProp =
    test
      { testErrorProp = \opts err journal -> testErrorProp test opts err journal .&&. journalProp journal,
        testResultProp = \opts val state journal -> testResultProp test opts val state journal .&&. journalProp journal
      }

instance (IsProp prop) => AddToTest a prop (a -> UtxoState -> prop) where
  test ==> resultProp = test {testResultProp = \opts val state journal -> testResultProp test opts val state journal .&&. resultProp val state}

instance (IsProp prop) => AddToTest a prop (PrettyCookedOpts -> MockChainError -> prop) where
  test ==> errorProp = test {testErrorProp = \opts err journal -> testErrorProp test opts err journal .&&. errorProp opts err}

instance (IsProp prop) => AddToTest a prop (MockChainError -> prop) where
  test ==> errorProp = test ==> \(_ :: PrettyCookedOpts) -> errorProp

testProp :: (IsProp prop) => Test a prop -> prop
testProp Test {..} =
  let innerProp (res, mcLog) =
        case res of
          Left err -> testErrorProp testPrettyOpts err mcLog
          Right (result, state) -> testResultProp testPrettyOpts result state mcLog
   in testAll
        (\ret@(_, mcLog) -> testCounterexample (renderString (prettyCookedOpt testPrettyOpts) mcLog) (innerProp ret))
        (interpretAndRunWith (runMockChainTFrom testInitDist) testRun)

-- | Ensure that all results produced by the staged mockchain /succeed/,
-- starting from the default initial distribution
testSucceeds :: (IsProp prop) => StagedMockChain a -> prop
testSucceeds = testProp . mustSucceedTest

-- | Ensure that all results produced by the staged mockchain /fail/
testFails :: (IsProp prop, Show a) => StagedMockChain a -> prop
testFails = testProp . mustFailTest

isPhase1Failure :: (IsProp prop) => PrettyCookedOpts -> MockChainError -> prop
isPhase1Failure _ (MCEValidationError Ledger.Phase1 _) = testSuccess
isPhase1Failure pcOpts e = testFailureMsg $ "Expected phase 1 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

testFailsInPhase1 :: forall prop a. (IsProp prop, Show a) => StagedMockChain a -> prop
testFailsInPhase1 run = testProp $ mustFailTest run ==> isPhase1Failure @prop

isPhase2Failure :: (IsProp prop) => PrettyCookedOpts -> MockChainError -> prop
isPhase2Failure _ (MCEValidationError Ledger.Phase2 _) = testSuccess
isPhase2Failure pcOpts e = testFailureMsg $ "Expected phase 2 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

testFailsInPhase2 :: forall prop a. (IsProp prop, Show a) => StagedMockChain a -> prop
testFailsInPhase2 run = testProp $ mustFailTest run ==> isPhase2Failure @prop

isPhase1FailureWithMsg :: (IsProp prop) => (String -> Bool) -> PrettyCookedOpts -> MockChainError -> prop
isPhase1FailureWithMsg f _ (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) | f $ T.unpack text = testSuccess
isPhase1FailureWithMsg _ pcOpts e = testFailureMsg $ "Expected phase 1 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

testFailsInPhase1WithMsg :: forall prop a. (IsProp prop, Show a) => StagedMockChain a -> (String -> Bool) -> prop
testFailsInPhase1WithMsg run f = testProp $ mustFailTest run ==> isPhase1FailureWithMsg @prop f

isPhase2FailureWithMsg :: (IsProp prop) => (String -> Bool) -> PrettyCookedOpts -> MockChainError -> prop
isPhase2FailureWithMsg f _ (MCEValidationError Ledger.Phase2 (Ledger.ScriptFailure (Ledger.EvaluationError texts _))) | any (f . T.unpack) texts = testSuccess
isPhase2FailureWithMsg _ pcOpts e = testFailureMsg $ "Expected phase 2 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

testFailsInPhase2WithMsg :: forall prop a. (IsProp prop, Show a) => StagedMockChain a -> (String -> Bool) -> prop
testFailsInPhase2WithMsg run f = testProp $ mustFailTest run ==> isPhase2FailureWithMsg @prop f

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

-- | Here we provide our own universsal quantifier instead of 'QC.forAll', so we
--  can monomorphize it to returning a 'QC.Property'
forAll :: (Show a) => QC.Gen a -> (a -> QC.Property) -> QC.Property
forAll = QC.forAll

-- TODO: Discuss this instance; its here to enable us to easily run things in a
-- repl but I'm not sure whether to ignore the counterexample messages or not.
instance IsProp Bool where
  testCounterexample msg False = trace msg False
  testCounterexample _ True = True
  testConjoin = and
  testDisjoin = or

assertSubset :: (Show a, Eq a) => [a] -> [a] -> HU.Assertion
assertSubset l r =
  testConjoin
    ( map
        ( \x ->
            HU.assertBool
              ( "not a subset:\n\n"
                  ++ show x
                  ++ "\n\nis not an element of\n\n"
                  ++ show r
              )
              $ x `elem` r
        )
        l
    )

assertSameSets :: (Show a, Eq a) => [a] -> [a] -> HU.Assertion
assertSameSets l r =
  HU.assertBool
    ("expected lists of the same length, got " ++ show (length l) ++ " and " ++ show (length r))
    (length l == length r)
    .&&. assertSubset l r
    .&&. assertSubset r l
