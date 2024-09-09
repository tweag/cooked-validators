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

-- * Common interface between HUnit and QuickCheck

-- | 'IsProp' is a common interface for HUnit and QuickCheck tests. It abstracts
-- uses of 'HU.Assertion' and 'QC.Property' for @(IsProp prop) => prop@, then
-- provide instances for both @HU.Asserton@ and @QC.Property@.
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

-- | Catches a HUnit test failure, if the test fails.
assertionToMaybe :: HU.Assertion -> IO (Maybe HU.HUnitFailure)
assertionToMaybe = flip E.catches [E.Handler $ return . Just] . (>> return Nothing)

-- | HUnit instance of 'IsProp'
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

-- | QuickCheck instance of 'IsProp'
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

-- | Bool instance of 'IsProp' instances

-- TODO: Discuss this instance; its here to enable us to easily run things in a
-- repl but I'm not sure whether to ignore the counterexample messages or not.
instance IsProp Bool where
  testCounterexample msg False = trace msg False
  testCounterexample _ True = True
  testConjoin = and
  testDisjoin = or

-- * Extra HUnit assertions

-- | Asserts whether a set is a subset of another one, both given as lists.
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

-- | Asserts whether 2 sets are equal, both given as lists.
assertSameSets :: (Show a, Eq a) => [a] -> [a] -> HU.Assertion
assertSameSets l r =
  HU.assertBool
    ("expected lists of the same length, got " ++ show (length l) ++ " and " ++ show (length r))
    (length l == length r)
    .&&. assertSubset l r
    .&&. assertSubset r l

-- * Testing mockchain runs

-- | Data structure to test a mockchain run
data Test a prop = Test
  { -- | The mockchain run to test
    testRun :: StagedMockChain a,
    -- | The initial distribution from which the run should be ran
    testInitDist :: InitialDistribution,
    -- | The property should hold in case of failure
    testErrorProp :: PrettyCookedOpts -> MockChainError -> [MockChainLogEntry] -> prop,
    -- | The property that should hold in case of success
    testResultProp :: PrettyCookedOpts -> a -> UtxoState -> [MockChainLogEntry] -> prop,
    -- | The printing option that should be use to render test results
    testPrettyOpts :: PrettyCookedOpts
  }

-- | A test template which expects a success from a run
mustSucceedTest :: (IsProp prop) => StagedMockChain a -> Test a prop
mustSucceedTest run =
  Test
    { testRun = run,
      testInitDist = def,
      testErrorProp = \opts res _ -> testFailureMsg $ renderString (prettyCookedOpt opts) res,
      testResultProp = \_ _ _ _ -> testSuccess,
      testPrettyOpts = def
    }

-- | A test template which expects a failure from a run
mustFailTest :: (IsProp prop, Show a) => StagedMockChain a -> Test a prop
mustFailTest run =
  Test
    { testRun = run,
      testInitDist = def,
      testErrorProp = \_ _ _ -> testSuccess,
      testResultProp = \opts a res _ -> testFailureMsg $ renderString (prettyCookedOpt opts) (a, res),
      testPrettyOpts = def
    }

-- | A test template with no particular requirement on the run
emptyTest :: (IsProp prop) => StagedMockChain a -> Test a prop
emptyTest run =
  Test
    { testRun = run,
      testInitDist = def,
      testErrorProp = \_ _ _ -> testSuccess,
      testResultProp = \_ _ _ _ -> testSuccess,
      testPrettyOpts = def
    }

infixl 5 <==

-- | The represents anything that can be added to a test
class AddToTest a prop b where
  (<==) :: b -> Test a prop -> Test a prop

-- | Appending an initial distribution to a test
instance AddToTest a prop InitialDistribution where
  initDist <== test = test {testInitDist = initDist}

-- | Appending printing options to a test
instance AddToTest a prop PrettyCookedOpts where
  opts <== test = test {testPrettyOpts = opts}

-- | Appending a predicate over the log to a test. This will be used both in
-- case of success or failure of the run.
instance (IsProp prop) => AddToTest a prop ([MockChainLogEntry] -> prop) where
  journalProp <== test =
    test
      { testErrorProp = \opts err journal -> testErrorProp test opts err journal .&&. journalProp journal,
        testResultProp = \opts val state journal -> testResultProp test opts val state journal .&&. journalProp journal
      }

-- | Appending a predicate over the resulting mockchain state and value, which
-- will be used in case of success of the run.
instance (IsProp prop) => AddToTest a prop (a -> UtxoState -> prop) where
  resultProp <== test =
    test
      { testResultProp = \opts val state journal -> testResultProp test opts val state journal .&&. resultProp val state
      }

-- | Appending a predicate over an error which uses the printing options, which
-- will be used in case of failure of the run.
instance (IsProp prop) => AddToTest a prop (PrettyCookedOpts -> MockChainError -> prop) where
  errorProp <== test = test {testErrorProp = \opts err journal -> testErrorProp test opts err journal .&&. errorProp opts err}

-- | Appending a predicate over and error, which will be used in case of
-- failure of the run.
instance (IsProp prop) => AddToTest a prop (MockChainError -> prop) where
  errorProp <== test = (\(_ :: PrettyCookedOpts) -> errorProp) <== test

-- | This takes a test and transforms it into an actual test case in prop.
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

-- | A property to ensure a phase 1 failure
isPhase1Failure :: (IsProp prop) => PrettyCookedOpts -> MockChainError -> prop
isPhase1Failure _ (MCEValidationError Ledger.Phase1 _) = testSuccess
isPhase1Failure pcOpts e = testFailureMsg $ "Expected phase 1 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | A test that succeeds when the run result in a phase 1 failure
testFailsInPhase1 :: forall prop a. (IsProp prop, Show a) => StagedMockChain a -> prop
testFailsInPhase1 = testProp . (isPhase1Failure @prop <==) . mustFailTest

-- | A property to ensure a phase 2 failure
isPhase2Failure :: (IsProp prop) => PrettyCookedOpts -> MockChainError -> prop
isPhase2Failure _ (MCEValidationError Ledger.Phase2 _) = testSuccess
isPhase2Failure pcOpts e = testFailureMsg $ "Expected phase 2 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | A test that succeeds when the run result in a phase 2 failure
testFailsInPhase2 :: forall prop a. (IsProp prop, Show a) => StagedMockChain a -> prop
testFailsInPhase2 = testProp . (isPhase2Failure @prop <==) . mustFailTest

-- | Same as 'isPhaseIFailure' with an added predicate on the text error
isPhase1FailureWithMsg :: (IsProp prop) => (String -> Bool) -> PrettyCookedOpts -> MockChainError -> prop
isPhase1FailureWithMsg f _ (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) | f $ T.unpack text = testSuccess
isPhase1FailureWithMsg _ pcOpts e = testFailureMsg $ "Expected phase 1 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Same as 'testFailsInPhase1' with an added predicate on the text error
testFailsInPhase1WithMsg :: forall prop a. (IsProp prop, Show a) => (String -> Bool) -> StagedMockChain a -> prop
testFailsInPhase1WithMsg f = testProp . (isPhase1FailureWithMsg @prop f <==) . mustFailTest

-- | Same as 'isPhase2Failure' with an added predicate over the text error
isPhase2FailureWithMsg :: (IsProp prop) => (String -> Bool) -> PrettyCookedOpts -> MockChainError -> prop
isPhase2FailureWithMsg f _ (MCEValidationError Ledger.Phase2 (Ledger.ScriptFailure (Ledger.EvaluationError texts _))) | any (f . T.unpack) texts = testSuccess
isPhase2FailureWithMsg _ pcOpts e = testFailureMsg $ "Expected phase 2 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Same as 'testFailsInPhase2' with an added predicate over the text error
testFailsInPhase2WithMsg :: forall prop a. (IsProp prop, Show a) => (String -> Bool) -> StagedMockChain a -> prop
testFailsInPhase2WithMsg f = testProp . (isPhase2FailureWithMsg @prop f <==) . mustFailTest
