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

-- | Turns a boolean into a @prop@
testBool :: (IsProp prop) => Bool -> prop
testBool True = testSuccess
testBool False = testFailure

-- | Ensures all elements of a list satisfy a given @prop@
testAll :: (IsProp prop) => (a -> prop) -> [a] -> prop
testAll f = testConjoin . map f

infix 4 .==.

-- | Lifts an equality test to a @prop@
(.==.) :: (IsProp prop, Eq a) => a -> a -> prop
a .==. b = testBool $ a == b

infixr 3 .&&.

-- | Conjunction of two @prop@s
(.&&.) :: (IsProp prop) => prop -> prop -> prop
a .&&. b = testConjoin [a, b]

infixr 2 .||.

-- | Disjunction of two @prop@s
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

-- * Testing mockchain traces

-- | Data structure to test a mockchain trace
data Test a prop = Test
  { -- | The mockchain trace to test
    testTrace :: StagedMockChain a,
    -- | The initial distribution from which the trace should be run
    testInitDist :: InitialDistribution,
    -- | The requirement on the number of results
    testSizeProp :: Integer -> prop,
    -- | The property should hold in case of failure
    testErrorProp :: PrettyCookedOpts -> MockChainError -> [MockChainLogEntry] -> prop,
    -- | The property that should hold in case of success
    testResultProp :: PrettyCookedOpts -> a -> UtxoState -> [MockChainLogEntry] -> prop,
    -- | The printing option that should be use to render test results
    testPrettyOpts :: PrettyCookedOpts
  }

-- | A test template which expects a success from a trace
mustSucceedTest :: (IsProp prop) => StagedMockChain a -> Test a prop
mustSucceedTest trace =
  Test
    { testTrace = trace,
      testInitDist = def,
      testSizeProp = const testSuccess,
      testErrorProp = \opts res _ -> testFailureMsg $ renderString (prettyCookedOpt opts) res,
      testResultProp = \_ _ _ _ -> testSuccess,
      testPrettyOpts = def
    }

-- | A test template which expects a failure from a trace
mustFailTest :: (IsProp prop, Show a) => StagedMockChain a -> Test a prop
mustFailTest trace =
  Test
    { testTrace = trace,
      testInitDist = def,
      testSizeProp = const testSuccess,
      testErrorProp = \_ _ _ -> testSuccess,
      testResultProp = \opts a res _ -> testFailureMsg $ renderString (prettyCookedOpt opts) (a, res),
      testPrettyOpts = def
    }

-- | A test template with no particular requirement on the trace
emptyTest :: (IsProp prop) => StagedMockChain a -> Test a prop
emptyTest trace =
  Test
    { testTrace = trace,
      testInitDist = def,
      testSizeProp = const testSuccess,
      testErrorProp = \_ _ _ -> testSuccess,
      testResultProp = \_ _ _ _ -> testSuccess,
      testPrettyOpts = def
    }

-- | Appending an initial distribution to a test
withInitDist :: Test a prop -> InitialDistribution -> Test a prop
withInitDist test initDist = test {testInitDist = initDist}

-- | Appending printing options to a test
withPrettyOpts :: Test a prop -> PrettyCookedOpts -> Test a prop
withPrettyOpts test opts = test {testPrettyOpts = opts}

-- | Appending a predicate over the log to a test. This will be used both in
-- case of success or failure of the trace.
withJournalPred :: (IsProp prop) => Test a prop -> ([MockChainLogEntry] -> prop) -> Test a prop
withJournalPred test journalPred =
  test
    { testErrorProp = \opts err journal -> testErrorProp test opts err journal .&&. journalPred journal,
      testResultProp = \opts val state journal -> testResultProp test opts val state journal .&&. journalPred journal
    }

-- | Appending a predicate over the return value and state, which will be used
-- in case of success of the trace.
withValueAndStatePred :: (IsProp prop) => Test a prop -> (a -> UtxoState -> prop) -> Test a prop
withValueAndStatePred test resultPred =
  test
    { testResultProp = \opts val state journal -> testResultProp test opts val state journal .&&. resultPred val state
    }

-- | Ensuring the number of results of a test is exactly as expected
withExactSize :: (IsProp prop) => Test a prop -> Integer -> Test a prop
withExactSize test reqSize =
  test
    { testSizeProp = \n ->
        if n == reqSize
          then testSuccess
          else testFailureMsg $ "Incorrect number of results (expected: " <> show reqSize <> " but got: " <> show n <> ")"
    }

-- | Appending a predicate over the return value, which will be used in case of
-- success of the trace.
withValuePred :: (IsProp prop) => Test a prop -> (a -> prop) -> Test a prop
withValuePred test valuePred = withValueAndStatePred test $ \val _ -> valuePred val

-- | Appending a predicate over the return state, which will be used in case of
-- success of the trace.
withStatePred :: (IsProp prop) => Test a prop -> (UtxoState -> prop) -> Test a prop
withStatePred test statePred = withValueAndStatePred test $ \_ st -> statePred st

-- | Appending a predicate over an error which uses the printing options, which
-- will be used in case of failure of the trace.
withPrettyAndErrorPred :: (IsProp prop) => Test a prop -> (PrettyCookedOpts -> MockChainError -> prop) -> Test a prop
withPrettyAndErrorPred test errorPred = test {testErrorProp = \opts err journal -> testErrorProp test opts err journal .&&. errorPred opts err}

-- | Appends a predicate over an error to a 'Test'
withErrorPred :: (IsProp prop) => Test a prop -> (MockChainError -> prop) -> Test a prop
withErrorPred test errorPred = withPrettyAndErrorPred test $ \_ err -> errorPred err

-- | This takes a test and transforms it into an actual test case in prop.
testToProp :: (IsProp prop) => Test a prop -> prop
testToProp Test {..} =
  let innerProp (res, MockChainBook mcLog names) =
        case res of
          Left err -> testErrorProp (addHashNames names testPrettyOpts) err mcLog
          Right (result, state) -> testResultProp (addHashNames names testPrettyOpts) result state mcLog
      results = interpretAndRunWith (runMockChainTFrom testInitDist) testTrace
   in testSizeProp (toInteger (length results))
        .&&. testAll
          ( \res@(_, MockChainBook mcLog names) ->
              testCounterexample
                (renderString (prettyCookedOpt (addHashNames names testPrettyOpts)) mcLog)
                (innerProp res)
          )
          results

-- | Ensure that all results produced by the staged mockchain /succeed/,
-- starting from the default initial distribution
testSucceeds :: (IsProp prop) => StagedMockChain a -> prop
testSucceeds = testToProp . mustSucceedTest

-- | Ensure that all results produced by the staged mockchain /fail/
testFails :: (IsProp prop, Show a) => StagedMockChain a -> prop
testFails = testToProp . mustFailTest

-- | A property to ensure a phase 1 failure
isPhase1Failure :: (IsProp prop) => PrettyCookedOpts -> MockChainError -> prop
isPhase1Failure _ (MCEValidationError Ledger.Phase1 _) = testSuccess
isPhase1Failure pcOpts e = testFailureMsg $ "Expected phase 1 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | A test that succeeds when the trace results in a phase 1 failure
testFailsInPhase1 :: (IsProp prop, Show a) => StagedMockChain a -> prop
testFailsInPhase1 = testToProp . (`withPrettyAndErrorPred` isPhase1Failure) . mustFailTest

-- | A property to ensure a phase 2 failure
isPhase2Failure :: (IsProp prop) => PrettyCookedOpts -> MockChainError -> prop
isPhase2Failure _ (MCEValidationError Ledger.Phase2 _) = testSuccess
isPhase2Failure pcOpts e = testFailureMsg $ "Expected phase 2 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | A test that succeeds when the trace results in a phase 2 failure
testFailsInPhase2 :: (IsProp prop, Show a) => StagedMockChain a -> prop
testFailsInPhase2 = testToProp . (`withPrettyAndErrorPred` isPhase2Failure) . mustFailTest

-- | Same as 'isPhase1Failure' with an added predicate on the text error
isPhase1FailureWithMsg :: (IsProp prop) => (String -> Bool) -> PrettyCookedOpts -> MockChainError -> prop
isPhase1FailureWithMsg f _ (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) | f $ T.unpack text = testSuccess
isPhase1FailureWithMsg _ pcOpts e = testFailureMsg $ "Expected phase 1 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Same as 'testFailsInPhase1' with an added predicate on the text error
testFailsInPhase1WithMsg :: (IsProp prop, Show a) => (String -> Bool) -> StagedMockChain a -> prop
testFailsInPhase1WithMsg f = testToProp . (`withPrettyAndErrorPred` isPhase1FailureWithMsg f) . mustFailTest

-- | Same as 'isPhase2Failure' with an added predicate over the text error
isPhase2FailureWithMsg :: (IsProp prop) => (String -> Bool) -> PrettyCookedOpts -> MockChainError -> prop
isPhase2FailureWithMsg f _ (MCEValidationError Ledger.Phase2 (Ledger.ScriptFailure (Ledger.EvaluationError texts _))) | any (f . T.unpack) texts = testSuccess
isPhase2FailureWithMsg _ pcOpts e = testFailureMsg $ "Expected phase 2 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Same as 'testFailsInPhase2' with an added predicate over the text error
testFailsInPhase2WithMsg :: (IsProp prop, Show a) => (String -> Bool) -> StagedMockChain a -> prop
testFailsInPhase2WithMsg f = testToProp . (`withPrettyAndErrorPred` isPhase2FailureWithMsg f) . mustFailTest
