-- | This modules provides primitives to run tests over mockchain executions and
-- to provide requirements on the the number and results of these runs.
module Cooked.MockChain.Testing where

import Control.Exception qualified as E
import Control.Monad
import Cooked.InitialDistribution
import Cooked.MockChain.Error
import Cooked.MockChain.Instances
import Cooked.MockChain.Log
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Write
import Cooked.Pretty
import Data.Default
import Data.List (isInfixOf)
import Data.Set qualified as Set
import Data.Text qualified as T
import Ledger qualified
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import Polysemy
import Test.QuickCheck qualified as QC
import Test.Tasty qualified as HU
import Test.Tasty.HUnit qualified as HU
import Test.Tasty.QuickCheck qualified as QC

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

-- | Turns a boolean into a @prop@, displaying an error message when applicable
testBoolMsg :: (IsProp prop) => String -> Bool -> prop
testBoolMsg _ True = testSuccess
testBoolMsg msg False = testFailureMsg msg

-- | Ensures all elements of a list satisfy a given @prop@
testAll :: (IsProp prop) => (a -> prop) -> [a] -> prop
testAll f = testConjoin . map f

-- | Ensures at least one element of a list satisfy a given @prop@
testAny :: (IsProp prop) => (a -> prop) -> [a] -> prop
testAny f = testDisjoin . map f

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

-- * Data structure to test mockchain traces

{--
  Note on properties over the journal (or list of 'MockChainLogEntry'): our
  'Test' structure does not directly embed a predicate over the journal. Instead
  it is embedded in both the failure and success prediates. The reason is
  simple: the journal is generated and accessible in both cases and thus it is
  theoretically possible to define predicates that combine requirements over the
  journal and the error in case of failure, and the journal and the returning
  state and value in the case of success. If the journal predicate was a field
  in itself, this link would be broken and it would not be possible to epxress
  complex requirements that involve both the journal and other components of the
  returned elements in the mockchain run. Granted, this use cas is extremely
  rare, but it does not mean our API should not reflect this capability.
  However, we also provide 'JournalProp' as in most cases predicating over
  the journal itself will be sufficient.
--}

-- | Type of properties over failures
type FailureProp prop = PrettyCookedOpts -> [MockChainLogEntry] -> MockChainError -> UtxoState -> prop

-- | Type of properties over successes
type SuccessProp a prop = PrettyCookedOpts -> [MockChainLogEntry] -> a -> UtxoState -> prop

-- | Type of properties over the number of run outcomes. This does not
-- necessitate a 'PrettyCookedOpts' as parameter as an 'Integer' does not
-- contain anything significant that can be pretty printed.
type SizeProp prop = Integer -> prop

-- | Type of properties over the mockchain journal
type JournalProp prop = PrettyCookedOpts -> [MockChainLogEntry] -> prop

-- | Type of properties over the 'UtxoState'
type StateProp prop = PrettyCookedOpts -> UtxoState -> prop

-- | Data structure to test a mockchain trace. @a@ is the return typed of the
-- tested trace, @prop@ is the domain in which the properties live. This is not
-- enforced here, but it will often be assumed that @prop@ satisfies 'IsProp'.
data Test effs a prop = Test
  { -- | The mockchain trace to test, which returns a result of type a
    testTrace :: Sem effs a,
    -- | The initial distribution from which the trace should be run
    testInitDist :: InitialDistribution,
    -- | The requirement on the number of results, as 'StagedMockChain' is a
    -- 'Control.Monad.MonadPlus'
    testSizeProp :: SizeProp prop,
    -- | The property that should hold in case of failure over the resulting
    -- error and the logs emitted during the run
    testFailureProp :: FailureProp prop,
    -- | The property that should hold in case of success over the returned
    -- result and the final state of the trace, as well as the logs
    testSuccessProp :: SuccessProp a prop,
    -- | The printing option that should be use to render test results
    testPrettyOpts :: PrettyCookedOpts
  }

-- | This takes a 'Test' and transforms it into an actual test case in
-- prop. This is the main function justifying the existence of 'Test'. This runs
-- the traces, ensures there is the right number of outcomes and, depending on
-- the nature of these outcomes, either calls 'testFailureProp' or
-- 'testSuccessProp'. It also uses the aliases emitted during the mockchain run
-- to pretty print messages when applicable.
testToProp ::
  ( IsProp prop,
    Show a,
    Member MockChainWrite effs,
    IsMockChain effs
  ) =>
  Test effs a prop ->
  prop
testToProp Test {..} =
  let results = runMockChainFromConf (mockChainConfTemplate {mccInitialDistribution = testInitDist}) testTrace
   in testSizeProp (toInteger (length results))
        .&&. testAll
          ( \ret@(MockChainReturn outcome _ state mcLog names) ->
              let pcOpts = addHashNames names testPrettyOpts
               in testCounterexample
                    (renderString (prettyCookedOpt pcOpts) ret)
                    $ case outcome of
                      Left err -> testFailureProp pcOpts mcLog err state
                      Right result -> testSuccessProp pcOpts mcLog result state
          )
          results

-- | A convenience helper when using 'HU.Assertion' which allows to replace
-- 'HU.testCase' with 'testCooked' and thus avoid the use of 'testToProp'.
-- Sadly we cannot generalise it with type classes on @prop@ to work for
-- QuichCheck at GHC will never be able to instantiate @prop@.
testCooked ::
  forall effs a.
  ( Show a,
    Member MockChainWrite effs,
    IsMockChain effs
  ) =>
  String ->
  Test effs a HU.Assertion ->
  HU.TestTree
testCooked name = HU.testCase name . testToProp

-- | Same as 'testCooked', but for 'QC.Property'
testCookedQC ::
  forall effs a.
  ( Show a,
    Member MockChainWrite effs,
    IsMockChain effs
  ) =>
  String ->
  Test effs a QC.Property ->
  HU.TestTree
testCookedQC name = QC.testProperty name . testToProp

-- * Simple test templates

-- | A test template which expects a success from a trace
mustSucceedTest :: (IsProp prop) => Sem effs a -> Test effs a prop
mustSucceedTest trace =
  Test
    { testTrace = trace,
      testInitDist = def,
      testSizeProp = isAtLeastOfSize 1,
      testFailureProp = \_ _ _ _ -> testFailureMsg "💀 Unexpected failure!",
      testSuccessProp = \_ _ _ _ -> testSuccess,
      testPrettyOpts = def
    }

-- | A test template which expects a failure from a trace
mustFailTest :: (IsProp prop) => Sem effs a -> Test effs a prop
mustFailTest trace =
  Test
    { testTrace = trace,
      testInitDist = def,
      testSizeProp = const testSuccess,
      testFailureProp = \_ _ _ _ -> testSuccess,
      testSuccessProp = \_ _ _ _ -> testFailureMsg "💀 Unexpected success!",
      testPrettyOpts = def
    }

-- * Appending elements (in particular requirements) to existing tests

-- | Gives an initial distribution from which the trace will be run
withInitDist :: Test effs a prop -> InitialDistribution -> Test effs a prop
withInitDist test initDist = test {testInitDist = initDist}

-- | Gives some pretty options to render test messages
withPrettyOpts :: Test effs a prop -> PrettyCookedOpts -> Test effs a prop
withPrettyOpts test opts = test {testPrettyOpts = opts}

-- | Appends a requirements over the emitted log, which will need to be satisfied
-- both in case of success or failure of the run.
withJournalProp :: (IsProp prop) => Test effs a prop -> JournalProp prop -> Test effs a prop
withJournalProp test journalProp =
  test
    { testFailureProp = \opts journal err state -> testFailureProp test opts journal err state .&&. journalProp opts journal,
      testSuccessProp = \opts journal val state -> testSuccessProp test opts journal val state .&&. journalProp opts journal
    }

-- | Appends a requirements over the resulting 'UtxoState', which will need to
-- be satisfied both in case of success or failure of the run.
withStateProp :: (IsProp prop) => Test effs a prop -> StateProp prop -> Test effs a prop
withStateProp test stateProp =
  test
    { testFailureProp = \opts journal err state -> testFailureProp test opts journal err state .&&. stateProp opts state,
      testSuccessProp = \opts journal val state -> testSuccessProp test opts journal val state .&&. stateProp opts state
    }

-- | Appends a requirement over the resulting value and state of the mockchain
-- run which will need to be satisfied if the run is successful
withSuccessProp :: (IsProp prop) => Test effs a prop -> SuccessProp a prop -> Test effs a prop
withSuccessProp test successProp =
  test
    { testSuccessProp = \opts journal val state -> testSuccessProp test opts journal val state .&&. successProp opts journal val state
    }

-- | Same as 'withSuccessProp' but only considers the returning value of the run
withResultProp :: (IsProp prop) => Test effs a prop -> (a -> prop) -> Test effs a prop
withResultProp test p = withSuccessProp test (\_ _ res _ -> p res)

-- | Appends a requirement over the resulting number of outcomes of the run
withSizeProp :: (IsProp prop) => Test effs a prop -> SizeProp prop -> Test effs a prop
withSizeProp test reqSize =
  test
    { testSizeProp = \size -> testSizeProp test size .&&. reqSize size
    }

-- | Appends a requirement over the resulting value and state of the mockchain
-- run which will need to be satisfied if the run is successful
withFailureProp :: (IsProp prop) => Test effs a prop -> FailureProp prop -> Test effs a prop
withFailureProp test failureProp = test {testFailureProp = \opts journal err state -> testFailureProp test opts journal err state .&&. failureProp opts journal err state}

-- | Same as 'withFailureProp' but only considers the returning error of the run
withErrorProp :: (IsProp prop) => Test effs a prop -> (MockChainError -> prop) -> Test effs a prop
withErrorProp test errorProp = withFailureProp test (\_ _ err _ -> errorProp err)

-- * Specific properties around failures

-- | A property to ensure a phase 1 failure
isPhase1Failure :: (IsProp prop) => FailureProp prop
isPhase1Failure _ _ (MCEValidationError Ledger.Phase1 _) _ = testSuccess
isPhase1Failure pcOpts _ e _ = testFailureMsg $ "Expected phase 1 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | A property to ensure a phase 2 failure
isPhase2Failure :: (IsProp prop) => FailureProp prop
isPhase2Failure _ _ (MCEValidationError Ledger.Phase2 _) _ = testSuccess
isPhase2Failure pcOpts _ e _ = testFailureMsg $ "Expected phase 2 evaluation failure, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Same as 'isPhase1Failure' with an added predicate on the text error
isPhase1FailureWithMsg :: (IsProp prop) => String -> FailureProp prop
isPhase1FailureWithMsg s _ _ (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) _ | s `isInfixOf` T.unpack text = testSuccess
isPhase1FailureWithMsg _ pcOpts _ e _ = testFailureMsg $ "Expected phase 1 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- | Same as 'isPhase2Failure' with an added predicate over the text error
isPhase2FailureWithMsg :: (IsProp prop) => String -> FailureProp prop
isPhase2FailureWithMsg s _ _ (MCEValidationError Ledger.Phase2 (Ledger.ScriptFailure (Ledger.EvaluationError texts _))) _ | any (isInfixOf s . T.unpack) texts = testSuccess
isPhase2FailureWithMsg _ pcOpts _ e _ = testFailureMsg $ "Expected phase 2 evaluation failure with constrained messages, got: " ++ renderString (prettyCookedOpt pcOpts) e

-- * Specific properties around number of outcomes

-- | Ensures the run has an exact given number of outcomes
isOfSize :: (IsProp prop) => Integer -> SizeProp prop
isOfSize n1 n2 | n1 == n2 = testSuccess
isOfSize n1 n2 = testFailureMsg $ "Incorrect number of results (expected: " <> show n1 <> " but got: " <> show n2 <> ")"

-- | Ensures the run has a minimal number of outcomes
isAtLeastOfSize :: (IsProp prop) => Integer -> SizeProp prop
isAtLeastOfSize n1 n2 | n1 <= n2 = testSuccess
isAtLeastOfSize n1 n2 = testFailureMsg $ "Incorrect number of results (expected at least: " <> show n1 <> " but got: " <> show n2 <> ")"

-- | Ensures the run has a minimal number of outcomes
isAtMostOfSize :: (IsProp prop) => Integer -> SizeProp prop
isAtMostOfSize n1 n2 | n1 >= n2 = testSuccess
isAtMostOfSize n1 n2 = testFailureMsg $ "Incorrect number of results (expected at most: " <> show n1 <> " but got: " <> show n2 <> ")"

-- * Specific properties over the journal

-- | Ensures a certain event has been emitted. This uses the constructor's name
-- of the 'MockChainLogEntry' by relying on 'show' being lazy.
happened :: (IsProp prop) => String -> JournalProp prop
happened eventName _ journal
  | allEventNames <- Set.fromList (head . words . show <$> journal) =
      if eventName `Set.member` allEventNames
        then testSuccess
        else testFailureMsg $ "The event " <> show eventName <> " did not occur (but those did: " <> show allEventNames <> ")"

-- | Ensures a certain event has not been emitted. This uses the constructor's
-- name of the 'MockChainLogEntry' by relying on 'show' being lazy.
didNotHappen :: (IsProp prop) => String -> JournalProp prop
didNotHappen eventName _ journal | not (eventName `Set.member` Set.fromList (head . words . show <$> journal)) = testSuccess
didNotHappen eventName _ _ = testFailureMsg $ "The event " <> show eventName <> " was forbidden but occurred nonetheless"

-- * Specific properties over successes

-- | Ensures that the given addresses satisfy certain amount requirements over a
-- list of given asset classes in the end of the run
isAtAddress :: (IsProp prop, Script.ToAddress addr, Show addr) => [(addr, [(Api.AssetClass, Integer -> Bool)])] -> SuccessProp a prop
isAtAddress addressesReqs _ _ _ utxoState =
  testAll
    ( \(w, assetsReqs) ->
        let ownedValue = holdsInState w utxoState
         in testAll
              ( \(ac, nbReq) ->
                  let amount = Api.assetClassValueOf ownedValue ac
                   in if nbReq amount
                        then testSuccess
                        else testFailureMsg $ "Unsatisfied quantity requirement for " <> show w <> " over asset class " <> show ac
              )
              assetsReqs
    )
    addressesReqs

-- | Ensures that a given address possesses exactly a certain amount of a given
-- asset class in the end of the run
possesses :: (IsProp prop, Script.ToAddress addr, Show addr) => addr -> Api.AssetClass -> Integer -> SuccessProp a prop
possesses w ac n = isAtAddress [(w, [(ac, (== n))])]

-- * Advanced test templates

{--
  Note on advanced templates:

  The idea here is definely not to have a huge number of combinations of
  predicates and bundle them into templates. This has been attempted in the past
  and is never a good idea. However, there are a few more advanced template that
  make sense because they will occur a lot when testing smart contracts. Only
  those should appear in the following list. For all the other templates, the
  pattern @testCaseCookedXX $ template testName trace `withXXX` myPropicate@
  should be advocated.

--}

-- | A test template which expects a Phase 2 failure
mustFailInPhase2Test :: (IsProp prop) => Sem effs a -> Test effs a prop
mustFailInPhase2Test trace = mustFailTest trace `withFailureProp` isPhase2Failure

-- | A test template which expects a specific phase 2 error message
mustFailInPhase2WithMsgTest :: (IsProp prop) => String -> Sem effs a -> Test effs a prop
mustFailInPhase2WithMsgTest msg trace = mustFailTest trace `withFailureProp` isPhase2FailureWithMsg msg

-- | A test template which expects a Phase 1 failure
mustFailInPhase1Test :: (IsProp prop) => Sem effs a -> Test effs a prop
mustFailInPhase1Test trace = mustFailTest trace `withFailureProp` isPhase1Failure

-- | A test template which expects a specific phase 1 error message
mustFailInPhase1WithMsgTest :: (IsProp prop) => String -> Sem effs a -> Test effs a prop
mustFailInPhase1WithMsgTest msg trace = mustFailTest trace `withFailureProp` isPhase1FailureWithMsg msg

-- | A test template which expects a certain number of successful outcomes
mustSucceedWithSizeTest :: (IsProp prop) => Integer -> Sem effs a -> Test effs a prop
mustSucceedWithSizeTest size trace = mustSucceedTest trace `withSizeProp` (testBool . (== size))

-- | A test template which expects a certain number of unsuccessful outcomes
mustFailWithSizeTest :: (IsProp prop) => Integer -> Sem effs a -> Test effs a prop
mustFailWithSizeTest size trace = mustFailTest trace `withSizeProp` isOfSize size
