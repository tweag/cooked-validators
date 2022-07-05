module Language.Pirouette.PlutusIR.Runner.Tasty where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Data
import Data.Default
import qualified Data.Map as M
import Data.String
import Data.Tagged
import Language.Pirouette.PlutusIR.Runner
import Language.Pirouette.PlutusIR.SMT ()
import Language.Pirouette.PlutusIR.Syntax
import Language.Pirouette.PlutusIR.ToTerm
import Language.Pirouette.PlutusIR.Typing ()
import Pirouette
import Pirouette.Monad
import Pirouette.Symbolic.Eval
import Pirouette.Symbolic.Prover
import Pirouette.Symbolic.Prover.Runner
import Pirouette.Term.Syntax
import qualified Pirouette.Term.Syntax.SystemF as SystF
import Pirouette.Term.TypeChecker
import Pirouette.Transformations
import qualified PlutusCore.Pretty as Pl
import qualified PlutusTx.Code as Pl
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options

testBoundedSymExec ::
  String ->
  Pl.CompiledCode a ->
  PrtUnorderedDefs PlutusIR ->
  AssumeProve PlutusIR ->
  TestTree
testBoundedSymExec name code augments cond =
  askOption $ \(PirouetteStoppingCondition stop) ->
    askOption $ \(PirouetteSolverDebug dbg) ->
      askOption $ \dump ->
        _

-- * Tasty Options

newtype PirouetteStoppingCondition = PirouetteStoppingCondition StoppingCondition
  deriving (Typeable)

mkStoppingCondition :: (SymEvalStatistics -> Int) -> Int -> PirouetteStoppingCondition
mkStoppingCondition f limit = PirouetteStoppingCondition $ \st -> f st > limit

instance IsOption PirouetteStoppingCondition where
  defaultValue = PirouetteStoppingCondition $ stoppingCondition def
  parseValue ('c' : num) = mkStoppingCondition sestConstructors <$> safeRead num
  parseValue ('f' : num) = mkStoppingCondition sestConsumedFuel <$> safeRead num
  parseValue _ = Nothing

  optionName = Tagged "piroutte-stop"
  optionHelp = Tagged "When to stop pirouette's symbolic execution engine. Accepts the form cINT and fINT for specifying bounds on either sestConstructors or sestConsumedFuel"

newtype PirouetteSolverDebug = PirouetteSolverDebug Bool
  deriving (Typeable)

instance IsOption PirouetteSolverDebug where
  defaultValue = PirouetteSolverDebug False
  parseValue = safeReadBool

  optionName = Tagged "piroutte-solver-dbg"
  optionHelp = Tagged "If set to 'true', will echo all messages from the solver to stdout"

-- | Instructs our runner to dump the intermediate programs to a file. Passing @PirouetteDumpAny f l@
--  instructs the runner to dump the result of any stage whose name has a prefix in @l@.
--  This is an option indended for developers of pirouette and hence,
--  if not available through the command line: must be used with 'localOption'
data PirouetteDumpIntermediate
  = PirouetteDumpNothing
  | PirouetteDumpAny [String] FilePath
  deriving (Typeable)

instance IsOption PirouetteDumpIntermediate where
  defaultValue = PirouetteDumpNothing
  parseValue _ = Nothing

  optionName = Tagged "piroutte-dump-intermediate"
  optionHelp = Tagged "This options must be set with localOption; check the source for info"

{-
Hook options to tasty:

data PirouetteStoppingCondition  PirouetteStoppingCondition StoppingCondition
  deriving (Typeable)

instance IsOption PirouetteStoppingCondition where
  ...
-}
