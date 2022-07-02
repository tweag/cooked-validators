module Language.Pirouette.PlutusIR.Runner where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as M
import Data.String
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
import Pirouette.Transformations.Contextualize
import qualified PlutusTx.Code as Pl
import Test.Tasty.HUnit

-- | Runs pirouette on some 'Pl.CompiledCode'
pirouetteCompiledCode' ::
  StoppingCondition ->
  Pl.CompiledCode a ->
  PrtUnorderedDefs PlutusIR ->
  AssumeProve PlutusIR ->
  Assertion
pirouetteCompiledCode' stop code (PrtUnorderedDefs augments) (toAssume :==>: toProve) =
  case Pl.getPir code of
    Nothing -> assertFailure "Can't getPir of the provided code"
    Just pir ->
      case runExcept $ trProgram pir of
        Left err0 -> assertFailure $ "Error translating pir: " ++ show err0
        Right (main0, decls0) ->
          -- We try to remove as many 'nameUnique' bits as possible, to help with
          -- writing predicates and referring to types.
          let (decls1, main) = declsUniqueNames (augments `M.union` decls0) main0
              -- TODO: what if we don't manage to infer the type of main? Also, can't we make this interface much better? These empty lists are awkward here.
              Right (Just mainTy) = runExcept $ runReaderT (typeInferTerm main) ((decls1, []), [])
              (mainTyArgs, mainTyRes) = SystF.tyFunArgs mainTy

              -- If @main :: Parms -> Datum -> Redeemer -> Ctx -> RES@, then the type of our toAssume and toProve terms is:
              --
              -- > RES -> Parms -> Datum -> Redeemer -> Bool
              --
              ty = SystF.TyFun mainTyRes (foldr SystF.TyFun (SystF.TyPure $ SystF.Free (TyBuiltin PIRTypeBool)) mainTyArgs)

              -- Because we might use higher order or polymorphic functions in our assume and prove definitons, we will actually reify
              -- them into definitions and put them in the context, this will make sure that we will transform them accordingly
              assumeName = fromString "__toAssume"
              assumeTerm = runReader (contextualizeTerm toAssume) (PrtUnorderedDefs decls1)
              assumeDef = DFunDef FunDef {funIsRec = NonRec, funBody = assumeTerm, funTy = ty}

              proveName = fromString "__toProve"
              proveTerm = runReader (contextualizeTerm toProve) (PrtUnorderedDefs decls1)
              proveDef = DFunDef FunDef {funIsRec = NonRec, funBody = proveTerm, funTy = ty}

              -- The final declarations are:
              decls = M.insert (TermNamespace, assumeName) assumeDef $ M.insert (TermNamespace, proveName) proveDef decls1

              -- Finally, we prepare the following problem to send to the solver
              ip =
                IncorrectnessParams
                  { ipTarget = main,
                    ipTargetType = mainTyRes,
                    ipCondition =
                      SystF.termPure (SystF.Free (TermSig assumeName))
                        :==>: SystF.termPure (SystF.Free (TermSig proveName))
                  }

              -- And call everything
              res = runIdentity $ execIncorrectnessLogic (proveAny stop isCounter) (PrtUnorderedDefs decls) ip
           in case res of
                Just Path {pathResult = CounterExample _ model} -> do
                  assertFailure $ "Counterexample:\n" ++ show (pretty model)
                Just _ -> error "Should never happen"
                Nothing -> return () -- success, no counter found anywhere.
  where
    isCounter Path {pathResult = CounterExample _ _, pathStatus = s}
      | s /= OutOfFuel = True
    isCounter _ = False

{-
Hook options to tasty:

data PirouetteStoppingCondition = PirouetteStoppingCondition StoppingCondition
  deriving (Typeable)

instance IsOption PirouetteStoppingCondition where
  ...
-}
