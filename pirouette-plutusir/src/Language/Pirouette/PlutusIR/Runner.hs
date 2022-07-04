{-# LANGUAGE FlexibleContexts #-}

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
import Pirouette.Transformations
import Pirouette.Transformations.Contextualize
import qualified PlutusCore.Pretty as Pl
import qualified PlutusTx.Code as Pl
import Test.Tasty.HUnit

-- | Returns the main term and decls from a 'Pl.CompiledCode'. Runs in @IO@ because
--  we call 'assertFailure' if anything fails.
pirouetteDeclsFromCompiledCode :: Pl.CompiledCode a -> IO (Term PlutusIR, Decls PlutusIR)
pirouetteDeclsFromCompiledCode code =
  case Pl.getPir code of
    Nothing -> assertFailure "Can't getPir of the provided code"
    Just pir -> do
      writeFile "prog.pir" (show $ Pl.prettyClassicDebug pir)
      case runExcept $ trProgram pir of
        Left err0 -> assertFailure $ "Error translating pir: " ++ show err0
        Right res -> return res

-- | Runs pirouette on some 'Pl.CompiledCode'
pirouetteCompiledCode' ::
  StoppingCondition ->
  Pl.CompiledCode a ->
  PrtUnorderedDefs PlutusIR ->
  AssumeProve PlutusIR ->
  Assertion
pirouetteCompiledCode' stop code (PrtUnorderedDefs augments) (toAssume :==>: toProve) = do
  (main0, decls0) <- pirouetteDeclsFromCompiledCode code

  -- first we contextualize the user declarations, making sure they will refer to their right
  -- counterparts in  @decls0@.
  let augments' = runReader (contextualizeDecls augments) (PrtUnorderedDefs decls0)
  -- Now we try to remove as many 'nameUnique' bits as possible, to help with writing
  -- predicates and referring to types.
  let (decls1, main) = declsUniqueNames (augments' `M.union` decls0) main0

  -- PrtUnorderedDefs decls1 = prenex $ PrtUnorderedDefs decls0'
  -- TODO: what if we don't manage to infer the type of main? Also, can't we make this interface much better? These empty lists are awkward here.
  let Right (Just mainTy) = runExcept $ runReaderT (typeInferTerm main) ((decls1, []), [])
  let (mainTyArgs, mainTyRes) = SystF.tyFunArgs mainTy

  -- If @main :: Parms -> Datum -> Redeemer -> Ctx -> RES@, then the type of our toAssume and toProve terms is:
  --
  -- > RES -> Parms -> Datum -> Redeemer -> Bool
  --
  let ty = SystF.TyFun mainTyRes (foldr SystF.TyFun (SystF.TyPure $ SystF.Free (TyBuiltin PIRTypeBool)) mainTyArgs)

  -- Because we might use higher order or polymorphic functions in our assume and prove definitons, we will actually reify
  -- them into definitions and put them in the context, this will make sure that we will transform them accordingly
  let assumeName = fromString "__toAssume"
  let assumeTerm = runReader (contextualizeTerm toAssume) (PrtUnorderedDefs decls1)
  let assumeDef = DFunDef FunDef {funIsRec = NonRec, funBody = assumeTerm, funTy = ty}

  let proveName = fromString "__toProve"
  let proveTerm = runReader (contextualizeTerm toProve) (PrtUnorderedDefs decls1)
  let proveDef = DFunDef FunDef {funIsRec = NonRec, funBody = proveTerm, funTy = ty}

  -- The final declarations are:
  let decls = M.insert (TermNamespace, assumeName) assumeDef $ M.insert (TermNamespace, proveName) proveDef decls1

  -- Finally, we prepare the following problem to send to the solver
  let ip =
        IncorrectnessParams
          { ipTarget = main,
            ipTargetType = mainTyRes,
            ipCondition =
              SystF.termPure (SystF.Free (TermSig assumeName))
                :==>: SystF.termPure (SystF.Free (TermSig proveName))
          }

  -- At this point, we're almost ready to call the prover. It does help tp ensure that our
  -- definitions are type-correct, though!
  case typeCheckDecls decls of
    Left tyerr -> assertFailure $ show $ pretty tyerr
    Right _ -> do
      res <- gogogo (proveAny stop isCounter) (PrtUnorderedDefs decls) ip
      case res of
        Just Path {pathResult = CounterExample _ model} -> do
          assertFailure $ "Counterexample:\n" ++ show (pretty model)
        Just _ -> error "Should never happen"
        Nothing -> return () -- success, no counter found anywhere.
  where
    isCounter Path {pathResult = CounterExample _ _, pathStatus = s}
      | s /= OutOfFuel = True
    isCounter _ = False

-- was execIncorrectnessLogic, but I'm needing to dump the intermediate steps
gogogo ::
  (Language lang, LanguageBuiltinTypes lang, MonadIO m) =>
  -- | Worker to run on the processed definitions
  (Problem lang -> ReaderT (PrtOrderedDefs lang) m res) ->
  PrtUnorderedDefs lang ->
  IncorrectnessParams lang ->
  m res
gogogo toDo prog0 (IncorrectnessParams fn ty (assume :==>: toProve)) = do
  -- First, we apply a series of transformations to our program that will enable
  -- us to translate it to SMTLIB later on
  liftIO (writeFile "prog0" $ show $ pretty $ prtUODecls prog0)
  let prog1 = monomorphize prog0
  liftIO (writeFile "prog1-mono" $ show $ pretty $ prtUODecls prog1)
  let prog2 = defunctionalize prog1
  liftIO (writeFile "prog2-defun" $ show $ pretty $ prtUODecls prog2)
  let prog3 = elimEvenOddMutRec prog2
  liftIO (writeFile "prog3-ord" $ unlines $ map show $ prtDepOrder prog3)
  liftIO (writeFile "prog3-nomr" $ show $ pretty $ prtDecls prog3)
  let orderedDecls = prog3
  -- Now we can contextualize the necessary terms and run the worker
  flip runReaderT orderedDecls $ do
    ty' <- contextualizeType ty
    fn' <- contextualizeTerm fn
    assume' <- contextualizeTerm assume
    toProve' <- contextualizeTerm toProve
    toDo (Problem ty' fn' assume' toProve')

{-
Hook options to tasty:

data PirouetteStoppingCondition = PirouetteStoppingCondition StoppingCondition
  deriving (Typeable)

instance IsOption PirouetteStoppingCondition where
  ...
-}
