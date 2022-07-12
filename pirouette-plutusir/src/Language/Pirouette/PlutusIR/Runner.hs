{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Pirouette.PlutusIR.Runner where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Default
import qualified Data.List as L
import qualified Data.Map as M
import Data.String
import Language.Pirouette.PlutusIR.Prelude
import Language.Pirouette.PlutusIR.SMT ()
import Language.Pirouette.PlutusIR.Syntax
import Language.Pirouette.PlutusIR.ToTerm
import Language.Pirouette.PlutusIR.Typing ()
import Pirouette
import Pirouette.Monad
import Pirouette.Symbolic.Eval hiding (Options)
import qualified Pirouette.Symbolic.Eval as P (Options)
import Pirouette.Symbolic.Prover
import Pirouette.Term.Syntax
import qualified Pirouette.Term.Syntax.SystemF as SystF
import Pirouette.Term.TypeChecker
import Pirouette.Transformations
import qualified PlutusCore.Pretty as Pl
import qualified PlutusTx.Code as Pl
import Test.Tasty.HUnit

-- | Set of options to control the particular run of pirouette
data Options = Options
  { optsPirouette :: P.Options,
    optsDumpIntermediate :: Maybe ([String], FilePath)
  }

instance Default Options where
  def = Options def Nothing

-- | Runs pirouette on some 'Pl.CompiledCode' with some 'Options'. You should likely be using
-- the interface to pirouette from "Language.Pirouette.PlutusIR.Runner.Tasty" instead, which
-- will be a little more ergonomic.
pirouetteCompiledCodeOpts ::
  Options ->
  Pl.CompiledCode a ->
  PrtUnorderedDefs PlutusIR ->
  AssumeProve PlutusIR ->
  Assertion
pirouetteCompiledCodeOpts opts code (PrtUnorderedDefs augments) (toAssume :==>: toProve) = do
  (main0, preDecls) <- pirouetteDeclsFromCompiledCode code
  let PrtUnorderedDefs decls0 = complementWithBuiltinPrelude $ PrtUnorderedDefs preDecls

  -- first we contextualize the user declarations, making sure they will refer to their right
  -- counterparts in  @decls0@.
  let augments' = runReader (contextualizeDecls augments) (PrtUnorderedDefs decls0)
  -- Now we try to remove as many 'nameUnique' bits as possible, to help with writing
  -- predicates and referring to types.
  let (decls1, main) = (augments' `M.union` decls0, main0)

  -- PrtUnorderedDefs decls1 = prenex $ PrtUnorderedDefs decls0'
  -- TODO: what if we don't manage to infer the type of main? Also, can't we make this interface much better? These empty lists are awkward here.
  let Right (Just mainTy) = runExcept $ runReaderT (typeInferTerm main) ((decls1, []), [])
  let mainName = fromString "__main"
  let mainDef = DFunDef FunDef {funIsRec = NonRec, funBody = main, funTy = mainTy}
  let (mainTyArgs, mainTyRes) = SystF.tyFunArgs mainTy

  -- If @main :: Parms -> Datum -> Redeemer -> Ctx -> RES@, then the type of our toAssume and toProve terms is:
  --
  -- > RES -> Parms -> Datum -> Redeemer -> Bool
  --
  let bool = SystF.TyPure $ SystF.Free (TyBuiltin PIRTypeBool)
  let ty = SystF.TyFun bool (foldr SystF.TyFun bool mainTyArgs)

  -- Because we might use higher order or polymorphic functions in our assume and prove definitons, we will actually reify
  -- them into definitions and put them in the context, this will make sure that we will transform them accordingly
  let assumeName = fromString "__toAssume"
  let assumeTerm = runReader (contextualizeTerm toAssume) (PrtUnorderedDefs decls1)
  let assumeDef = DFunDef FunDef {funIsRec = NonRec, funBody = assumeTerm, funTy = ty}

  let proveName = fromString "__toProve"
  let proveTerm = runReader (contextualizeTerm toProve) (PrtUnorderedDefs decls1)
  let proveDef = DFunDef FunDef {funIsRec = NonRec, funBody = proveTerm, funTy = ty}

  -- The final declarations are:
  let decls =
        M.insert (TermNamespace, mainName) mainDef $
          M.insert (TermNamespace, assumeName) assumeDef $
            M.insert (TermNamespace, proveName) proveDef decls1

  -- At this point, we're almost ready to call the prover. It does help tp ensure that our
  -- definitions are type-correct, though!
  case typeCheckDecls decls of
    Left tyerr -> assertFailure $ show $ pretty tyerr
    Right _ -> do
      let fpath = maybe "" snd $ optsDumpIntermediate opts
      let dump = fst <$> optsDumpIntermediate opts
      orderedDecls <- runStages dump (pirouetteBoundedSymExecStages fpath) (PrtUnorderedDefs decls)
      res <- flip runReaderT orderedDecls $ do
        fn' <- prtTermDefOf mainName
        assume' <- prtTermDefOf assumeName
        toProve' <- prtTermDefOf proveName
        proveAny (optsPirouette opts) isCounter (Problem mainTyRes fn' assume' toProve')
      case res of
        Just Path {pathResult = CounterExample _ model} -> do
          assertFailure $ "Counterexample:\n" ++ show (pretty model)
        Just _ -> error "Should never happen"
        Nothing -> return () -- success, no counter found anywhere.
  where
    isCounter Path {pathResult = CounterExample _ _, pathStatus = s}
      | s /= OutOfFuel = True
    isCounter _ = False

{- ORMOLU_DISABLE -}
-- | Defines the stages a pirouette program goes through; the first stage is the identidy to enable us
--  to easily print the set of definitions that come from translating a PlutusIR program into a
--  pirouette one, before the set of definitions that happen in between.
pirouetteBoundedSymExecStages ::
  -- | The filepath to dump stages to, if the user requires that when running
  FilePath ->
  Stages (PrtUnorderedDefs PlutusIR) (PrtOrderedDefs PlutusIR)
pirouetteBoundedSymExecStages fpath =
  Comp "init" id dumpUDefs $
  Comp "monomorphize" monomorphize dumpUDefs $
  Comp "defunctionalize" defunctionalize dumpUDefs $
  Comp "cycle-elim" elimEvenOddMutRec dumpOrdDefs
  Id
  where
    typecheck :: String -> Decls PlutusIR -> IO ()
    typecheck stageName prog =
      case typeCheckDecls prog of
        Left tyerr -> assertFailure ("Result of stage " ++ stageName ++ " has type errors:\n" ++ show (pretty tyerr))
        Right _ -> return ()

    dumpUDefs :: String -> PrtUnorderedDefs PlutusIR -> IO ()
    dumpUDefs stageName (PrtUnorderedDefs prog) = do
      writeFile (fpath ++ "-" ++ stageName) $ show $ pretty prog
      typecheck stageName prog

    -- Similar to dumpUDefs, but dumps things in order
    dumpOrdDefs :: String -> PrtOrderedDefs PlutusIR -> IO ()
    dumpOrdDefs stageName (PrtOrderedDefs prog ord) = do
      let ordProg = map (\arg -> runReader (prtDefOf (argNamespace arg) (argName arg)) (PrtUnorderedDefs prog)) ord
      writeFile (fpath ++ "-" ++ stageName) $ show $ pretty ordProg
      typecheck stageName prog
      where
        argNamespace = SystF.argElim (const TypeNamespace) (const TermNamespace)
        argName = SystF.argElim id id
{- ORMOLU_ENABLE -}

-- * Auxiliar Definitions

data Stages a b where
  Id :: Stages a a
  Comp :: String -> (a -> b) -> (String -> b -> IO ()) -> Stages b c -> Stages a c

runStages :: (MonadIO m) => Maybe [String] -> Stages a b -> a -> m b
runStages _dump Id a = return a
runStages dump (Comp stageName f dbg rest) a = do
  let b = f a
  when shouldDebug $ liftIO $ dbg stageName b
  runStages dump rest b
  where
    shouldDebug =
      case dump of
        Nothing -> False
        Just [] -> True
        Just prefs -> any (`L.isPrefixOf` stageName) prefs

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

{-
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
  case typeCheckDecls (prtUODecls prog1) of
    Left tyerr -> liftIO $ assertFailure $ ("prog1: " ++) $ show $ pretty tyerr
    Right _ -> liftIO (putStrLn "monomorphize type checks...")
  liftIO (writeFile "prog1-mono" $ show $ pretty $ prtUODecls prog1)
  let prog2 = defunctionalize prog1
  case typeCheckDecls (prtUODecls prog1) of
    Left tyerr -> liftIO $ assertFailure $ ("prog2: " ++) $ show $ pretty tyerr
    Right _ -> liftIO (putStrLn "defunctionalizaton type checks...")
  liftIO (writeFile "prog2-defun" $ show $ pretty $ prtUODecls prog2)
  let prog3 = elimEvenOddMutRec prog2
  liftIO (writeFile "prog3-ord" $ unlines $ map show $ prtDepOrder prog3)
  liftIO (writeFile "prog3-nomr" $ show $ pretty $ prtDecls prog3)
  let orderedDecls = prog3
  -- Now we can contextualize the necessary terms and run the worker
-}
