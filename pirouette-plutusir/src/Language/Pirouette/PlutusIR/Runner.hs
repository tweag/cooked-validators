module Language.Pirouette.PlutusIR.Runner where

import Control.Monad.Except
import Language.Pirouette.PlutusIR.Prelude ()
import Language.Pirouette.PlutusIR.SMT ()
import Language.Pirouette.PlutusIR.Syntax
import Language.Pirouette.PlutusIR.ToTerm
import Language.Pirouette.PlutusIR.Typing ()
import Pirouette
import Pirouette.Monad
import qualified Pirouette.Runner as P
import Pirouette.Term.Syntax
import qualified PlutusCore.Pretty as Pl
import qualified PlutusTx.Code as Pl
import Test.Tasty.HUnit

-- | Runs pirouette on some 'Pl.CompiledCode' with some 'Options'. You should likely be using
-- the interface to pirouette from "Language.Pirouette.PlutusIR.Runner.Tasty" instead, which
-- will be a little more ergonomic.
pirouetteCompiledCodeOpts ::
  P.RunOptions ->
  Pl.CompiledCode a ->
  PrtUnorderedDefs PlutusIR ->
  AssumeProve PlutusIR ->
  Assertion
pirouetteCompiledCodeOpts opts code augments assumeProve = do
  (main0, preDecls) <- pirouetteDeclsFromCompiledCode code
  P.runPirouette opts augments assumeProve main0 preDecls

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
