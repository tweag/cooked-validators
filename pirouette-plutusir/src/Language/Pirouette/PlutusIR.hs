{-# LANGUAGE TypeApplications #-}

module Language.Pirouette.PlutusIR (module X, pir, pirTy, pirDecls) where

import Language.Pirouette.PlutusIR.Runner.Tasty as X
import Language.Pirouette.PlutusIR.SMT as X
import Language.Pirouette.PlutusIR.Syntax as X
import Language.Pirouette.PlutusIR.ToTerm as X (trProgram, trProgramDecls)
import Language.Pirouette.PlutusIR.Typing as X ()
import qualified Language.Pirouette.QuasiQuoter as QQ

pir :: QQ.QuasiQuoter
pir = QQ.term @PlutusIR

pirDecls :: QQ.QuasiQuoter
pirDecls = QQ.progNoTC @PlutusIR

pirTy :: QQ.QuasiQuoter
pirTy = QQ.ty @PlutusIR
