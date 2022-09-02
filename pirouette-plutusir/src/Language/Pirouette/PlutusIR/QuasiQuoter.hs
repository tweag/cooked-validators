{-# LANGUAGE TypeApplications #-}

module Language.Pirouette.PlutusIR.QuasiQuoter where

import Language.Pirouette.PlutusIR.Syntax
import Language.Pirouette.PlutusIR.Typing ()
import qualified Language.Pirouette.QuasiQuoter as QQ

pir :: QQ.QuasiQuoter
pir = QQ.term @PlutusIR

pirDecls :: QQ.QuasiQuoter
pirDecls = QQ.progNoTC @PlutusIR

pirDeclsWithTC :: QQ.QuasiQuoter
pirDeclsWithTC = QQ.prog @PlutusIR

pirTy :: QQ.QuasiQuoter
pirTy = QQ.ty @PlutusIR
