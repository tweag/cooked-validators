{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Defines the 'LanguageBuiltinTypes' instance for 'PlutusIR',
-- the only export of this module is that instance.
module Language.Pirouette.PlutusIR.Typing () where

import Data.String (fromString)
import Language.Pirouette.PlutusIR.Syntax
import Pirouette.Term.Syntax
import qualified Pirouette.Term.Syntax.SystemF as SystF

-- | Shortcut for system F arrows
infixr 2 :->:

pattern (:->:) :: Type PlutusIR -> Type PlutusIR -> Type PlutusIR
pattern (:->:) x y = SystF.TyFun x y

-- | Helper to lift PIR builtin types to system F
systfType :: PIRBuiltinType -> Type PlutusIR
systfType = SystF.TyPure . SystF.Free . TyBuiltin

-- Shortcuts for PIR builtin types in systemF
tInt, tBool, tByteString, tString :: Type PlutusIR
tInt = systfType PIRTypeInteger
tBool = systfType PIRTypeBool
tByteString = systfType PIRTypeByteString
tString = systfType PIRTypeString

-- | Shortcuts for type variables
tVar :: String -> Integer -> Type PlutusIR
tVar name deBruijn = SystF.TyPure $ SystF.Bound (SystF.Ann (fromString name)) deBruijn

-- | "Forall" type shortcut helper for types of kind *
forall :: SystF.Ann (SystF.Ann ann) -> SystF.AnnType ann tyVar -> SystF.AnnType ann tyVar
forall x = SystF.TyAll (SystF.ann x) SystF.KStar

instance LanguageBuiltinTypes PlutusIR where
  typeOfConstant = cstToBuiltinType

  typeOfBuiltin AddInteger = tInt :->: tInt :->: tInt
  typeOfBuiltin SubtractInteger = tInt :->: tInt :->: tInt
  typeOfBuiltin MultiplyInteger = tInt :->: tInt :->: tInt
  typeOfBuiltin DivideInteger = tInt :->: tInt :->: tInt
  typeOfBuiltin ModInteger = tInt :->: tInt :->: tInt
  typeOfBuiltin QuotientInteger = tInt :->: tInt :->: tInt
  typeOfBuiltin RemainderInteger = tInt :->: tInt :->: tInt
  typeOfBuiltin EqualsInteger = tInt :->: tInt :->: tBool
  typeOfBuiltin LessThanInteger = tInt :->: tInt :->: tBool
  typeOfBuiltin LessThanEqualsInteger = tInt :->: tInt :->: tBool
  typeOfBuiltin EqualsString = tString :->: tString :->: tBool
  typeOfBuiltin EqualsByteString = tByteString :->: tByteString :->: tBool
  typeOfBuiltin IfThenElse = forall "a" (tBool :->: tVar "a" 0 :->: tVar "a" 0 :->: tVar "a" 0)
  typeOfBuiltin Trace = forall "a" (tString :->: tVar "a" 0 :->: tVar "a" 0)
  typeOfBuiltin FstPair = forall "a" (forall "b" (tyTuple2Of (tVar "a" 1) (tVar "b" 0) :->: tVar "a" 1))
  typeOfBuiltin SndPair = forall "a" (forall "b" (tyTuple2Of (tVar "a" 1) (tVar "b" 0) :->: tVar "b" 0))
  -- https://github.com/input-output-hk/plutus/blob/3c4067bb96251444c43ad2b17bc19f337c8b47d7/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs#L1009
  typeOfBuiltin ChooseList =
    forall "a" (forall "b" (tyListOf (tVar "a" 1) :->: tVar "b" 0 :->: tVar "b" 0 :->: tVar "b" 0))
  typeOfBuiltin HeadList = forall "a" (tyListOf (tVar "a" 0) :->: tVar "a" 0)
  typeOfBuiltin TailList = forall "a" (tyListOf (tVar "a" 0) :->: tyListOf (tVar "a" 0))
  -- https://github.com/input-output-hk/plutus/blob/3c4067bb96251444c43ad2b17bc19f337c8b47d7/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs#L1075
  typeOfBuiltin ChooseData =
    forall "a" (tyData :->: tVar "a" 0 :->: tVar "a" 0 :->: tVar "a" 0 :->: tVar "a" 0 :->: tVar "a" 0 :->: tVar "a" 0)
  typeOfBuiltin UnConstrData = tyData :->: tyTuple2Of tInt (tyListOf tyData)
  typeOfBuiltin UnIData = tyData :->: tInt
  typeOfBuiltin UnBData = tyData :->: tByteString
  typeOfBuiltin MkNilData = tyData
  -- TODO: implement the types of other builtins, but make sure to always bring in a golden
  -- test that comes from the plutus compiler. We should REALLY not be guessing these types,
  -- no matter how simple they seem.
  typeOfBuiltin builtin = error $ "typeOfBuiltin " ++ show builtin ++ " is not implemented"

  -- The type of bottom in PlutusIR is similar to Haskell; we translate @PIR.Error loc ty@
  -- to @Free Bottom `App` [TyArg ty]@.
  typeOfBottom = forall "a" (tVar "a" 0)
