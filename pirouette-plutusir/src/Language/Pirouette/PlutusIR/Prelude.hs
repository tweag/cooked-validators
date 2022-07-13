{-# LANGUAGE QuasiQuotes #-}

module Language.Pirouette.PlutusIR.Prelude where

import Language.Pirouette.PlutusIR.QuasiQuoter
import Language.Pirouette.PlutusIR.Syntax
import Pirouette.Monad

-- The functions from this prelude are implementations of Plutus builtins
-- in terms of raw pirouette; they are based on:
-- https://github.com/input-output-hk/plutus/blob/3c4067bb96251444c43ad2b17bc19f337c8b47d7/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs#L1009

instance LanguagePrelude PlutusIR where
  builtinPrelude =
    [pirDeclsWithTC|

-- * Booleans

data Bool
  = True : Bool
  | False : Bool
  destructor Bool_match

fun ifThenElse : all (res : Type) . Bool -> res -> res -> res
  = /\(res : Type) . \(cond : Bool) (th : res) (el : res) . Bool_match cond @res th el

-- * Lists

data List (a : Type)
  = Nil : List a
  | Cons : a -> List a -> List a
  destructor Nil_match

fun chooseList : all (a : Type) (b : Type) . List a -> b -> b -> b
  = /\(a : Type) (b : Type) . \(x : List a) (caseNil : b) (caseCons : b)
  . Nil_match @a x @b
      caseNil
      (\(hd : a) (tl : List a) . caseCons)

fun tailList : all (a : Type) . List a -> List a
  = /\(a : Type) . \(x : List a)
  . Nil_match @a x @(List a)
      (bottom @(List a))
      (\(hd : a) (tl : List a) . tl)

fun headList : all (a : Type) . List a -> a
  = /\(a : Type) . \(x : List a)
  . Nil_match @a x @a
      (bottom @a)
      (\(hd : a) (tl : List a) . hd)

-- * Tuples

data Tuple2 (a : Type) (b : Type)
  = Tuple2 : a -> b -> Tuple2 a b
  destructor Tuple2_match

fun fstPair : all (a : Type) (b : Type) . Tuple2 a b -> a
  = /\(a : Type) (b : Type) . \(t : Tuple2 a b)
  . Tuple2_match @a @b t @a
      (\(e1 : a) (e2 : b) . e1)

fun sndPair : all (a : Type) (b : Type) . Tuple2 a b -> b
  = /\(a : Type) (b : Type) . \(t : Tuple2 a b)
  . Tuple2_match @a @b t @b
      (\(e1 : a) (e2 : b) . e2)

-- * Unit

data Unit = Unit : Unit
  destructor Unit_match

-- * Data

data Data
  = Constr : Integer -> List Data -> Data
  | Map : List (Tuple2 Data Data) -> Data
  | List : List Data -> Data
  | I : Integer -> Data
  | B : ByteString -> Data
  destructor Data_match

fun chooseData : all (a : Type) . Data -> a -> a -> a -> a -> a -> a
  = /\(a : Type) . \(d : Data) (constr : a) (map : a) (list : a) (i : a) (b : a)
  . Data_match d @a
      (\(m : Integer) (ms : List Data) . constr)
      (\(l : List (Tuple2 Data Data)) . map)
      (\(l : List Data) . list)
      (\(x : Integer) . i)
      (\(x : ByteString) . i)

fun unIData : Data -> Integer
  = \(d : Data) . Data_match d @Integer
      (\(m : Integer) (ms : List Data) . bottom @Integer)
      (\(l : List (Tuple2 Data Data)) . bottom @Integer)
      (\(l : List Data) . bottom @Integer)
      (\(x : Integer) . x)
      (\(x : ByteString) . bottom @Integer)

fun unConstrData : Data -> Tuple2 Integer (List Data)
  = \(d : Data) . Data_match d @(Tuple2 Integer (List Data))
      (\(m : Integer) (ms : List Data) . Tuple2 @Integer @(List Data) m ms)
      (\(l : List (Tuple2 Data Data)) . bottom @(Tuple2 Integer (List Data)))
      (\(l : List Data) . bottom @(Tuple2 Integer (List Data)))
      (\(x : Integer) . bottom @(Tuple2 Integer (List Data)))
      (\(x : ByteString) . bottom @(Tuple2 Integer (List Data)))

fun unBData : Data -> ByteString
  = \(d : Data) . Data_match d @ByteString
      (\(m : Integer) (ms : List Data) . bottom @ByteString)
      (\(l : List (Tuple2 Data Data)) . bottom @ByteString)
      (\(l : List Data) . bottom @ByteString)
      (\(x : Integer) . bottom @ByteString)
      (\(x : ByteString) . x)

fun mkNilData : Data = List (Nil @Data)


|]

{-
    -- only define List and Unit if they are not yet defined
    [("List", listTypeDef) | not (isDefined "List")]
      ++ [("Unit", unitTypeDef) | not (isDefined "Unit")]
      ++ [("Tuple2", tuple2TypeDef) | not (isDefined "Tuple2")]
      ++ [("Data", dataTypeDef)]
    where
      a = SystF.TyApp (SystF.Bound (SystF.Ann "a") 0) []

      isDefined nm = isJust (lookup nm definedTypes)

      listOf x = SystF.TyApp (SystF.Free $ TySig "List") [x]
      tuple2Of x y = SystF.TyApp (SystF.Free $ TySig "Tuple2") [x, y]
      builtin nm = SystF.TyApp (SystF.Free $ TyBuiltin nm) []

      listTypeDef =
        Datatype
          { kind = SystF.KTo SystF.KStar SystF.KStar,
            typeVariables = [("a", SystF.KStar)],
            destructor = "Nil_match",
            constructors =
              [ ("Nil", SystF.TyAll (SystF.Ann "a") SystF.KStar (listOf a)),
                ("Cons", SystF.TyAll (SystF.Ann "a") SystF.KStar (SystF.TyFun a (SystF.TyFun (listOf a) (listOf a))))
              ]
          }

      unitTypeDef =
        Datatype
          { kind = SystF.KStar,
            typeVariables = [],
            destructor = "Unit_match",
            constructors = [("Unit", SystF.TyPure (SystF.Free $ TySig "Unit"))]
          }

      -- !! warning, we define it as "Tuple2 b a" to reuse 'a' for both list and tuple
      b = SystF.TyApp (SystF.Bound (SystF.Ann "b") 1) []
      tuple2TypeDef =
        Datatype
          { kind = SystF.KTo SystF.KStar (SystF.KTo SystF.KStar SystF.KStar),
            typeVariables = [("b", SystF.KStar), ("a", SystF.KStar)],
            destructor = "Tuple2_match",
            constructors =
              [ ("Tuple2", SystF.TyAll (SystF.Ann "b") SystF.KStar $ SystF.TyAll (SystF.Ann "a") SystF.KStar $ SystF.TyFun b (SystF.TyFun a (tuple2Of b a)))
              ]
          }

      -- defined following https://github.com/input-output-hk/plutus/blob/master/plutus-core/plutus-core/src/PlutusCore/Data.hs
      dataTypeDef =
        Datatype
          { kind = SystF.KStar,
            typeVariables = [],
            destructor = "Data_match",
            constructors =
              [ ("Data_Constr", SystF.TyFun (builtin PIRTypeInteger) (SystF.TyFun (tyListOf tyData) tyData)),
                ("Data_Map", SystF.TyFun (tyListOf (tyTuple2Of tyData tyData)) tyData),
                ("Data_List", SystF.TyFun (tyListOf tyData) tyData),
                ("Data_I", SystF.TyFun (builtin PIRTypeInteger) tyData),
                ("Data_B", SystF.TyFun (builtin PIRTypeByteString) tyData)
              ]
          }
-}
