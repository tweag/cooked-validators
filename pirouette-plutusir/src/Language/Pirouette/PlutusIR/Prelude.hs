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

ifThenElse : forall res . Bool -> res -> res -> res
ifThenElse @res cond th el = Bool_match cond @res th el

-- * Lists

data List (a : Type)
  = Nil : List a
  | Cons : a -> List a -> List a
  destructor Nil_match

chooseList : forall a b . List a -> b -> b -> b
chooseList @a @b x caseNil caseCons =
  Nil_match @a x @b
    caseNil
    (\(hd : a) (tl : List a) . caseCons)

tailList : forall a . List a -> List a
tailList @a x =
  Nil_match @a x @(List a)
    (bottom @(List a))
    (\(hd : a) (tl : List a) . tl)

headList : forall a . List a -> a
headList @a x =
  Nil_match @a x @a
      (bottom @a)
      (\(hd : a) (tl : List a) . hd)

-- * Tuples

data Tuple2 (a : Type) (b : Type)
  = Tuple2 : a -> b -> Tuple2 a b
  destructor Tuple2_match

fstPair : forall a b . Tuple2 a b -> a
fstPair @a @b t =
  Tuple2_match @a @b t @a
      (\(e1 : a) (e2 : b) . e1)

sndPair : forall a b . Tuple2 a b -> b
sndPair @a @b t =
  Tuple2_match @a @b t @b
      (\(e1 : a) (e2 : b) . e2)

-- * Unit

data Unit = Unit : Unit
  destructor Unit_match

chooseUnit : forall a . Unit -> a -> a
chooseUnit @a u x = x

-- * Data

data Data
  = Constr : Integer -> List Data -> Data
  | Map : List (Tuple2 Data Data) -> Data
  | List : List Data -> Data
  | I : Integer -> Data
  | B : ByteString -> Data
  destructor Data_match

chooseData : forall a . Data -> a -> a -> a -> a -> a -> a
chooseData @a d constr map list i b =
  Data_match d @a
      (\(m : Integer) (ms : List Data) . constr)
      (\(l : List (Tuple2 Data Data)) . map)
      (\(l : List Data) . list)
      (\(x : Integer) . i)
      (\(x : ByteString) . i)

unIData : Data -> Integer
unIData d =
  Data_match d @Integer
      (\(m : Integer) (ms : List Data) . bottom @Integer)
      (\(l : List (Tuple2 Data Data)) . bottom @Integer)
      (\(l : List Data) . bottom @Integer)
      (\(x : Integer) . x)
      (\(x : ByteString) . bottom @Integer)

unConstrData : Data -> Tuple2 Integer (List Data)
unConstrData d
  = Data_match d @(Tuple2 Integer (List Data))
      (\(m : Integer) (ms : List Data) . Tuple2 @Integer @(List Data) m ms)
      (\(l : List (Tuple2 Data Data)) . bottom @(Tuple2 Integer (List Data)))
      (\(l : List Data) . bottom @(Tuple2 Integer (List Data)))
      (\(x : Integer) . bottom @(Tuple2 Integer (List Data)))
      (\(x : ByteString) . bottom @(Tuple2 Integer (List Data)))

unBData : Data -> ByteString
unBData d
  = Data_match d @ByteString
      (\(m : Integer) (ms : List Data) . bottom @ByteString)
      (\(l : List (Tuple2 Data Data)) . bottom @ByteString)
      (\(l : List Data) . bottom @ByteString)
      (\(x : Integer) . bottom @ByteString)
      (\(x : ByteString) . x)

mkNilData : Data
mkNilData = List (Nil @Data)

|]
