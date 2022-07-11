{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares 'PlutusIR' as a supported language, instantiates all the
--  necessary bits for using the facilities from "Pirouette.Term.Syntax" and provides
--  a translation function 'trProgram' to translate a plutusIR program into a 'PrtTerm'
--  and a map of definitions.
module Language.Pirouette.PlutusIR.Syntax where

import Control.Monad.Combinators.Expr
import qualified Data.ByteString as BS
import Data.Data
import Data.Foldable
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Lift)
import Language.Pirouette.QuasiQuoter.Syntax hiding (TyAll, TyApp, TyFun)
import Pirouette.Term.Syntax
import qualified Pirouette.Term.Syntax.SystemF as SystF
import PlutusCore
  ( DefaultUni (..),
    pattern DefaultUniList,
    pattern DefaultUniPair,
    pattern DefaultUniString,
  )
import qualified PlutusCore as P
import qualified PlutusCore.Data as P
import Prettyprinter hiding (Pretty, pretty)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- * Declaring the builtins of PlutusIR.

-- | Defining the 'PlutusIR' as a language, which contains a set of builtin (types and terms)
-- and constants.
data PlutusIR
  deriving (Data, Typeable)

deriving instance Data P.DefaultFun

deriving instance Lift P.DefaultFun

deriving instance Lift P.Data

type PIRDefaultFun = P.DefaultFun

instance LanguageBuiltins PlutusIR where
  type BuiltinTypes PlutusIR = PIRBuiltinType
  type BuiltinTerms PlutusIR = PIRDefaultFun
  type Constants PlutusIR = PIRConstant

  builtinTypeDefinitions definedTypes =
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

tyConstant :: PIRBuiltinType -> Type PlutusIR
tyConstant = SystF.TyPure . SystF.Free . TyBuiltin

tyApp ::
  String ->
  [SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR))] ->
  SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR))
tyApp n = SystF.TyApp (SystF.Free $ TySig $ fromString n)

tyListOf ::
  SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR)) ->
  SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR))
tyListOf = tyApp "List" . (: [])

tyTuple2Of ::
  SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR)) ->
  SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR)) ->
  SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR))
tyTuple2Of x y = tyApp "Tuple2" [x, y]

tyData :: SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR))
tyData = tyApp "Data" []

tyUnit :: SystF.AnnType ann (SystF.VarMeta meta ann (TypeBase PlutusIR))
tyUnit = tyApp "Unit" []

cstToBuiltinType :: PIRConstant -> Type PlutusIR
cstToBuiltinType (PIRConstInteger _) = tyConstant PIRTypeInteger
cstToBuiltinType (PIRConstByteString _) = tyConstant PIRTypeByteString
cstToBuiltinType (PIRConstBool _) = tyConstant PIRTypeBool
cstToBuiltinType (PIRConstString _) = tyConstant PIRTypeString

-- | Builtin Plutus Types
data PIRBuiltinType
  = PIRTypeInteger
  | PIRTypeByteString
  | PIRTypeBool
  | PIRTypeString
  deriving (Eq, Ord, Show, Read, Data, Typeable, Lift)

defUniToType :: forall k (a :: k). DefaultUni (P.Esc a) -> Type PlutusIR
defUniToType DefaultUniInteger = tyConstant PIRTypeInteger
defUniToType DefaultUniByteString = tyConstant PIRTypeByteString
defUniToType DefaultUniBool = tyConstant PIRTypeBool
defUniToType DefaultUniString = tyConstant PIRTypeString
defUniToType DefaultUniUnit = tyUnit
defUniToType DefaultUniData = tyData
defUniToType (DefaultUniList a) = tyListOf (defUniToType a)
defUniToType (DefaultUniPair a b) = tyTuple2Of (defUniToType a) (defUniToType b)
-- The following are partially applied:
defUniToType DefaultUniProtoList = tyApp "List" []
defUniToType DefaultUniProtoPair = tyApp "Tuple2" []
defUniToType (DefaultUniApply DefaultUniProtoPair a) = tyApp "Tuple2" [defUniToType a]
defUniToType x = error $ "defUniToType impossible: " ++ show x

-- | Untyped Plutus Constants
data PIRConstant
  = PIRConstInteger Integer
  | PIRConstByteString BS.ByteString
  | PIRConstBool Bool
  | PIRConstString T.Text
  deriving (Eq, Ord, Show, Data, Typeable, Lift)

termConstant :: PIRConstant -> Term PlutusIR
termConstant = SystF.termPure . SystF.Free . Constant

ctorApp :: String -> [Arg PlutusIR] -> Term PlutusIR
ctorApp name = SystF.App (SystF.Free (TermSig $ fromString name))

defUniToConstant :: DefaultUni (P.Esc a) -> a -> Term PlutusIR
defUniToConstant DefaultUniInteger x = termConstant $ PIRConstInteger x
defUniToConstant DefaultUniByteString x = termConstant $ PIRConstByteString x
defUniToConstant DefaultUniBool x = termConstant $ PIRConstBool x
defUniToConstant DefaultUniString x = termConstant $ PIRConstString x
defUniToConstant DefaultUniUnit _ = ctorApp "Unit" []
defUniToConstant (DefaultUniList a) x =
  let tyA = SystF.TyArg $ defUniToType a
   in foldr (\uni t -> ctorApp "Cons" [tyA, SystF.TermArg (defUniToConstant a uni), SystF.TermArg t]) (ctorApp "Nil" [tyA]) x
defUniToConstant (DefaultUniPair a b) x =
  let tyA = SystF.TyArg $ defUniToType a
      tyB = SystF.TyArg $ defUniToType b
   in ctorApp "Tuple2" [tyA, tyB, SystF.TermArg (defUniToConstant a (fst x)), SystF.TermArg (defUniToConstant b (snd x))]
defUniToConstant DefaultUniData x = reifyData x
defUniToConstant uni _ = error $ "defUniToConstant impossible: " ++ show uni

reifyData :: P.Data -> Term PlutusIR
reifyData (P.Constr cN fields) =
  ctorApp
    "Constr"
    [ SystF.TermArg (termConstant $ PIRConstInteger cN),
      SystF.TermArg $ defUniToConstant (DefaultUniList DefaultUniData) fields
    ]
reifyData (P.Map kvs) =
  ctorApp
    "Map"
    [ SystF.TermArg $
        defUniToConstant (DefaultUniList (DefaultUniPair DefaultUniData DefaultUniData)) kvs
    ]
reifyData (P.List xs) =
  ctorApp
    "List"
    [SystF.TermArg $ defUniToConstant (DefaultUniList DefaultUniData) xs]
reifyData (P.I i) = ctorApp "I" [SystF.TermArg $ termConstant $ PIRConstInteger i]
reifyData (P.B bs) = ctorApp "B" [SystF.TermArg $ termConstant $ PIRConstByteString bs]

instance Pretty PIRBuiltinType where
  pretty PIRTypeInteger = "Integer"
  pretty PIRTypeByteString = "ByteString"
  pretty PIRTypeBool = "Bool"
  pretty PIRTypeString = "String"

instance Pretty (Maybe PIRBuiltinType) where
  pretty Nothing = "-"
  pretty (Just t) = pretty t

instance Pretty PIRConstant where
  pretty (PIRConstInteger x) = pretty x
  pretty (PIRConstByteString x) = "b" <> pretty x
  pretty (PIRConstBool x) = pretty x
  pretty (PIRConstString x) = dquotes (pretty x)

instance Pretty P.Data where
  pretty (P.Constr cN fields) = pretty cN <+> pretty fields
  pretty (P.Map kvs) = pretty kvs
  pretty (P.List xs) = pretty xs
  pretty (P.I i) = pretty i
  pretty (P.B bs) = pretty bs

-- * Parsing

instance LanguageParser PlutusIR where
  -- TODO: How about applied lists and pairs?
  parseBuiltinType =
    label "Builtin type" $
      asum $
        map
          try
          [ PIRTypeInteger <$ symbol "Integer",
            PIRTypeByteString <$ symbol "ByteString",
            PIRTypeBool <$ symbol "Bool",
            PIRTypeString <$ symbol "String"
          ]

  -- TODO: more constants
  parseConstant =
    label "Constant" $
      asum $
        map
          try
          [ PIRConstInteger <$> try integer,
            PIRConstBool <$> parseBoolean,
            PIRConstString . T.pack <$> stringLiteral
          ]
    where
      parseBoolean = (True <$ symbol "True") <|> (False <$ symbol "False")
      integer :: Parser Integer
      integer = L.lexeme spaceConsumer L.decimal
      -- copied from
      -- https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html#v:charLiteral
      stringLiteral :: Parser String
      stringLiteral = L.lexeme spaceConsumer (char '"' >> manyTill L.charLiteral (char '"'))

  -- Taken from: https://github.com/input-output-hk/plutus/blob/3c81f60746f50b596d3209c38e048ebb473c93b8/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs#L45
  parseBuiltinTerm =
    label "Builtin Term" $
      asum $
        map
          try
          [ -- Integers
            P.AddInteger <$ symbol "b/addInteger",
            P.SubtractInteger <$ symbol "b/subtractInteger",
            P.MultiplyInteger <$ symbol "b/multiplyInteger",
            P.DivideInteger <$ symbol "b/divideInteger",
            P.QuotientInteger <$ symbol "b/quotientInteger",
            P.RemainderInteger <$ symbol "b/remainderInteger",
            P.ModInteger <$ symbol "b/modInteger",
            P.EqualsInteger <$ symbol "b/equalsInteger",
            P.LessThanInteger <$ symbol "b/lessThanInteger",
            P.LessThanEqualsInteger <$ symbol "b/lessThanEqualsInteger",
            -- Bytestrings
            P.AppendByteString <$ symbol "b/appendByteString",
            P.ConsByteString <$ symbol "b/consByteString",
            P.SliceByteString <$ symbol "b/sliceByteString",
            P.LengthOfByteString <$ symbol "b/lengthOfByteString",
            P.IndexByteString <$ symbol "b/indexByteString",
            P.EqualsByteString <$ symbol "b/equalsByteString",
            P.LessThanByteString <$ symbol "b/lessThanByteString",
            P.LessThanEqualsByteString <$ symbol "b/lessThanEqualsByteString",
            -- Cryptography and hashes
            P.Sha2_256 <$ symbol "b/sha2",
            P.Sha3_256 <$ symbol "b/sha3",
            P.Blake2b_256 <$ symbol "b/blake2b",
            P.VerifyEd25519Signature <$ symbol "b/verifyEd25519Signature",
            P.VerifyEcdsaSecp256k1Signature <$ symbol "b/verifyEcdsaSecp256k1Signature",
            P.VerifySchnorrSecp256k1Signature <$ symbol "b/verifySchnorrSecp256k1Signature",
            -- Strings
            P.AppendString <$ symbol "b/appendString",
            P.EqualsString <$ symbol "b/equalsString",
            P.EncodeUtf8 <$ symbol "b/encodeUtf8",
            P.DecodeUtf8 <$ symbol "b/decodeUtf8",
            -- Bool
            P.IfThenElse <$ symbol "b/ifThenElse",
            -- Unit
            P.ChooseUnit <$ symbol "b/chooseUnit",
            -- Tracing
            P.Trace <$ symbol "b/trace",
            -- Pairs
            P.FstPair <$ symbol "b/fstPair",
            P.SndPair <$ symbol "b/sndPair",
            -- Lists
            P.ChooseList <$ symbol "b/chooseList",
            P.MkCons <$ symbol "b/mkCons",
            P.HeadList <$ symbol "b/headList",
            P.TailList <$ symbol "b/tailList",
            P.NullList <$ symbol "b/nullList",
            -- Data
            -- It is convenient to have a "choosing" function for a data type that has more than two
            -- constructors to get pattern matching over it and we may end up having multiple such data
            -- types, hence we include the name of the data type as a suffix.
            P.ChooseData <$ symbol "b/chooseData",
            P.ConstrData <$ symbol "b/constrData",
            P.MapData <$ symbol "b/mapData",
            P.ListData <$ symbol "b/listData",
            P.IData <$ symbol "b/iData",
            P.BData <$ symbol "b/bData",
            P.UnConstrData <$ symbol "b/unConstrData",
            P.UnMapData <$ symbol "b/unMapData",
            P.UnListData <$ symbol "b/unListData",
            P.UnIData <$ symbol "b/unIData",
            P.UnBData <$ symbol "b/unBData",
            P.EqualsData <$ symbol "b/equalsData",
            -- Misc constructors
            P.MkPairData <$ symbol "b/mkPairData",
            P.MkNilData <$ symbol "b/mkNilData",
            P.MkNilPairData <$ symbol "b/mkNilPairData"
          ]

  -- Some builtins will also be available through more familiar infix operators
  operators =
    [ [ InfixR (symbol "*" >> return (exprBinApp P.MultiplyInteger)),
        InfixR (symbol "/" >> return (exprBinApp P.DivideInteger)),
        InfixR (symbol "%" >> return (exprBinApp P.ModInteger))
      ],
      [ InfixR (symbol "+" >> return (exprBinApp P.AddInteger)),
        InfixR (symbol "-" >> return (exprBinApp P.SubtractInteger))
      ],
      [ InfixN (symbol "<" >> return (exprBinApp P.LessThanInteger)),
        InfixN (symbol "<=" >> return (exprBinApp P.LessThanEqualsInteger)),
        InfixN (symbol "==i" >> return (exprBinApp P.EqualsInteger)),
        InfixN (symbol "==d" >> return (exprBinApp P.EqualsData)),
        InfixN (symbol "==bs" >> return (exprBinApp P.EqualsByteString)),
        InfixN (symbol "==s" >> return (exprBinApp P.EqualsString))
      ]
    ]

  reservedNames = S.fromList $ words "True False unit"
  reservedTypeNames = S.fromList $ words "Bool String ByteString"

  ifThenElse resTy c t e = SystF.App (SystF.Free $ Builtin P.IfThenElse) $ SystF.TyArg resTy : map SystF.TermArg [c, t, e]
