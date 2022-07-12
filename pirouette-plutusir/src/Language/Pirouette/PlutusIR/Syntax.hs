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

-- | Defining the 'PlutusIR' as a language, which contains a set of builtin (types and terms)
-- and constants.
data PlutusIR
  deriving (Data, Typeable)

-- * Declaring the builtins of PlutusIR.

instance LanguageBuiltins PlutusIR where
  type BuiltinTypes PlutusIR = PIRBuiltinType
  type BuiltinTerms PlutusIR = PIRDefaultFun
  type Constants PlutusIR = PIRConstant

deriving instance Data P.DefaultFun

deriving instance Lift P.DefaultFun

deriving instance Lift P.Data

-- * Builtin Types

-- $bintypes
-- We will only consider four types as builtin types in pirouette. The reason being that
-- it is much easier to rely as much as possible on pirouette's data definitinos, since
-- these integrate seamlessly with the entirety of pirouette, for things like @List@, @Data@, etc

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

-- | We will keep builtin functions related to any type in 'PIRBuiltinType',
--  any other functions will be compiled to something else.
data PIRDefaultFun
  = AddInteger
  | SubtractInteger
  | MultiplyInteger
  | DivideInteger
  | QuotientInteger
  | RemainderInteger
  | ModInteger
  | EqualsInteger
  | LessThanInteger
  | LessThanEqualsInteger
  | -- Bytestrings
    AppendByteString
  | ConsByteString
  | SliceByteString
  | LengthOfByteString
  | IndexByteString
  | EqualsByteString
  | LessThanByteString
  | LessThanEqualsByteString
  | -- Cryptography and hashes
    Sha2_256
  | Sha3_256
  | Blake2b_256
  | VerifySignature
  | -- VerifyEd25519Signature
    -- VerifyEcdsaSecp256k1Signature
    -- VerifySchnorrSecp256k1Signature
    -- Strings
    AppendString
  | EqualsString
  | EncodeUtf8
  | DecodeUtf8
  | -- Bool
    IfThenElse
  | -- Unit
    ChooseUnit
  | -- Tracing
    Trace
  | -- Pairs
    FstPair
  | SndPair
  | -- Lists
    ChooseList
  | MkCons
  | HeadList
  | TailList
  | NullList
  | -- Data
    -- It is convenient to have a "choosing" function for a data type that has more than two
    -- constructors to get pattern matching over it and we may end up having multiple such data
    -- types, hence we include the name of the data type as a suffix.
    ChooseData
  | ConstrData
  | MapData
  | ListData
  | IData
  | BData
  | UnConstrData
  | UnMapData
  | UnListData
  | UnIData
  | UnBData
  | EqualsData
  | -- Misc constructors
    MkPairData
  | MkNilData
  | MkNilPairData
  deriving (Eq, Show, Ord, Data, Typeable, Lift)

builtinToTerm :: P.DefaultFun -> [Arg PlutusIR] -> Term PlutusIR
builtinToTerm hd = SystF.App (SystF.Free (Builtin $ fromSupportedPlutusDefaultFun hd))

fromSupportedPlutusDefaultFun :: P.DefaultFun -> PIRDefaultFun
fromSupportedPlutusDefaultFun P.AddInteger = AddInteger
fromSupportedPlutusDefaultFun P.SubtractInteger = SubtractInteger
fromSupportedPlutusDefaultFun P.MultiplyInteger = MultiplyInteger
fromSupportedPlutusDefaultFun P.DivideInteger = DivideInteger
fromSupportedPlutusDefaultFun P.QuotientInteger = QuotientInteger
fromSupportedPlutusDefaultFun P.RemainderInteger = RemainderInteger
fromSupportedPlutusDefaultFun P.ModInteger = ModInteger
fromSupportedPlutusDefaultFun P.EqualsInteger = EqualsInteger
fromSupportedPlutusDefaultFun P.LessThanInteger = LessThanInteger
fromSupportedPlutusDefaultFun P.LessThanEqualsInteger = LessThanEqualsInteger
fromSupportedPlutusDefaultFun P.AppendByteString = AppendByteString
fromSupportedPlutusDefaultFun P.ConsByteString = ConsByteString
fromSupportedPlutusDefaultFun P.SliceByteString = SliceByteString
fromSupportedPlutusDefaultFun P.LengthOfByteString = LengthOfByteString
fromSupportedPlutusDefaultFun P.IndexByteString = IndexByteString
fromSupportedPlutusDefaultFun P.EqualsByteString = EqualsByteString
fromSupportedPlutusDefaultFun P.LessThanByteString = LessThanByteString
fromSupportedPlutusDefaultFun P.LessThanEqualsByteString = LessThanEqualsByteString
fromSupportedPlutusDefaultFun P.Sha2_256 = Sha2_256
fromSupportedPlutusDefaultFun P.Sha3_256 = Sha3_256
fromSupportedPlutusDefaultFun P.Blake2b_256 = Blake2b_256
fromSupportedPlutusDefaultFun P.VerifySignature = VerifySignature
fromSupportedPlutusDefaultFun P.AppendString = AppendString
fromSupportedPlutusDefaultFun P.EqualsString = EqualsString
fromSupportedPlutusDefaultFun P.EncodeUtf8 = EncodeUtf8
fromSupportedPlutusDefaultFun P.DecodeUtf8 = DecodeUtf8
fromSupportedPlutusDefaultFun P.IfThenElse = IfThenElse
fromSupportedPlutusDefaultFun P.ChooseUnit = ChooseUnit
fromSupportedPlutusDefaultFun P.Trace = Trace
fromSupportedPlutusDefaultFun P.FstPair = FstPair
fromSupportedPlutusDefaultFun P.SndPair = SndPair
fromSupportedPlutusDefaultFun P.ChooseList = ChooseList
fromSupportedPlutusDefaultFun P.MkCons = MkCons
fromSupportedPlutusDefaultFun P.HeadList = HeadList
fromSupportedPlutusDefaultFun P.TailList = TailList
fromSupportedPlutusDefaultFun P.NullList = NullList
fromSupportedPlutusDefaultFun P.ChooseData = ChooseData
fromSupportedPlutusDefaultFun P.ConstrData = ConstrData
fromSupportedPlutusDefaultFun P.MapData = MapData
fromSupportedPlutusDefaultFun P.ListData = ListData
fromSupportedPlutusDefaultFun P.IData = IData
fromSupportedPlutusDefaultFun P.BData = BData
fromSupportedPlutusDefaultFun P.UnConstrData = UnConstrData
fromSupportedPlutusDefaultFun P.UnMapData = UnMapData
fromSupportedPlutusDefaultFun P.UnListData = UnListData
fromSupportedPlutusDefaultFun P.UnIData = UnIData
fromSupportedPlutusDefaultFun P.UnBData = UnBData
fromSupportedPlutusDefaultFun P.EqualsData = EqualsData
fromSupportedPlutusDefaultFun P.MkPairData = MkPairData
fromSupportedPlutusDefaultFun P.MkNilData = MkNilData
fromSupportedPlutusDefaultFun P.MkNilPairData = MkNilPairData

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

instance Pretty PIRDefaultFun where
  pretty s = "b/" <> pretty (show s)

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
            AddInteger <$ symbol "b/addInteger",
            SubtractInteger <$ symbol "b/subtractInteger",
            MultiplyInteger <$ symbol "b/multiplyInteger",
            DivideInteger <$ symbol "b/divideInteger",
            QuotientInteger <$ symbol "b/quotientInteger",
            RemainderInteger <$ symbol "b/remainderInteger",
            ModInteger <$ symbol "b/modInteger",
            EqualsInteger <$ symbol "b/equalsInteger",
            LessThanInteger <$ symbol "b/lessThanInteger",
            LessThanEqualsInteger <$ symbol "b/lessThanEqualsInteger",
            -- Bytestrings
            AppendByteString <$ symbol "b/appendByteString",
            ConsByteString <$ symbol "b/consByteString",
            SliceByteString <$ symbol "b/sliceByteString",
            LengthOfByteString <$ symbol "b/lengthOfByteString",
            IndexByteString <$ symbol "b/indexByteString",
            EqualsByteString <$ symbol "b/equalsByteString",
            LessThanByteString <$ symbol "b/lessThanByteString",
            LessThanEqualsByteString <$ symbol "b/lessThanEqualsByteString",
            -- Cryptography and hashes
            Sha2_256 <$ symbol "b/sha2",
            Sha3_256 <$ symbol "b/sha3",
            Blake2b_256 <$ symbol "b/blake2b",
            VerifySignature <$ symbol "b/verifySignature",
            -- P.VerifyEd25519Signature <$ symbol "b/verifyEd25519Signature",
            -- P.VerifyEcdsaSecp256k1Signature <$ symbol "b/verifyEcdsaSecp256k1Signature",
            -- P.VerifySchnorrSecp256k1Signature <$ symbol "b/verifySchnorrSecp256k1Signature",
            -- Strings
            AppendString <$ symbol "b/appendString",
            EqualsString <$ symbol "b/equalsString",
            EncodeUtf8 <$ symbol "b/encodeUtf8",
            DecodeUtf8 <$ symbol "b/decodeUtf8",
            -- Bool
            IfThenElse <$ symbol "b/ifThenElse",
            -- Unit
            ChooseUnit <$ symbol "b/chooseUnit",
            -- Tracing
            Trace <$ symbol "b/trace",
            -- Pairs
            FstPair <$ symbol "b/fstPair",
            SndPair <$ symbol "b/sndPair",
            -- Lists
            ChooseList <$ symbol "b/chooseList",
            MkCons <$ symbol "b/mkCons",
            HeadList <$ symbol "b/headList",
            TailList <$ symbol "b/tailList",
            NullList <$ symbol "b/nullList",
            -- Data
            -- It is convenient to have a "choosing" function for a data type that has more than two
            -- constructors to get pattern matching over it and we may end up having multiple such data
            -- types, hence we include the name of the data type as a suffix.
            ChooseData <$ symbol "b/chooseData",
            ConstrData <$ symbol "b/constrData",
            MapData <$ symbol "b/mapData",
            ListData <$ symbol "b/listData",
            IData <$ symbol "b/iData",
            BData <$ symbol "b/bData",
            UnConstrData <$ symbol "b/unConstrData",
            UnMapData <$ symbol "b/unMapData",
            UnListData <$ symbol "b/unListData",
            UnIData <$ symbol "b/unIData",
            UnBData <$ symbol "b/unBData",
            EqualsData <$ symbol "b/equalsData",
            -- Misc constructors
            MkPairData <$ symbol "b/mkPairData",
            MkNilData <$ symbol "b/mkNilData",
            MkNilPairData <$ symbol "b/mkNilPairData"
          ]

  -- Some builtins will also be available through more familiar infix operators
  operators =
    [ [ InfixR (symbol "*" >> return (exprBinApp MultiplyInteger)),
        InfixR (symbol "/" >> return (exprBinApp DivideInteger)),
        InfixR (symbol "%" >> return (exprBinApp ModInteger))
      ],
      [ InfixR (symbol "+" >> return (exprBinApp AddInteger)),
        InfixR (symbol "-" >> return (exprBinApp SubtractInteger))
      ],
      [ InfixN (symbol "<" >> return (exprBinApp LessThanInteger)),
        InfixN (symbol "<=" >> return (exprBinApp LessThanEqualsInteger)),
        InfixN (symbol "==i" >> return (exprBinApp EqualsInteger)),
        InfixN (symbol "==d" >> return (exprBinApp EqualsData)),
        InfixN (symbol "==bs" >> return (exprBinApp EqualsByteString)),
        InfixN (symbol "==s" >> return (exprBinApp EqualsString))
      ]
    ]

  reservedNames = S.fromList $ words "True False unit"
  reservedTypeNames = S.fromList $ words "Bool String ByteString"

  ifThenElse resTy c t e = SystF.App (SystF.Free $ Builtin IfThenElse) $ SystF.TyArg resTy : map SystF.TermArg [c, t, e]
