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
import Data.Functor (($>))
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

-- | Builtin Plutus Types that are considered builtin for Pirouette.
-- We will only consider a few types as builtin types in pirouette. The reason being that
-- it is much easier to just /define/ the inductive types as pirouette first class values.
-- These integrate seamlessly with the entirety of pirouette, classic examples are: @List@, @Data@, etc
data PIRBuiltinType
  = PIRTypeInteger
  | PIRTypeByteString
  | PIRTypeString
  deriving (Eq, Ord, Show, Read, Data, Typeable, Lift)

defUniToType :: forall k (a :: k). DefaultUni (P.Esc a) -> Type PlutusIR
defUniToType DefaultUniInteger = tyConstant PIRTypeInteger
defUniToType DefaultUniByteString = tyConstant PIRTypeByteString
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

-- | We will keep some of the builtin functions related to any type in 'PIRBuiltinType',
--  any other functions will be compiled to something else by the 'builtinToTerm' function.
--  We want to keep this datatype as small as possible: the more builtins we have to handle
--  the more custom definitions need to be made in the different classes needed to
--  interface with Pirouette: LanguageSMT, LanguageBuiltinTypes, LanguageSymEval, ...
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
  -- | VerifyEd25519Signature
    -- VerifyEcdsaSecp256k1Signature
    -- VerifySchnorrSecp256k1Signature
    -- Strings
  | AppendString
  | EqualsString
  | EncodeUtf8
  | DecodeUtf8
  | -- Tracing
    Trace
  deriving (Eq, Show, Ord, Data, Typeable, Lift)

appSig :: Name -> [Arg PlutusIR] -> Term PlutusIR
appSig hd = SystF.App (SystF.Free $ TermSig hd)

appBuiltin :: PIRDefaultFun -> [Arg PlutusIR] -> Term PlutusIR
appBuiltin hd = SystF.App (SystF.Free $ Builtin hd)

-- | Translates a PlutusIR stock builtin ('P.DefaultFun') into a term
--  of Pirouette's view over PlutusIR. The insight here is that we are /not/ translating
--  all PlutusIR builtins to pirouette builtins: some of those have perfectly simple and
--  standard implementations. In particular, all functions matching inductive
--  datatypes are much easier to handle as /defined/ functions, not builtins.
--  The reason is that the symbolic evaluation already knows how to handle destructors,
--  but would need to be explained how to handle builtin functions that match on things.
builtinToTerm :: P.DefaultFun -> [Arg PlutusIR] -> Term PlutusIR
-- Bool
builtinToTerm P.IfThenElse = appSig "ifThenElse"
-- Lists
builtinToTerm P.ChooseList = appSig "chooseList"
builtinToTerm P.TailList = appSig "tailList"
builtinToTerm P.HeadList = appSig "headList"
-- Tuples
builtinToTerm P.FstPair = appSig "fstPair"
builtinToTerm P.SndPair = appSig "sndPair"
-- Data
builtinToTerm P.ChooseData = appSig "chooseData"
builtinToTerm P.UnIData = appSig "unIData"
builtinToTerm P.UnBData = appSig "unBData"
builtinToTerm P.UnConstrData = appSig "unConstrData"
builtinToTerm P.MkNilData = appSig "mkNilData"
-- Unit
builtinToTerm P.ChooseUnit = appSig "chooseUnit"
-- Default to translating to supported builtin
builtinToTerm hd = appBuiltin $ fromSupportedPlutusDefaultFun hd
  where
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
    fromSupportedPlutusDefaultFun P.VerifyEd25519Signature = VerifySignature
    fromSupportedPlutusDefaultFun P.AppendString = AppendString
    fromSupportedPlutusDefaultFun P.EqualsString = EqualsString
    fromSupportedPlutusDefaultFun P.EncodeUtf8 = EncodeUtf8
    fromSupportedPlutusDefaultFun P.DecodeUtf8 = DecodeUtf8
    fromSupportedPlutusDefaultFun P.Trace = Trace
    fromSupportedPlutusDefaultFun x =
      error $
        unlines
          [ "Unsupported builtin: " ++ show x,
            "You should consider either adding it to the Language.Pirouette.PlutusIR.Prelude",
            "or add it to the definition of Language.Pirouette.PlutusIR.Syntax.PIRDefaultFun"
          ]

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
cstToBuiltinType (PIRConstString _) = tyConstant PIRTypeString

-- | Constants are the inhabitants of the 'PIRBuiltinType's.
data PIRConstant
  = PIRConstInteger Integer
  | PIRConstByteString BS.ByteString
  | PIRConstString T.Text
  deriving (Eq, Ord, Show, Data, Typeable, Lift)

termConstant :: PIRConstant -> Term PlutusIR
termConstant = SystF.termPure . SystF.Free . Constant

ctorApp :: String -> [Arg PlutusIR] -> Term PlutusIR
ctorApp name = SystF.App (SystF.Free (TermSig $ fromString name))

-- | Translates a term of a default type to a pirouette term. Note that because
-- some builtin types were translated to types defined in our "Language.Pirouette.PlutusIR.Prelude",
-- we need to reify their values with the correct constructors. For instance, a value of type
-- @DefaultUniList DefaultUniBool@, say, @[True, False]@, have to be translated to a value:
--
-- > Cons @Bool True (Cons @Bool False (Nil @Bool))
--
-- where @Cons@, @Nil@, @True@ and @False@ will be defined in the prelude.
defUniToConstant :: DefaultUni (P.Esc a) -> a -> Term PlutusIR
defUniToConstant DefaultUniInteger x = termConstant $ PIRConstInteger x
defUniToConstant DefaultUniByteString x = termConstant $ PIRConstByteString x
defUniToConstant DefaultUniBool x = ctorApp (if x then "True" else "False") []
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
  pretty PIRTypeString = "String"

instance Pretty (Maybe PIRBuiltinType) where
  pretty Nothing = "-"
  pretty (Just t) = pretty t

instance Pretty PIRConstant where
  pretty (PIRConstInteger x) = pretty x
  pretty (PIRConstByteString x) = "b" <> pretty x
  pretty (PIRConstString x) = dquotes (pretty x)

instance Pretty P.Data where
  pretty (P.Constr cN fields) = pretty cN <+> pretty fields
  pretty (P.Map kvs) = pretty kvs
  pretty (P.List xs) = pretty xs
  pretty (P.I i) = pretty i
  pretty (P.B bs) = pretty bs

instance Pretty PIRDefaultFun where
  pretty s = "b/" <> pretty s

-- * Parsing

instance LanguageParser PlutusIR where
  parseBuiltinType =
    label "Builtin type" $
      asum $
        map
          try
          [ PIRTypeInteger <$ symbol "Integer",
            PIRTypeByteString <$ symbol "ByteString",
            PIRTypeString <$ symbol "String"
          ]

  -- TODO: more constants
  parseConstant =
    label "Constant" $
      asum $
        map
          try
          [ PIRConstInteger <$> try integer,
            PIRConstString . T.pack <$> stringLiteral
          ]
    where
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
            -- VerifySignature <$ symbol "b/verifySignature", -- doesn't exist anymore ?
            VerifySignature <$ symbol "b/verifyEd25519Signature",
            -- P.VerifyEcdsaSecp256k1Signature <$ symbol "b/verifyEcdsaSecp256k1Signature",
            -- P.VerifySchnorrSecp256k1Signature <$ symbol "b/verifySchnorrSecp256k1Signature",
            -- Strings
            AppendString <$ symbol "b/appendString",
            EqualsString <$ symbol "b/equalsString",
            EncodeUtf8 <$ symbol "b/encodeUtf8",
            DecodeUtf8 <$ symbol "b/decodeUtf8",
            -- Tracing
            Trace <$ symbol "b/trace"
          ]

  -- Some builtins will also be available through more familiar infix operators
  operators =
    [ [ InfixR (symbol "*" $> exprBinApp MultiplyInteger),
        InfixR (symbol "/" $> exprBinApp DivideInteger),
        InfixR (symbol "%" $> exprBinApp ModInteger)
      ],
      [ InfixR (symbol "+" $> exprBinApp AddInteger),
        InfixR (symbol "-" $> exprBinApp SubtractInteger)
      ],
      [ InfixN (symbol "<" $> exprBinApp LessThanInteger),
        InfixN (symbol "<=" $> exprBinApp LessThanEqualsInteger),
        InfixN (symbol "==i" $> exprBinApp EqualsInteger),
        InfixN (symbol "==bs" $> exprBinApp EqualsByteString),
        InfixN (symbol "==s" $> exprBinApp EqualsString)
      ]
    ]

  reservedTermNames = S.fromList $ words ""
  reservedTypeNames = S.fromList $ words "Integer String ByteString"

  ifThenElse resTy c t e = appSig "ifThenElse" $ SystF.TyArg resTy : map SystF.TermArg [c, t, e]
