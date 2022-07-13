{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Pirouette.PlutusIR.SMT where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Pirouette.PlutusIR.Syntax
import Pirouette.Monad
import Pirouette.SMT.Base
import Pirouette.SMT.Constraints
import Pirouette.Symbolic.Eval.Types
import Pirouette.Term.Syntax.Base
import Pirouette.Term.Syntax.SystemF as SystemF
import qualified PureSMT

-- See https://github.com/input-output-hk/plutus/blob/master/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs

instance LanguageSMT PlutusIR where
  translateBuiltinType = trPIRType

  -- translateBuiltinTerm = error "translateBuiltinTerm (t :: BuiltinTerms PlutusIR): not yet impl"
  translateBuiltinTerm = trPIRFun
  translateConstant = trPIRConstant

  isStuckBuiltin e
    | termIsConstant e = True
    | Just _ <- termIsMeta e = True
  isStuckBuiltin (App (Free (Builtin op)) args)
    | op `elem` plutusIRBasicOps || op `elem` plutusIRBasicRels,
      all isArg args -- this ensures that we only have TermArg
      =
      let args' = map (\(TermArg a) -> a) args
       in all isStuckBuiltin args' && not (all termIsConstant args')
  isStuckBuiltin _ = False

trPIRType :: PIRBuiltinType -> PureSMT.SExpr
trPIRType PIRTypeInteger = PureSMT.tInt
trPIRType PIRTypeString = PureSMT.tString
trPIRType PIRTypeByteString = PureSMT.tString

-- TODO Implement remaining constants
trPIRConstant :: PIRConstant -> PureSMT.SExpr
trPIRConstant (PIRConstInteger n) = PureSMT.int n
trPIRConstant (PIRConstByteString bs) = PureSMT.text $ T.decodeUtf8 $ B64.encode bs
trPIRConstant (PIRConstString txt) = PureSMT.text txt

-- | These operations can be fully evaluated
-- if the given arguments are constants.
-- For example, @3 + 4@ is *not* stuck.
plutusIRBasicOps :: [PIRDefaultFun]
plutusIRBasicOps =
  [ AddInteger,
    SubtractInteger,
    MultiplyInteger,
    DivideInteger,
    ModInteger,
    QuotientInteger,
    RemainderInteger,
    AppendString,
    AppendByteString,
    ConsByteString,
    IndexByteString,
    SliceByteString
  ]

-- | These relations can be fully evaluated
-- if the given arguments are constants.
-- For example, @3 < 4@ is *not* stuck.
plutusIRBasicRels :: [PIRDefaultFun]
plutusIRBasicRels =
  [ EqualsInteger,
    LessThanInteger,
    LessThanEqualsInteger,
    EqualsByteString,
    EqualsString,
    LessThanByteString,
    LessThanEqualsByteString
  ]

trPIRFun :: PIRDefaultFun -> [PureSMT.SExpr] -> Maybe PureSMT.SExpr

-- TODO Implement remaining builtins: those used by the "Auction" example
-- validator are marked with an [A] and as commented out lines in the
-- code afterwards

-- ** Hash and signatures **

--
--     Sha2_256
--     Sha3_256
--     Blake2b_256
--     VerifySignature
--

-- ** Strings and encoding **

--
--     EncodeUtf8
--     DecodeUtf8
--

-- Pattern matching in disguise,
-- so we return here Nothing and then "translate"
-- into an actual match in 'branchesBuiltinTerm'
trPIRFun ChooseUnit _ = Nothing
-- Relations returning booleans actually need to instruct the solver
-- to branch, their interpretation is handled in 'branchesBuiltinTerm'
trPIRFun EqualsInteger _ = Nothing
trPIRFun LessThanInteger _ = Nothing
trPIRFun LessThanEqualsInteger _ = Nothing
trPIRFun EqualsByteString _ = Nothing
trPIRFun EqualsString _ = Nothing
-- Unary
trPIRFun op [x] =
  case op of
    Trace -> Just $ PureSMT.List [x]
    _ ->
      error $
        "Translate builtin to SMT: "
          <> show op
          <> " is not an implemented unary operator/function"
-- Binary operations and relations
trPIRFun op [x, y] =
  case op of
    -- integer operations
    AddInteger -> Just $ PureSMT.add x y
    SubtractInteger -> Just $ PureSMT.sub x y
    MultiplyInteger -> Just $ PureSMT.mul x y
    -- divMod and quotRem work differently
    -- when not exact, but this is a good approximation
    DivideInteger -> Just $ PureSMT.div x y
    ModInteger -> Just $ PureSMT.mod x y
    QuotientInteger -> Just $ PureSMT.div x y
    RemainderInteger -> Just $ PureSMT.mod x y
    -- operations over other types
    AppendString -> Just $ PureSMT.fun "str.++" [x, y]
    _ ->
      error $
        "Translate builtin to SMT: "
          <> show op
          <> " is not an implemented binary operator/function"
-- Remainder
trPIRFun op _ =
  error $
    "Translate builtin to SMT: "
      <> show op
      <> " is not an implemented constant/operator/function"

trPIRBooleanRel :: PIRDefaultFun -> PureSMT.SExpr -> PureSMT.SExpr -> PureSMT.SExpr
trPIRBooleanRel EqualsInteger x y = PureSMT.eq x y
trPIRBooleanRel LessThanInteger x y = PureSMT.lt x y
trPIRBooleanRel LessThanEqualsInteger x y = PureSMT.leq x y
trPIRBooleanRel EqualsByteString x y = PureSMT.eq x y
trPIRBooleanRel EqualsString x y = PureSMT.eq x y
trPIRBooleanRel op _ _ = error $ "trPIRBooleanRel: " <> show op <> " is not yet implemented"

pattern K :: Constants lang -> AnnTerm ty ann (SystemF.VarMeta meta ann (TermBase lang))
pattern K n = App (Free (Constant n)) []

pattern S :: meta -> AnnTerm ty ann (SystemF.VarMeta meta ann (TermBase lang))
pattern S n = App (Meta n) []

wrapBoolean :: PureSMT.SExpr -> PureSMT.SExpr
wrapBoolean smtBuiltinBool = PureSMT.ite smtBuiltinBool (wrapOne "True") (wrapOne "False")
  where
    wrapOne name = PureSMT.as (PureSMT.symbol (toSmtName name)) (PureSMT.symbol "Bool")

reifyBoolean :: Bool -> AnnTerm ty ann (SystemF.VarMeta meta ann (TermBase lang))
reifyBoolean x = SystemF.App (SystemF.Free (TermSig ctor)) []
  where
    ctor = if x then "True" else "False"

instance LanguageSymEval PlutusIR where
  -- We don't have builtin booleans, its part of our prelude! Hence, whenever asserting
  -- whether a predicate "is true", we actually assert it is equal to the
  -- "True" constructor
  isTrue x = PureSMT.eq x (PureSMT.as (PureSMT.symbol (toSmtName "True")) (PureSMT.symbol (toSmtName "Bool")))

  -- basic operations over constants

  -- TODO: This is cool, we could implement some tracing in the SMT monad to help
  -- debug counter-examples! For now, just ignore traces.
  branchesBuiltinTerm Trace _ [TyArg _, TermArg _, TermArg res] =
    pure $ Just [Branch {additionalInfo = mempty, newTerm = res}]
  branchesBuiltinTerm op _ [TermArg (K (PIRConstInteger x)), TermArg (K (PIRConstInteger y))]
    | op `elem` plutusIRBasicOps || op `elem` plutusIRBasicRels =
      (\r -> pure $ Just [Branch {additionalInfo = mempty, newTerm = r}]) $
        case op of
          AddInteger -> K $ PIRConstInteger $ x + y
          SubtractInteger -> K $ PIRConstInteger $ x - y
          MultiplyInteger -> K $ PIRConstInteger $ x * y
          DivideInteger -> K $ PIRConstInteger $ x `div` y
          ModInteger -> K $ PIRConstInteger $ x `mod` y
          QuotientInteger -> K $ PIRConstInteger $ x `quot` y
          RemainderInteger -> K $ PIRConstInteger $ x `rem` y
          EqualsInteger -> reifyBoolean $ x == y
          LessThanInteger -> reifyBoolean $ x < y
          LessThanEqualsInteger -> reifyBoolean $ x <= y
          _ -> error "ill-typed application"
  branchesBuiltinTerm op tr [TermArg x, TermArg y]
    | op `elem` plutusIRBasicRels = do
      tx <- tr x
      ty <- tr y
      case (,) <$> tx <*> ty of
        Nothing -> pure Nothing
        Just (rx, ry) -> do
          let rel = trPIRBooleanRel op rx ry
          pure $
            Just
              [ Branch {additionalInfo = And [Native rel], newTerm = reifyBoolean True},
                Branch {additionalInfo = And [Native $ PureSMT.not rel], newTerm = reifyBoolean False}
              ]
  branchesBuiltinTerm op _ [TermArg (K (PIRConstByteString x)), TermArg (K (PIRConstByteString y))]
    | op `elem` plutusIRBasicOps || op `elem` plutusIRBasicRels =
      (\r -> pure $ Just [Branch {additionalInfo = mempty, newTerm = r}]) $
        case op of
          AppendByteString -> K $ PIRConstByteString (x <> y)
          EqualsByteString -> reifyBoolean $ x == y
          LessThanByteString -> reifyBoolean $ x < y
          LessThanEqualsByteString -> reifyBoolean $ x <= y
          _ -> error "ill-typed application"
  branchesBuiltinTerm LengthOfByteString _ [TermArg (K (PIRConstByteString b))] =
    let r = K $ PIRConstInteger (fromIntegral $ BS.length b)
     in pure $ Just [Branch {additionalInfo = mempty, newTerm = r}]
  branchesBuiltinTerm
    ConsByteString
    _
    [TermArg (K (PIRConstInteger i)), TermArg (K (PIRConstByteString b))] =
      let r = K $ PIRConstByteString (BS.cons (fromInteger i) b)
       in pure $ Just [Branch {additionalInfo = mempty, newTerm = r}]
  branchesBuiltinTerm
    IndexByteString
    _
    [TermArg (K (PIRConstByteString b)), TermArg (K (PIRConstInteger i))] =
      let r =
            if i < 0
              then errorTerm
              else K $ PIRConstInteger $ fromIntegral (BS.index b (fromInteger i))
       in pure $ Just [Branch {additionalInfo = mempty, newTerm = r}]
  branchesBuiltinTerm
    IndexByteString
    _
    [TermArg (K (PIRConstInteger start)), TermArg (K (PIRConstInteger n)), TermArg (K (PIRConstByteString xs))] =
      let r = K $ PIRConstByteString $ BS.take (fromInteger n) (BS.drop (fromInteger start) xs)
       in pure $ Just [Branch {additionalInfo = mempty, newTerm = r}]
  branchesBuiltinTerm op _ [TermArg (K (PIRConstString x)), TermArg (K (PIRConstString y))]
    | op `elem` plutusIRBasicOps || op `elem` plutusIRBasicRels =
      (\r -> pure $ Just [Branch {additionalInfo = mempty, newTerm = r}]) $
        case op of
          AppendString -> K $ PIRConstString $ x <> y
          EqualsString -> reifyBoolean $ x == y
          _ -> error "ill-typed application"
  -- if-then-else goes to the helpers
  -- branchesBuiltinTerm IfThenElse _ (TyArg _ : TermArg c : TermArg t : TermArg e : excess) =
  --   let isEq EqualsInteger = True
  --       isEq EqualsString = True
  --       isEq EqualsByteString = True
  --       isEq _ = False
  --       isTrue (K (PIRConstBool True)) = True
  --       isTrue _ = False
  --       isFalse (K (PIRConstBool False)) = True
  --       isFalse _ = False
  --    in ifThenElseBranching
  --         isTrue
  --         (K (PIRConstBool True))
  --         isFalse
  --         (K (PIRConstBool False))
  --         isEq
  --         c
  --         t
  --         e
  --         excess
  -- pattern matching and built-in matchers

  -- they take the arguments in a different order
  branchesBuiltinTerm ChooseUnit _ (tyR : unit : rest) =
    continueWith "Unit_match" (unit : tyR : rest)
  -- built-in matchers
  branchesBuiltinTerm _rest _translator _args =
    pure Nothing

-- | Indicates that the next step in the evaluation of that
-- built-in is the given function with the given arguments.
-- There's quite some boilerplate around it, hence this utility.
continueWith ::
  (ToSMT meta, Applicative m) =>
  Text ->
  [ArgMeta lang meta] ->
  m (Maybe [Branch lang meta])
continueWith destr args = do
  let destr' = Name destr Nothing
      tm = App (Free $ TermSig destr') args
  pure $ Just [Branch {additionalInfo = mempty, newTerm = tm}]

errorTerm :: AnnTerm ty ann (SystemF.VarMeta meta ann (TermBase lang))
errorTerm = App (Free Bottom) []

builtin :: PIRBuiltinType -> AnnType ann (SystemF.VarMeta meta ann (TypeBase PlutusIR))
builtin nm = TyApp (Free $ TyBuiltin nm) []
