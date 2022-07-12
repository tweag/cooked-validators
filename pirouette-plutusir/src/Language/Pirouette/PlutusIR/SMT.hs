{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Pirouette.PlutusIR.SMT where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Language.Pirouette.PlutusIR.Syntax
import Pirouette.Monad
import Pirouette.SMT.Base
import Pirouette.SMT.Constraints
import Pirouette.Symbolic.Eval.BranchingHelpers
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
trPIRType PIRTypeBool = PureSMT.tBool
trPIRType PIRTypeString = PureSMT.tString
trPIRType PIRTypeByteString = PureSMT.tString

-- TODO Implement remaining constants
trPIRConstant :: PIRConstant -> PureSMT.SExpr
trPIRConstant (PIRConstInteger n) = PureSMT.int n
trPIRConstant (PIRConstByteString _bs) = error "Not implemented: PIRConstByteString to SMT"
trPIRConstant (PIRConstBool b) = PureSMT.bool b
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
    EqualsData,
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
trPIRFun ChooseData _ = Nothing
trPIRFun FstPair _ = Nothing
trPIRFun SndPair _ = Nothing
trPIRFun UnConstrData _ = Nothing
trPIRFun UnMapData _ = Nothing
trPIRFun UnListData _ = Nothing
trPIRFun UnIData _ = Nothing
trPIRFun UnBData _ = Nothing
-- If-then-else is complicated
trPIRFun IfThenElse _ = Nothing
-- Unary
trPIRFun op [x] =
  case op of
    Trace -> Just $ PureSMT.List [x]
    -- some simple operations
    -- constructors
    -- those are defined as unary functions for historical reasons
    MkNilData -> Just $ PureSMT.fun "Nil" []
    MkNilPairData -> Just $ PureSMT.fun "Nil" []
    -- and these are for Data
    MapData -> Just $ PureSMT.fun "Map" [x]
    ListData -> Just $ PureSMT.fun "List" [x]
    IData -> Just $ PureSMT.fun "I" [x]
    BData -> Just $ PureSMT.fun "B" [x]
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
    -- relations
    EqualsInteger -> Just $ PureSMT.eq x y
    LessThanInteger -> Just $ PureSMT.lt x y
    LessThanEqualsInteger -> Just $ PureSMT.leq x y
    EqualsByteString -> Just $ PureSMT.eq x y
    EqualsString -> Just $ PureSMT.eq x y
    EqualsData -> Just $ PureSMT.eq x y
    -- constructors
    MkPairData -> Just $ PureSMT.fun "Tuple2" [x, y]
    ConstrData -> Just $ PureSMT.fun "Constr" [x, y]
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

pattern K :: Constants lang -> AnnTerm ty ann (SystemF.VarMeta meta ann (TermBase lang))
pattern K n = App (Free (Constant n)) []

instance LanguageSymEval PlutusIR where
  -- basic operations over constants

  branchesBuiltinTerm op _ [TermArg (K (PIRConstInteger x)), TermArg (K (PIRConstInteger y))]
    | op `elem` plutusIRBasicOps || op `elem` plutusIRBasicRels =
      (\r -> pure $ Just [Branch {additionalInfo = mempty, newTerm = K r}]) $
        case op of
          AddInteger -> PIRConstInteger $ x + y
          SubtractInteger -> PIRConstInteger $ x - y
          MultiplyInteger -> PIRConstInteger $ x * y
          DivideInteger -> PIRConstInteger $ x `div` y
          ModInteger -> PIRConstInteger $ x `mod` y
          QuotientInteger -> PIRConstInteger $ x `quot` y
          RemainderInteger -> PIRConstInteger $ x `rem` y
          EqualsInteger -> PIRConstBool $ x == y
          LessThanInteger -> PIRConstBool $ x < y
          LessThanEqualsInteger -> PIRConstBool $ x <= y
          _ -> error "ill-typed application"
  branchesBuiltinTerm op _ [TermArg (K (PIRConstByteString x)), TermArg (K (PIRConstByteString y))]
    | op `elem` plutusIRBasicOps || op `elem` plutusIRBasicRels =
      (\r -> pure $ Just [Branch {additionalInfo = mempty, newTerm = K r}]) $
        case op of
          AppendByteString -> PIRConstByteString (x <> y)
          EqualsByteString -> PIRConstBool $ x == y
          LessThanByteString -> PIRConstBool $ x < y
          LessThanEqualsByteString -> PIRConstBool $ x <= y
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
      (\r -> pure $ Just [Branch {additionalInfo = mempty, newTerm = K r}]) $
        case op of
          AppendString -> PIRConstString $ x <> y
          EqualsString -> PIRConstBool $ x == y
          _ -> error "ill-typed application"
  -- if-then-else goes to the helpers
  branchesBuiltinTerm IfThenElse _ (TyArg _ : TermArg c : TermArg t : TermArg e : excess) =
    let isEq EqualsInteger = True
        isEq EqualsString = True
        isEq EqualsByteString = True
        isEq EqualsData = True
        isEq _ = False
        isTrue (K (PIRConstBool True)) = True
        isTrue _ = False
        isFalse (K (PIRConstBool False)) = True
        isFalse _ = False
     in ifThenElseBranching
          isTrue
          (K (PIRConstBool True))
          isFalse
          (K (PIRConstBool False))
          isEq
          c
          t
          e
          excess
  -- applying constructors functions during evaluation
  -- is quite useful because we tend can interact with
  -- the pattern matching much better b/c we know
  -- the constructor that has been applied

  branchesBuiltinTerm MkNilData _ _args =
    continueWith "Nil" []
  branchesBuiltinTerm MkNilPairData _ _args =
    continueWith "Nil" []
  branchesBuiltinTerm ConstrData _ args =
    continueWith "Data_Constr" args
  branchesBuiltinTerm MapData _ args =
    continueWith "Data_Map" args
  branchesBuiltinTerm ListData _ args =
    continueWith "Data_List" args
  branchesBuiltinTerm IData _ args =
    continueWith "Data_I" args
  branchesBuiltinTerm BData _ args =
    continueWith "Data_B" args
  -- pattern matching and built-in matchers

  -- they take the arguments in a different order
  branchesBuiltinTerm ChooseUnit _ (tyR : unit : rest) =
    continueWith "Unit_match" (unit : tyR : rest)
  branchesBuiltinTerm
    ChooseData
    _
    (tyR : dat : TermArg caseC : TermArg caseM : TermArg caseL : TermArg caseI : TermArg caseB : excess) =
      continueWithDataMatch dat tyR caseC caseM caseL caseI caseB excess
  -- built-in matchers

  branchesBuiltinTerm FstPair _ [tyA@(TyArg a), tyB@(TyArg b), tuple] =
    continueWith
      "Tuple2_match"
      [ tyA,
        tyB,
        tuple,
        tyA,
        TermArg $ Lam (Ann "x") a $ Lam (Ann "y") b $ App (Bound (Ann "x") 1) []
      ]
  branchesBuiltinTerm SndPair _ [tyA@(TyArg a), tyB@(TyArg b), tuple] =
    continueWith
      "Tuple2_match"
      [ tyA,
        tyB,
        tuple,
        tyB,
        TermArg $ Lam (Ann "x") a $ Lam (Ann "y") b $ App (Bound (Ann "y") 0) []
      ]
  branchesBuiltinTerm UnConstrData _ [d] =
    continueWithDataMatch
      d
      (TyArg (tyTuple2Of (builtin PIRTypeInteger) tyData))
      (App (Free $ TermSig "Tuple2") [TermArg $ App (Bound (Ann "i") 1) [], TermArg $ App (Bound (Ann "ds") 0) []])
      errorTerm
      errorTerm
      errorTerm
      errorTerm
      []
  branchesBuiltinTerm UnMapData _ [d] =
    continueWithDataMatch
      d
      (TyArg (tyListOf (tyTuple2Of tyData tyData)))
      errorTerm
      (App (Bound (Ann "es") 0) [])
      errorTerm
      errorTerm
      errorTerm
      []
  branchesBuiltinTerm UnListData _ [d] =
    continueWithDataMatch
      d
      (TyArg (tyListOf tyData))
      errorTerm
      errorTerm
      (App (Bound (Ann "ds") 0) [])
      errorTerm
      errorTerm
      []
  branchesBuiltinTerm UnIData _ [d] =
    continueWithDataMatch
      d
      (TyArg (builtin PIRTypeInteger))
      errorTerm
      errorTerm
      errorTerm
      (App (Bound (Ann "i") 0) [])
      errorTerm
      []
  branchesBuiltinTerm UnBData _ [d] =
    continueWithDataMatch
      d
      (TyArg (builtin PIRTypeByteString))
      errorTerm
      errorTerm
      errorTerm
      errorTerm
      (App (Bound (Ann "b") 0) [])
      []
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

-- | Indicates that the next step is an application of
-- the List destructor. Note that this function does *not*
-- include any check for well-scopedness, so be careful!
continueWithListMatch ::
  (ToSMT meta, Applicative m, lang ~ PlutusIR) =>
  TypeMeta lang meta ->
  TypeMeta lang meta ->
  TermMeta lang meta ->
  TermMeta lang meta ->
  TermMeta lang meta ->
  [ArgMeta lang meta] ->
  m (Maybe [Branch lang meta])
continueWithListMatch tyA tyR lst caseNil caseCons excess =
  continueWith
    "Nil_match"
    [ TyArg tyA,
      TermArg lst,
      TyArg tyR,
      TermArg $ caseNil `appN` excess,
      TermArg $ Lam (Ann "x") tyA $ Lam (Ann "xs") (tyListOf tyA) $ caseCons `appN` excess
    ]

-- | Indicates that the next step is an application of
-- the Data destructor. Note that this function does *not*
-- include any check for well-scopedness, so be careful!
continueWithDataMatch ::
  (ToSMT meta, Applicative m, lang ~ PlutusIR) =>
  ArgMeta lang meta ->
  ArgMeta lang meta ->
  TermMeta lang meta ->
  TermMeta lang meta ->
  TermMeta lang meta ->
  TermMeta lang meta ->
  TermMeta lang meta ->
  [ArgMeta lang meta] ->
  m (Maybe [Branch lang meta])
continueWithDataMatch dat tyR caseC caseM caseL caseI caseB excess =
  continueWith
    "Data_match"
    [ dat,
      tyR,
      TermArg $ Lam (Ann "i") (builtin PIRTypeInteger) $ Lam (Ann "ds") (tyListOf tyData) caseC `appN` excess,
      TermArg $ Lam (Ann "es") (tyListOf (tyTuple2Of tyData tyData)) $ caseM `appN` excess,
      TermArg $ Lam (Ann "ds") (tyListOf tyData) $ caseL `appN` excess,
      TermArg $ Lam (Ann "i") (builtin PIRTypeInteger) $ caseI `appN` excess,
      TermArg $ Lam (Ann "b") (builtin PIRTypeByteString) $ caseB `appN` excess
    ]

errorTerm :: AnnTerm ty ann (SystemF.VarMeta meta ann (TermBase lang))
errorTerm = App (Free Bottom) []

builtin :: PIRBuiltinType -> AnnType ann (SystemF.VarMeta meta ann (TypeBase PlutusIR))
builtin nm = TyApp (Free $ TyBuiltin nm) []
