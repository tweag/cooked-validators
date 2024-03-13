{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Print all the types that occur on the 'TxInfo' to 'BuiltinString'. This is
-- useful for debugging of validators. You probably do not want to use this in
-- production code, as many of the functions in this module are wildly
-- inefficient due to limitations of the 'BuiltinString' type.
--
-- If the functions from this module make script execution on your transactions
-- go over budget, consider using 'txOptEmulatorParamsModification' to
-- temporarily loosen the limits (at the cost of breaking compatibility with
-- mainnet)
module Cooked.ShowBS (ShowBS (..), showBSs, app_prec) where

import Plutus.V2.Ledger.Api
import qualified PlutusTx.AssocMap as PlMap
import PlutusTx.Builtins
import PlutusTx.Prelude

-- | analogue of Haskell's 'Show' class for use in Plutus scripts.
class ShowBS a where
  -- | analogue of 'show'
  {-# INLINEABLE showBS #-}
  showBS :: a -> BuiltinString
  showBS x = showBSsPrec 0 x ""

  -- | analogue of 'showsPrec'
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec :: Integer -> a -> BuiltinString -> BuiltinString
  showBSsPrec _ x s = showBS x <> s

-- | analogue of 'shows'
{-# INLINEABLE showBSs #-}
showBSs :: (ShowBS a) => a -> BuiltinString -> BuiltinString
showBSs = showBSsPrec 0

-- | Precedence of function applications
{-# INLINEABLE app_prec #-}
app_prec :: Integer
app_prec = 10

{-# INLINEABLE literal #-}
literal :: BuiltinString -> BuiltinString -> BuiltinString
literal = (<>)

{-# INLINEABLE cat #-}
cat :: [BuiltinString] -> BuiltinString -> BuiltinString
cat [] = id -- we cannot use foldr here for some reason with the plutus compiler
cat (x : xs) = literal x . cat xs

-- | print with a surrounding parenthesis, if the boolean argument is true
{-# INLINEABLE showBSParen #-}
showBSParen :: Bool -> (BuiltinString -> BuiltinString) -> BuiltinString -> BuiltinString
showBSParen False s = s
showBSParen True s = literal "(" . s . literal ")"

-- | print an application of a constructor to an argument
{-# INLINEABLE application1 #-}
application1 :: (ShowBS a) => Integer -> BuiltinString -> a -> BuiltinString -> BuiltinString
application1 prec f x = showBSParen (app_prec <= prec) $ literal f . literal " " . showBSsPrec app_prec x

-- | like 'application1' with two arguments
{-# INLINEABLE application2 #-}
application2 :: (ShowBS a, ShowBS b) => Integer -> BuiltinString -> a -> b -> BuiltinString -> BuiltinString
application2 prec f x y =
  showBSParen (app_prec <= prec) $
    literal f
      . literal " "
      . showBSsPrec app_prec x
      . literal " "
      . showBSsPrec app_prec y

-- | like 'application1' with three arguments
{-# INLINEABLE application3 #-}
application3 :: (ShowBS a, ShowBS b, ShowBS c) => Integer -> BuiltinString -> a -> b -> c -> BuiltinString -> BuiltinString
application3 prec f x y z =
  showBSParen (app_prec <= prec) $
    literal f
      . literal " "
      . showBSsPrec app_prec x
      . literal " "
      . showBSsPrec app_prec y
      . literal " "
      . showBSsPrec app_prec z

-- | like 'application1' with four arguments
{-# INLINEABLE application4 #-}
application4 :: (ShowBS a, ShowBS b, ShowBS c, ShowBS d) => Integer -> BuiltinString -> a -> b -> c -> d -> BuiltinString -> BuiltinString
application4 prec f x y z w =
  showBSParen (app_prec <= prec) $
    literal f
      . literal " "
      . showBSsPrec app_prec x
      . literal " "
      . showBSsPrec app_prec y
      . literal " "
      . showBSsPrec app_prec z
      . literal " "
      . showBSsPrec app_prec w

instance ShowBS Integer where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ i = cat (integerToDigits i)

{-# INLINEABLE integerToDigits #-}
integerToDigits :: Integer -> [BuiltinString]
integerToDigits n
  | n < 0 = "-" : go (negate n) []
  | n == 0 = ["0"]
  | otherwise = go n []
  where
    go i acc
      | i == 0 = acc
      | otherwise = let (q, r) = quotRem i 10 in go q (digitToBS r : acc)

{-# INLINEABLE digitToBS #-}
digitToBS :: Integer -> BuiltinString
digitToBS x
  | x == 0 = "0"
  | x == 1 = "1"
  | x == 2 = "2"
  | x == 3 = "3"
  | x == 4 = "4"
  | x == 5 = "5"
  | x == 6 = "6"
  | x == 7 = "7"
  | x == 8 = "8"
  | x == 9 = "9"
  | otherwise = "?"

instance (ShowBS a) => ShowBS [a] where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ = catList "[" "," "]" showBSs

{-# INLINEABLE catList #-}
catList :: BuiltinString -> BuiltinString -> BuiltinString -> (a -> BuiltinString -> BuiltinString) -> [a] -> BuiltinString -> BuiltinString
catList open _ close _ [] = literal open . literal close
catList open sep close print (x : xs) = literal open . print x . printSeparated xs . literal close
  where
    printSeparated [] = id
    printSeparated (y : ys) = literal sep . print y . printSeparated ys

instance (ShowBS a, ShowBS b) => ShowBS (a, b) where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ (x, y) = literal "(" . showBSs x . literal "," . showBSs y . literal ")"

instance ShowBS Bool where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ True = literal "True"
  showBSsPrec _ False = literal "False"

instance (ShowBS a) => ShowBS (Maybe a) where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ Nothing = literal "Nothing"
  showBSsPrec p (Just x) = application1 p "Just" x

instance (ShowBS k, ShowBS v) => ShowBS (Map k v) where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p m = application1 p "fromList" (PlMap.toList m)

instance ShowBS BuiltinByteString where
  -- base16 representation
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ bs = literal "\"" . cat (builtinByteStringCharacters bs) . literal "\""

{-# INLINEABLE builtinByteStringCharacters #-}
builtinByteStringCharacters :: BuiltinByteString -> [BuiltinString]
builtinByteStringCharacters s = go (len - 1) []
  where
    len = lengthOfByteString s

    go :: Integer -> [BuiltinString] -> [BuiltinString]
    go i acc
      | i >= 0 =
          let (highNibble, lowNibble) = quotRem (indexByteString s i) 16
           in go (i - 1) (toHex highNibble : toHex lowNibble : acc)
      | otherwise = acc

    toHex :: Integer -> BuiltinString
    toHex x
      | x <= 9 = digitToBS x
      | x == 10 = "a"
      | x == 11 = "b"
      | x == 12 = "c"
      | x == 13 = "d"
      | x == 14 = "e"
      | x == 15 = "f"
      | otherwise = "?"

instance ShowBS TokenName where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (TokenName x) = application1 p "TokenName" x

instance ShowBS CurrencySymbol where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (CurrencySymbol x) = application1 p "CurrencySymbol" x

instance ShowBS Value where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (Value m) = application1 p "Value" m

instance ShowBS TxId where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (TxId x) = application1 p "TxId" x

instance ShowBS TxOutRef where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (TxOutRef txid i) = application2 p "TxOutRef" txid i

instance ShowBS ValidatorHash where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (ValidatorHash h) = application1 p "ValidatorHash" h

instance ShowBS PubKeyHash where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (PubKeyHash h) = application1 p "PubKeyHash" h

instance ShowBS Credential where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (ScriptCredential scriptHash) = application1 p "ScriptCredential" scriptHash
  showBSsPrec p (PubKeyCredential pkh) = application1 p "PubKeyCredential" pkh

instance ShowBS StakingCredential where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (StakingHash cred) = application1 p "StakingCredential" cred
  showBSsPrec p (StakingPtr i j k) = application3 p "StakingPtr" i j k

instance ShowBS Address where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (Address cred mStCred) = application2 p "Address" cred mStCred

instance ShowBS DatumHash where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (DatumHash h) = application1 p "DatumHash" h

instance ShowBS BuiltinData where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p d = showBSParen (app_prec <= p) $ literal "BuiltinData " . builtinDataShowBSsPrec app_prec d

{-# INLINEABLE builtinDataShowBSsPrec #-}
builtinDataShowBSsPrec :: Integer -> BuiltinData -> BuiltinString -> BuiltinString
builtinDataShowBSsPrec p d =
  matchData
    d
    ( \i ds ->
        showBSParen (app_prec <= p) $
          literal "Constr "
            . showBSs i
            . literal " "
            . catList "[" "," "]" (builtinDataShowBSsPrec 0) ds
    )
    ( \alist ->
        showBSParen (app_prec <= p) $
          literal "Map "
            . catList
              "["
              ","
              "]"
              (\(a, b) -> literal "(" . builtinDataShowBSsPrec 0 a . literal "," . builtinDataShowBSsPrec 0 b . literal ")")
              alist
    )
    ( \list ->
        showBSParen (app_prec <= p) $
          literal "List "
            . catList "[" "," "]" (builtinDataShowBSsPrec 0) list
    )
    (\i -> showBSParen (app_prec <= p) $ literal "I " . showBSs i)
    (\bs -> showBSParen (app_prec <= p) $ literal "B " . showBSs bs)

instance ShowBS Datum where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (Datum d) = application1 p "Datum" d

instance ShowBS OutputDatum where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ NoOutputDatum = literal "NoOutputDatum"
  showBSsPrec p (OutputDatumHash h) = application1 p "OutputDatumHash" h
  showBSsPrec p (OutputDatum d) = application1 p "OutputDatum" d

instance ShowBS ScriptHash where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (ScriptHash h) = application1 p "ScriptHash" h

instance ShowBS TxOut where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (TxOut address value datum mRefScriptHash) = application4 p "TxOut" address value datum mRefScriptHash

instance ShowBS TxInInfo where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (TxInInfo oref out) = application2 p "TxInInfo" oref out

instance ShowBS POSIXTime where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (POSIXTime t) = application1 p "POSIXTime" t

instance (ShowBS a) => ShowBS (Extended a) where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec _ NegInf = literal "NegInf"
  showBSsPrec _ PosInf = literal "PosInf"
  showBSsPrec p (Finite x) = application1 p "Finite" x

instance (ShowBS a) => ShowBS (LowerBound a) where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (LowerBound x closure) = application2 p "LowerBound" x closure

instance (ShowBS a) => ShowBS (UpperBound a) where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (UpperBound x closure) = application2 p "UpperBound" x closure

instance (ShowBS a) => ShowBS (Interval a) where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (Interval lb ub) = application2 p "Interval" lb ub

instance ShowBS DCert where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (DCertDelegRegKey stCred) = application1 p "DCertDelegRegKey" stCred
  showBSsPrec p (DCertDelegDeRegKey stCred) = application1 p "DCertDelegDeRegKey" stCred
  showBSsPrec p (DCertDelegDelegate stCred pkh) = application2 p "DCertDelegDelegate" stCred pkh
  showBSsPrec p (DCertPoolRegister stCred1 stCred2) = application2 p "DCertPoolRegister" stCred1 stCred2
  showBSsPrec p (DCertPoolRetire stCred i) = application2 p "DCertPoolRetire" stCred i
  showBSsPrec _ DCertGenesis = literal "DCertGenesis"
  showBSsPrec _ DCertMir = literal "DCertMir"

instance ShowBS ScriptPurpose where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (Minting cs) = application1 p "Minting" cs
  showBSsPrec p (Spending oref) = application1 p "Spending" oref
  showBSsPrec p (Rewarding stCred) = application1 p "Rewarding" stCred
  showBSsPrec p (Certifying dCert) = application1 p "Certifying" dCert

instance ShowBS Redeemer where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p (Redeemer builtinData) = application1 p "Redeemer" builtinData

instance ShowBS TxInfo where
  {-# INLINEABLE showBSsPrec #-}
  showBSsPrec p TxInfo {..} =
    showBSParen (app_prec <= p) $
      literal "TxInfo "
        . showBSsPrec app_prec txInfoInputs
        . literal " "
        . showBSsPrec app_prec txInfoReferenceInputs
        . literal " "
        . showBSsPrec app_prec txInfoOutputs
        . literal " "
        . showBSsPrec app_prec txInfoFee
        . literal " "
        . showBSsPrec app_prec txInfoMint
        . literal " "
        . showBSsPrec app_prec txInfoDCert
        . literal " "
        . showBSsPrec app_prec txInfoWdrl
        . literal " "
        . showBSsPrec app_prec txInfoValidRange
        . literal " "
        . showBSsPrec app_prec txInfoSignatories
        . literal " "
        . showBSsPrec app_prec txInfoRedeemers
        . literal " "
        . showBSsPrec app_prec txInfoData
        . literal " "
        . showBSsPrec app_prec txInfoId
