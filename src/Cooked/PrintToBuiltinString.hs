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
module Cooked.PrintToBuiltinString (PrintBS (..), printBSs, app_prec) where

import Plutus.V2.Ledger.Api
import qualified PlutusTx.AssocMap as PlMap
import PlutusTx.Builtins
import PlutusTx.Prelude

-- | analogue of Haskell's 'Show' class for use in Plutus scripts.
class PrintBS a where
  -- | analogue of 'show'
  {-# INLINEABLE printBS #-}
  printBS :: a -> BuiltinString
  printBS x = printBSsPrec 0 x ""

  -- | analogue of 'showsPrec'
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec :: Integer -> a -> BuiltinString -> BuiltinString
  printBSsPrec _ x s = printBS x <> s

-- | analogue of 'shows'
{-# INLINEABLE printBSs #-}
printBSs :: PrintBS a => a -> BuiltinString -> BuiltinString
printBSs = printBSsPrec 0

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
{-# INLINEABLE printBSParen #-}
printBSParen :: Bool -> (BuiltinString -> BuiltinString) -> BuiltinString -> BuiltinString
printBSParen False s = s
printBSParen True s = literal "(" . s . literal ")"

-- | print an application of a constructor to an argument
{-# INLINEABLE application1 #-}
application1 :: PrintBS a => Integer -> BuiltinString -> a -> BuiltinString -> BuiltinString
application1 prec f x = printBSParen (app_prec < prec) $ literal f . literal " " . printBSsPrec (app_prec + 1) x

-- | like 'application1' with two arguments
{-# INLINEABLE application2 #-}
application2 :: (PrintBS a, PrintBS b) => Integer -> BuiltinString -> a -> b -> BuiltinString -> BuiltinString
application2 prec f x y =
  printBSParen (app_prec < prec) $
    literal f
      . literal " "
      . printBSsPrec (app_prec + 1) x
      . literal " "
      . printBSsPrec (app_prec + 1) y

-- | like 'application1' with three arguments
{-# INLINEABLE application3 #-}
application3 :: (PrintBS a, PrintBS b, PrintBS c) => Integer -> BuiltinString -> a -> b -> c -> BuiltinString -> BuiltinString
application3 prec f x y z =
  printBSParen (app_prec < prec) $
    literal f
      . literal " "
      . printBSsPrec (app_prec + 1) x
      . literal " "
      . printBSsPrec (app_prec + 1) y
      . literal " "
      . printBSsPrec (app_prec + 1) z

-- | like 'application1' with four arguments
{-# INLINEABLE application4 #-}
application4 :: (PrintBS a, PrintBS b, PrintBS c, PrintBS d) => Integer -> BuiltinString -> a -> b -> c -> d -> BuiltinString -> BuiltinString
application4 prec f x y z w =
  printBSParen (app_prec < prec) $
    literal f
      . literal " "
      . printBSsPrec (app_prec + 1) x
      . literal " "
      . printBSsPrec (app_prec + 1) y
      . literal " "
      . printBSsPrec (app_prec + 1) z
      . literal " "
      . printBSsPrec (app_prec + 1) w

instance PrintBS Integer where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ i = cat (integerToDigits i)

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

instance PrintBS a => PrintBS [a] where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ = catList "[" "," "]" printBSs

{-# INLINEABLE catList #-}
catList :: BuiltinString -> BuiltinString -> BuiltinString -> (a -> BuiltinString -> BuiltinString) -> [a] -> BuiltinString -> BuiltinString
catList open _ close _ [] = literal open . literal close
catList open sep close print (x : xs) = literal open . print x . printSeparated xs . literal close
  where
    printSeparated [] = id
    printSeparated (y : ys) = literal sep . print y . printSeparated ys

instance (PrintBS a, PrintBS b) => PrintBS (a, b) where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ (x, y) = literal "(" . printBSs x . literal "," . printBSs y . literal ")"

instance PrintBS Bool where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ True = literal "True"
  printBSsPrec _ False = literal "False"

instance PrintBS a => PrintBS (Maybe a) where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ Nothing = literal "Nothing"
  printBSsPrec p (Just x) = application1 p "Just" x

instance (PrintBS k, PrintBS v) => PrintBS (Map k v) where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p m = application1 p "fromList" (PlMap.toList m)

instance PrintBS BuiltinByteString where
  -- base16 representation
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ bs = literal "\"" . cat (builtinByteStringCharacters bs) . literal "\""

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

instance PrintBS TokenName where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (TokenName x) = application1 p "TokenName" x

instance PrintBS CurrencySymbol where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (CurrencySymbol x) = application1 p "CurrencySymbol" x

instance PrintBS Value where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (Value m) = application1 p "Value" m

instance PrintBS TxId where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (TxId x) = application1 p "TxId" x

instance PrintBS TxOutRef where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (TxOutRef txid i) = application2 p "TxOutRef" txid i

instance PrintBS ValidatorHash where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (ValidatorHash h) = application1 p "ValidatorHash" h

instance PrintBS PubKeyHash where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (PubKeyHash h) = application1 p "PubKeyHash" h

instance PrintBS Credential where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (ScriptCredential scriptHash) = application1 p "ScriptCredential" scriptHash
  printBSsPrec p (PubKeyCredential pkh) = application1 p "PubKeyCredential" pkh

instance PrintBS StakingCredential where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (StakingHash cred) = application1 p "StakingCredential" cred
  printBSsPrec p (StakingPtr i j k) = application3 p "StakingPtr" i j k

instance PrintBS Address where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (Address cred mStCred) = application2 p "Address" cred mStCred

instance PrintBS DatumHash where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (DatumHash h) = application1 p "DatumHash" h

instance PrintBS BuiltinData where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p d = printBSParen (app_prec < p) $ literal "BuiltinData " . builtinDataPrintBSsPrec (app_prec + 1) d

{-# INLINEABLE builtinDataPrintBSsPrec #-}
builtinDataPrintBSsPrec :: Integer -> BuiltinData -> BuiltinString -> BuiltinString
builtinDataPrintBSsPrec p d =
  matchData
    d
    ( \i ds ->
        printBSParen (app_prec < p) $
          literal "Constr "
            . printBSs i
            . literal " "
            . catList "[" "," "]" (builtinDataPrintBSsPrec 0) ds
    )
    ( \alist ->
        printBSParen (app_prec < p) $
          literal "Map "
            . catList
              "["
              ","
              "]"
              (\(a, b) -> literal "(" . builtinDataPrintBSsPrec 0 a . literal "," . builtinDataPrintBSsPrec 0 b . literal ")")
              alist
    )
    ( \list ->
        printBSParen (app_prec < p) $
          literal "List "
            . catList "[" "," "]" (builtinDataPrintBSsPrec 0) list
    )
    (\i -> printBSParen (app_prec < p) $ literal "I " . printBSs i)
    (\bs -> printBSParen (app_prec < p) $ literal "B " . printBSs bs)

instance PrintBS Datum where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (Datum d) = application1 p "Datum" d

instance PrintBS OutputDatum where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ NoOutputDatum = literal "NoOutputDatum"
  printBSsPrec p (OutputDatumHash h) = application1 p "OutputDatumHash" h
  printBSsPrec p (OutputDatum d) = application1 p "OutputDatum" d

instance PrintBS ScriptHash where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (ScriptHash h) = application1 p "ScriptHash" h

instance PrintBS TxOut where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (TxOut address value datum mRefScriptHash) = application4 p "TxOut" address value datum mRefScriptHash

instance PrintBS TxInInfo where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (TxInInfo oref out) = application2 p "TxInInfo" oref out

instance PrintBS POSIXTime where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (POSIXTime t) = application1 p "POSIXTime" t

instance PrintBS a => PrintBS (Extended a) where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec _ NegInf = literal "NegInf"
  printBSsPrec _ PosInf = literal "PosInf"
  printBSsPrec p (Finite x) = application1 p "Finite" x

instance PrintBS a => PrintBS (LowerBound a) where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (LowerBound x closure) = application2 p "LowerBound" x closure

instance PrintBS a => PrintBS (UpperBound a) where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (UpperBound x closure) = application2 p "UpperBound" x closure

instance PrintBS a => PrintBS (Interval a) where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (Interval lb ub) = application2 p "Interval" lb ub

instance PrintBS DCert where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (DCertDelegRegKey stCred) = application1 p "DCertDelegRegKey" stCred
  printBSsPrec p (DCertDelegDeRegKey stCred) = application1 p "DCertDelegDeRegKey" stCred
  printBSsPrec p (DCertDelegDelegate stCred pkh) = application2 p "DCertDelegDelegate" stCred pkh
  printBSsPrec p (DCertPoolRegister stCred1 stCred2) = application2 p "DCertPoolRegister" stCred1 stCred2
  printBSsPrec p (DCertPoolRetire stCred i) = application2 p "DCertPoolRetire" stCred i
  printBSsPrec _ DCertGenesis = literal "DCertGenesis"
  printBSsPrec _ DCertMir = literal "DCertMir"

instance PrintBS ScriptPurpose where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (Minting cs) = application1 p "Minting" cs
  printBSsPrec p (Spending oref) = application1 p "Spending" oref
  printBSsPrec p (Rewarding stCred) = application1 p "Rewarding" stCred
  printBSsPrec p (Certifying dCert) = application1 p "Certifying" dCert

instance PrintBS Redeemer where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p (Redeemer builtinData) = application1 p "Redeemer" builtinData

-- In an ideal world, the following instance would print the whole 'TxInfo'. The
-- sad reality is that this causes the script to go over budget on even the
-- simplest of examples. TODO: investigate how to adjust the execution
-- budget.
--
instance PrintBS TxInfo where
  {-# INLINEABLE printBSsPrec #-}
  printBSsPrec p TxInfo {..} =
    printBSParen (app_prec < p) $
      literal "TxInfo "
        . printBSsPrec (app_prec + 1) txInfoInputs
        . printBSsPrec (app_prec + 1) txInfoReferenceInputs
        . printBSsPrec (app_prec + 1) txInfoOutputs
        . printBSsPrec (app_prec + 1) txInfoFee
        . printBSsPrec (app_prec + 1) txInfoMint
        . printBSsPrec (app_prec + 1) txInfoDCert
        . printBSsPrec (app_prec + 1) txInfoWdrl
        . printBSsPrec (app_prec + 1) txInfoValidRange
        . printBSsPrec (app_prec + 1) txInfoSignatories
        . printBSsPrec (app_prec + 1) txInfoRedeemers
        . printBSsPrec (app_prec + 1) txInfoData
        . printBSsPrec (app_prec + 1) txInfoId
