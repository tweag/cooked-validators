{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Print all the types that occur on the 'TxInfo' to 'BuiltinString'. This is
-- useful for debugging of validators. You probably do not want to use this in
-- production code, as many of the functions in this module are wildly
-- inefficient due to limitations of the 'BuiltinString' type.
module Cooked.PrintToBuiltinString (printBS) where

import Plutus.V2.Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlMap
import PlutusTx.Builtins
import PlutusTx.Prelude

-- | An abstract representation of a piece of data, used to generate a
-- 'BuiltinString' representation. (See 'printBSSyntax')
data BSSyntax
  = -- | a list of strings to concatenate. For efficiency reasons, the order is
    -- reversed, so that @ReversedCat ["ef", "bcd", "", "a"]@ is a
    -- representation of @"abcdef"@.
    ReversedCat [BuiltinString]
  | -- | a constructor applied to some arguments. Empty Applications are
    -- representations of the empty string, and one-element applications are
    -- constants.
    --
    -- The boolean controls whether parentheses are printed around the
    -- application. It is a leaky implementation detail though, and you'll
    -- probably want to use the 'application' smart constructor when defining
    -- 'ToBSSyntax' instances.
    Application Bool [BSSyntax]
  | -- | a list with a given opening, separator, and closing symbol (in that
    -- order).
    ListLike BuiltinString BuiltinString BuiltinString [BSSyntax]

PlutusTx.makeLift ''BSSyntax

{-# INLINEABLE literal #-}
literal :: BuiltinString -> BSSyntax
literal x = ReversedCat [x]

{-# INLINEABLE application #-}
application :: [BSSyntax] -> BSSyntax
application = Application False -- by default, use no parentheses around applications

{-# INLINEABLE printBSSyntax #-}
printBSSyntax :: BSSyntax -> BuiltinString
printBSSyntax expr = printBSSyntax' [simplBSSyntaxApplications expr] ""
  where
    -- make sure that every 'Application' holds a list of at least two
    -- elements. (i.e. a "constructor" applied to one or more "arguments")
    simplBSSyntaxApplications l@(ReversedCat _) = l
    simplBSSyntaxApplications (Application _ []) = ReversedCat []
    simplBSSyntaxApplications (Application _ [x]) = simplBSSyntaxApplications x
    simplBSSyntaxApplications (Application b xs@(_ : _ : _)) =
      Application b $
        simplBSSyntaxApplications <$> xs
    simplBSSyntaxApplications (ListLike open sep close xs) =
      ListLike open sep close $
        simplBSSyntaxApplications <$> xs

    -- The first argument is a stack, the head of which is whatever should be
    -- prepended to the string under construction next. I take this slightly
    -- convoluted approach in order to avoid the "painter's algorithm"
    -- associated with the linear complexity of '<>' on 'BuiltinString'.
    printBSSyntax' :: [BSSyntax] -> BuiltinString -> BuiltinString
    printBSSyntax' [] acc = acc
    printBSSyntax' (ReversedCat [] : rest) acc = printBSSyntax' rest acc
    printBSSyntax' (ReversedCat (x : xs) : rest) acc =
      printBSSyntax'
        (ReversedCat xs : rest)
        (x <> acc)
    printBSSyntax' (Application b xs : rest) acc =
      printBSSyntax'
        ( (if b then ListLike "(" " " ")" else ListLike "" " " "")
            ( map
                ( \case
                    Application _ ys ->
                      -- we know that ys contains at least two elements. Therefore, we
                      -- need parentheses:
                      Application True ys
                    y -> y
                )
                xs
            ) :
          rest
        )
        acc
    printBSSyntax' (ListLike open sep close xs : rest) acc =
      printBSSyntax'
        (literal close : reverse (literal open : separateWith (literal sep) xs) ++ rest)
        acc

    separateWith :: a -> [a] -> [a]
    separateWith _ [] = []
    separateWith _ [x] = [x]
    separateWith sep (x : xs@(_ : _)) = x : sep : separateWith sep xs

{-# INLINEABLE printBS #-}
printBS :: ToBSSyntax a => a -> BuiltinString
printBS = printBSSyntax . toBSSyntax

-- * The class 'TOBSSyntax' and its instances

class ToBSSyntax a where
  toBSSyntax :: a -> BSSyntax

instance ToBSSyntax Integer where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax = integerToBSSyntax

{-# INLINEABLE integerToBSSyntax #-}
integerToBSSyntax :: Integer -> BSSyntax
integerToBSSyntax n
  | n < 0 = ReversedCat $ reverse $ "-" : go (negate n) []
  | n == 0 = literal "0"
  | otherwise = ReversedCat $ reverse $ go n []
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

instance ToBSSyntax a => ToBSSyntax [a] where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax = ListLike "[" ", " "]" . fmap toBSSyntax

instance (ToBSSyntax a, ToBSSyntax b) => ToBSSyntax (a, b) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (x, y) = ListLike "(" ", " ")" [toBSSyntax x, toBSSyntax y]

instance ToBSSyntax a => ToBSSyntax (Maybe a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax Nothing = literal "Nothing"
  toBSSyntax (Just x) = application [literal "Just", toBSSyntax x]

instance (ToBSSyntax k, ToBSSyntax v) => ToBSSyntax (Map k v) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax m = application [literal "fromList", toBSSyntax (PlMap.toList m)]

instance ToBSSyntax BuiltinByteString where
  -- base16 representation
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax = builtinByteStringToBSSyntax

{-# INLINEABLE builtinByteStringToBSSyntax #-}
builtinByteStringToBSSyntax :: BuiltinByteString -> BSSyntax
builtinByteStringToBSSyntax s = ReversedCat $ "\"" : go 0 ["\""]
  where
    len = lengthOfByteString s

    go :: Integer -> [BuiltinString] -> [BuiltinString]
    go i acc
      | i < len =
        let (highNibble, lowNibble) = quotRem (indexByteString s i) 16
         in go (i + 1) (toHex lowNibble : toHex highNibble : acc)
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

instance ToBSSyntax TokenName where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TokenName x) = application [literal "TokenName", toBSSyntax x]

instance ToBSSyntax CurrencySymbol where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (CurrencySymbol x) = application [literal "CurrencySymbol ", toBSSyntax x]

instance ToBSSyntax Value where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Value m) = application [literal "Value", toBSSyntax m]

instance ToBSSyntax TxId where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxId x) = application [literal "TxId", toBSSyntax x]

instance ToBSSyntax TxOutRef where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxOutRef txid i) = application [literal "TxOutRef", toBSSyntax txid, toBSSyntax i]

instance ToBSSyntax ValidatorHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (ValidatorHash h) = application [literal "ValidatorHash", toBSSyntax h]

instance ToBSSyntax PubKeyHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (PubKeyHash h) = application [literal "PubKeyHash", toBSSyntax h]

instance ToBSSyntax Credential where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (ScriptCredential scriptHash) = application [literal "ScriptCredential", toBSSyntax scriptHash]
  toBSSyntax (PubKeyCredential pkh) = application [literal "PubKeyCredential", toBSSyntax pkh]

instance ToBSSyntax StakingCredential where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (StakingHash cred) = application [literal "StakingCredential", toBSSyntax cred]
  toBSSyntax (StakingPtr i j k) = application $ literal "StakingPtr" : map toBSSyntax [i, j, k]

instance ToBSSyntax Address where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Address cred mStCred) = application [literal "Address", toBSSyntax cred, toBSSyntax mStCred]

instance ToBSSyntax DatumHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (DatumHash h) = application [literal "DatumHash", toBSSyntax h]

instance ToBSSyntax BuiltinData where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax d = application [literal "BuiltinData", builtinDataToBSSyntax d]

{-# INLINEABLE builtinDataToBSSyntax #-}
builtinDataToBSSyntax :: BuiltinData -> BSSyntax
builtinDataToBSSyntax d =
  matchData
    d
    ( \i ds ->
        application
          [ literal "Constr",
            integerToBSSyntax i,
            ListLike "[" ", " "]" (fmap builtinDataToBSSyntax ds)
          ]
    )
    ( \alist ->
        application
          [ literal "Map",
            ListLike
              "["
              ", "
              "]"
              ( fmap
                  ( \(a, b) ->
                      ListLike
                        "("
                        ", "
                        ")"
                        [ builtinDataToBSSyntax a,
                          builtinDataToBSSyntax b
                        ]
                  )
                  alist
              )
          ]
    )
    ( \list ->
        application
          [ literal "List",
            ListLike "[" ", " "]" (fmap builtinDataToBSSyntax list)
          ]
    )
    ( \i ->
        application [literal "I", integerToBSSyntax i]
    )
    ( \bs ->
        application [literal "B", builtinByteStringToBSSyntax bs]
    )

instance ToBSSyntax Datum where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Datum d) = application [literal "Datum", toBSSyntax d]

instance ToBSSyntax OutputDatum where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax NoOutputDatum = literal "NoOutputDatum"
  toBSSyntax (OutputDatumHash h) = application [literal "OutputDatumHash", toBSSyntax h]
  toBSSyntax (OutputDatum d) = application [literal "OutputDatum", toBSSyntax d]

instance ToBSSyntax ScriptHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (ScriptHash h) = application [literal "ScriptHash", toBSSyntax h]

instance ToBSSyntax TxOut where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxOut address value datum mRefScriptHash) =
    application
      [ literal "TxOut",
        toBSSyntax address,
        toBSSyntax value,
        toBSSyntax datum,
        toBSSyntax mRefScriptHash
      ]

instance ToBSSyntax TxInInfo where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxInInfo oref out) = application [literal "TxInInfo", toBSSyntax oref, toBSSyntax out]

instance ToBSSyntax POSIXTime where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (POSIXTime t) = application [literal "POSIXTime", toBSSyntax t]

instance ToBSSyntax a => ToBSSyntax (Extended a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax NegInf = literal "NegInf"
  toBSSyntax PosInf = literal "PosInf"
  toBSSyntax (Finite x) = application [literal "Finite", toBSSyntax x]

instance ToBSSyntax Bool where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax True = literal "True"
  toBSSyntax False = literal "False"

instance ToBSSyntax a => ToBSSyntax (LowerBound a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (LowerBound x closure) = application [literal "LowerBound", toBSSyntax x, toBSSyntax closure]

instance ToBSSyntax a => ToBSSyntax (UpperBound a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (UpperBound x closure) = application [literal "UpperBound", toBSSyntax x, toBSSyntax closure]

instance ToBSSyntax a => ToBSSyntax (Interval a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Interval lb ub) = application [literal "Interval", toBSSyntax lb, toBSSyntax ub]

instance ToBSSyntax DCert where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (DCertDelegRegKey stCred) = application [literal "DCertDelegRegKey", toBSSyntax stCred]
  toBSSyntax (DCertDelegDeRegKey stCred) = application [literal "DCertDelegDeRegKey", toBSSyntax stCred]
  toBSSyntax (DCertDelegDelegate stCred pkh) = application [literal "DCertDelegDelegate", toBSSyntax stCred, toBSSyntax pkh]
  toBSSyntax (DCertPoolRegister stCred1 stCred2) = application [literal "DCertPoolRegister", toBSSyntax stCred1, toBSSyntax stCred2]
  toBSSyntax (DCertPoolRetire stCred i) = application [literal "DCertPoolRetire", toBSSyntax stCred, toBSSyntax i]
  toBSSyntax DCertGenesis = literal "DCertGenesis"
  toBSSyntax DCertMir = literal "DCertMir"

instance ToBSSyntax ScriptPurpose where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Minting cs) = application [literal "Minting", toBSSyntax cs]
  toBSSyntax (Spending oref) = application [literal "Spending", toBSSyntax oref]
  toBSSyntax (Rewarding stCred) = application [literal "Rewarding", toBSSyntax stCred]
  toBSSyntax (Certifying dCert) = application [literal "Certifying", toBSSyntax dCert]

instance ToBSSyntax Redeemer where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Redeemer builtinData) = application [literal "Redeemer", toBSSyntax builtinData]

-- In an ideal world, the following instance would print the whole 'TxInfo'. The
-- sad reality is that this causes the script to go over budget on even the
-- simplest of examples. TODO: investigate how to make adjust the execution
-- budget.
--
-- instance ToBSSyntax TxInfo where
--   {-# INLINEABLE toBSSyntax #-}
--   toBSSyntax TxInfo {..} =
--     application
--       [ literal "TxInfo",
--         toBSSyntax txInfoInputs,
--         toBSSyntax txInfoReferenceInputs,
--         toBSSyntax txInfoOutputs,
--         toBSSyntax txInfoFee,
--         toBSSyntax txInfoMint,
--         toBSSyntax txInfoDCert,
--         toBSSyntax txInfoWdrl,
--         toBSSyntax txInfoValidRange,
--         toBSSyntax txInfoSignatories,
--         toBSSyntax txInfoRedeemers,
--         toBSSyntax txInfoData,
--         toBSSyntax txInfoId
--       ]
