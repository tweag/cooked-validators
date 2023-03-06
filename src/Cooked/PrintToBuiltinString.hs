{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Print all the types that occur on the 'TxInfo' to 'BuiltinString'. This is
-- useful for debugging of validators. You probably do not want to use this in
-- production code, as many of the functions in this module are wildly
-- inefficient due to limitations of the 'BuiltinString' type.
module Cooked.PrintToBuiltinString where

import Control.Monad hiding (fmap)
import Cooked.Currencies
import Cooked.MockChain
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.Script.Utils.Value as Value
import Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Contexts as Pl
import qualified PlutusTx
import qualified PlutusTx as Pl
import qualified PlutusTx.AssocMap as PlMap
import PlutusTx.Builtins
import PlutusTx.Prelude
import qualified Prelude as Haskell

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
    Application [BSSyntax]
  | -- | a list with a given opening, separator, and closing symbol (in that
    -- order).
    ListLike BuiltinString BuiltinString BuiltinString [BSSyntax]

PlutusTx.makeLift ''BSSyntax

{-# INLINEABLE printBSSyntax #-}
printBSSyntax :: BSSyntax -> BuiltinString
printBSSyntax expr = printBSSyntax' [simplBSSyntaxApplications expr] ""
  where
    -- make sure that every 'Application' holds a list of at least two
    -- elements. (i.e. a "constructor" applied to one or more "arguments")
    simplBSSyntaxApplications l@(ReversedCat _) = l
    simplBSSyntaxApplications (Application []) = ReversedCat []
    simplBSSyntaxApplications (Application [x]) = simplBSSyntaxApplications x
    simplBSSyntaxApplications (Application xs@(_ : _ : _)) =
      Application $
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
    printBSSyntax' (Application xs : rest) acc =
      printBSSyntax'
        ( ListLike
            ""
            " "
            ""
            ( map
                ( \case
                    Application ys ->
                      -- we know that ys contains at least two elements. Therefore, we
                      -- need parentheses:
                      ListLike "(" " " ")" ys
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

-- * 'TOBSSyntax' instances

{-# INLINEABLE literal #-}
literal :: BuiltinString -> BSSyntax
literal x = ReversedCat [x]

class ToBSSyntax a where
  toBSSyntax :: a -> BSSyntax

instance ToBSSyntax Integer where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax = ReversedCat . integerToReversedDigits

{-# INLINEABLE integerToReversedDigits #-}
integerToReversedDigits :: Integer -> [BuiltinString]
integerToReversedDigits n
  | n < 0 = reverse $ "-" : go (negate n) []
  | n == 0 = ["0"]
  | otherwise = reverse $ go n []
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
  toBSSyntax (Just x) = Application [literal "Just", toBSSyntax x]

instance (ToBSSyntax k, ToBSSyntax v) => ToBSSyntax (Map k v) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax m = Application [literal "fromList", toBSSyntax (PlMap.toList m)]

instance ToBSSyntax BuiltinByteString where
  -- base16 representation
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax s = ReversedCat $ "\"" : go 0 ["\""]
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
  toBSSyntax (TokenName x) = Application [literal "TokenName", toBSSyntax x]

instance ToBSSyntax CurrencySymbol where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (CurrencySymbol x) = Application [literal "CurrencySymbol ", toBSSyntax x]

instance ToBSSyntax Value where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Value m) = Application [literal "Value", toBSSyntax m]

instance ToBSSyntax TxId where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxId x) = Application [literal "TxId", toBSSyntax x]

instance ToBSSyntax TxOutRef where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxOutRef txid i) = Application [literal "TxOutRef", toBSSyntax txid, toBSSyntax i]

instance ToBSSyntax ValidatorHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (ValidatorHash h) = Application [literal "ValidatorHash", toBSSyntax h]

instance ToBSSyntax PubKeyHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (PubKeyHash h) = Application [literal "PubKeyHash", toBSSyntax h]

instance ToBSSyntax Credential where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (ScriptCredential scriptHash) = Application [literal "ScriptCredential", toBSSyntax scriptHash]
  toBSSyntax (PubKeyCredential pkh) = Application [literal "PubKeyCredential", toBSSyntax pkh]

instance ToBSSyntax StakingCredential where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (StakingHash cred) = Application [literal "StakingCredential", toBSSyntax cred]
  toBSSyntax (StakingPtr i j k) = Application $ literal "StakingPtr" : map toBSSyntax [i, j, k]

instance ToBSSyntax Address where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Address cred mStCred) = Application [literal "Address", toBSSyntax cred, toBSSyntax mStCred]

instance ToBSSyntax DatumHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (DatumHash h) = Application [literal "DatumHash", toBSSyntax h]

instance ToBSSyntax BuiltinData where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax _ = literal "TODO"

instance ToBSSyntax Datum where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Datum d) = Application [literal "Datum", toBSSyntax d]

instance ToBSSyntax OutputDatum where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax NoOutputDatum = literal "NoOutputDatum"
  toBSSyntax (OutputDatumHash h) = Application [literal "OutputDatumHash", toBSSyntax h]
  toBSSyntax (OutputDatum d) = Application [literal "OutputDatum", toBSSyntax d]

instance ToBSSyntax ScriptHash where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (ScriptHash h) = Application [literal "ScriptHash", toBSSyntax h]

instance ToBSSyntax TxOut where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxOut address value datum mRefScriptHash) =
    Application
      [ literal "TxOut",
        toBSSyntax address,
        toBSSyntax value,
        toBSSyntax datum,
        toBSSyntax mRefScriptHash
      ]

instance ToBSSyntax TxInInfo where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (TxInInfo oref out) = Application [literal "TxInInfo", toBSSyntax oref, toBSSyntax out]

instance ToBSSyntax POSIXTime where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (POSIXTime t) = Application [literal "POSIXTime", toBSSyntax t]

instance ToBSSyntax a => ToBSSyntax (Extended a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax NegInf = literal "NegInf"
  toBSSyntax PosInf = literal "PosInf"
  toBSSyntax (Finite x) = Application [literal "Finite", toBSSyntax x]

instance ToBSSyntax Bool where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax True = literal "True"
  toBSSyntax False = literal "False"

instance ToBSSyntax a => ToBSSyntax (LowerBound a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (LowerBound x closure) = Application [literal "LowerBound", toBSSyntax x, toBSSyntax closure]

instance ToBSSyntax a => ToBSSyntax (UpperBound a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (UpperBound x closure) = Application [literal "UpperBound", toBSSyntax x, toBSSyntax closure]

instance ToBSSyntax a => ToBSSyntax (Interval a) where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Interval lb ub) = Application [literal "Interval", toBSSyntax lb, toBSSyntax ub]

instance ToBSSyntax DCert where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax _ = literal "TODO"

-- data DCert
--   = DCertDelegRegKey StakingCredential
--   | DCertDelegDeRegKey StakingCredential
--   | DCertDelegDelegate
--       StakingCredential
--       -- ^ delegator
--       PubKeyHash
--       -- ^ delegatee
--   | -- | A digest of the PoolParams
--     DCertPoolRegister
--       PubKeyHash
--       -- ^ poolId
--       PubKeyHash
--       -- ^ pool VFR
--   | -- | The retirement certificate and the Epoch in which the retirement will take place
--     DCertPoolRetire PubKeyHash Integer -- NB: Should be Word64 but we only have Integer on-chain
--   | -- | A really terse Digest
--     DCertGenesis
--   | -- | Another really terse Digest
--     DCertMir
--     deriving stock (Eq, Ord, Show, Generic)
--     deriving anyclass (NFData)
--     deriving Pretty via (PrettyShow DCert)

instance ToBSSyntax ScriptPurpose where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Minting cs) = Application [literal "Minting", toBSSyntax cs]
  toBSSyntax (Spending oref) = Application [literal "Spending", toBSSyntax oref]
  toBSSyntax (Rewarding stCred) = Application [literal "Rewarding", toBSSyntax stCred]
  toBSSyntax (Certifying dCert) = Application [literal "Certifying", toBSSyntax dCert]

instance ToBSSyntax Redeemer where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax (Redeemer builtinData) = Application [literal "Redeemer", toBSSyntax builtinData]

instance ToBSSyntax TxInfo where
  {-# INLINEABLE toBSSyntax #-}
  toBSSyntax TxInfo {..} =
    Application
      [ -- literal "TxInfo",
        -- toBSSyntax txInfoInputs,
        toBSSyntax txInfoReferenceInputs,
        toBSSyntax txInfoOutputs,
        -- toBSSyntax txInfoFee,
        toBSSyntax txInfoMint
        -- toBSSyntax txInfoDCert,
        -- toBSSyntax txInfoWdrl,
        -- toBSSyntax txInfoValidRange,
        -- toBSSyntax txInfoSignatories,
        -- toBSSyntax txInfoRedeemers,
        -- toBSSyntax txInfoData,
        -- toBSSyntax txInfoId
      ]

-- * simple validator to test the printing here

bananaAssetClass :: Value.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Value.Value -> Integer
bananasIn v = Value.assetClassValueOf v bananaAssetClass

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution [(i, [Ada.lovelaceValueOf 20_000_000 <> banana 5]) | i <- knownWallets]

data UnitContract

instance Pl.ValidatorTypes UnitContract where
  type RedeemerType UnitContract = Bool
  type DatumType UnitContract = ()

printValidator :: Pl.TypedValidator UnitContract
printValidator =
  Pl.mkTypedValidator @UnitContract
    $$(Pl.compile [||print||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator
    print _ _ ctx =
      let txi = scriptContextTxInfo ctx
       in -- Just input@(TxInInfo oref out@(TxOut {txOutAddress = Address (ScriptCredential vh) _})) = Pl.findOwnInput ctx
          -- spentValue = txOutValue out
          trace (printBS txi) False

printTrace :: MonadBlockChain m => m ()
printTrace = do
  (oref, _) : _ <-
    utxosFromCardanoTx
      Haskell.<$> validateTxSkel
        txSkelTemplate
          { txSkelSigners = [wallet 1],
            txSkelOuts =
              [ paysScript
                  printValidator
                  ()
                  (Ada.lovelaceValueOf 30_000_000 <> banana 3)
              ]
          }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts =
            def
              { txOptUnsafeModTx =
                  [ RawModTxAfterBalancing Debug.traceShowId
                  -- V$ \tx ->
                  --   tx
                  ]
              },
          txSkelSigners = [wallet 1],
          txSkelIns = Map.singleton oref $ TxSkelRedeemerForScript True
        }
