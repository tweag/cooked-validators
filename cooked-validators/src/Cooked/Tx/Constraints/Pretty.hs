{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.Tx.Constraints.Pretty where

import Cooked.Currencies (permanentCurrencySymbol, quickCurrencySymbol)
import Cooked.MockChain.Misc
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Char
import Data.Default
import Data.Either
import qualified Data.List.NonEmpty as NEList
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (TxOut, mintingPolicyHash, unspentOutputs, validatorHash)
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorAddress, validatorHash)
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V2.Scripts as Pl (mintingPolicyHash)
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.IsData.Class as Pl
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import Test.QuickCheck (NonZero)
import Test.Tasty.QuickCheck (NonZero (..))

-- prettyEnum "Foo" "-" ["bar1", "bar2", "bar3"]
--    Foo
--      - bar1
--      - bar2
--      - bar3
prettyEnum :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
prettyEnum title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 . PP.vsep $
        map (bullet <+>) items
    ]

prettyEnumNonEmpty :: Doc ann -> Doc ann -> [Doc ann] -> Maybe (Doc ann)
prettyEnumNonEmpty _ _ [] = Nothing
prettyEnumNonEmpty title bullet items = Just $ prettyEnum title bullet items

prettyTxSkel :: Map Pl.TxOutRef Pl.TxOut -> Map Pl.DatumHash (Pl.Datum, String) -> TxSkel -> Doc ann
prettyTxSkel managedTxOuts managedDatums (TxSkel lbl opts mints validityRange signers ins outs fee) =
  -- undefined
  PP.vsep $
    "Transaction Skeleton:" :
    map
      ("-" <+>)
      ( catMaybes
          [ prettyEnumNonEmpty "Labels:" "-" (PP.viaShow <$> Set.toList lbl),
            -- fmap ("Opts:" <+>) (prettyOpts opts),
            prettyEnumNonEmpty "Mints:" "-" (prettyMints <$> (mints ^. mintsListIso)),
            Just $ "Validity interval:" <+> PP.pretty validityRange,
            prettyEnumNonEmpty "Signers:" "-" (prettyPubKeyHash . walletPKHash <$> NEList.toList signers),
            -- TODO handle unsafe 'fromJust' better
            prettyEnumNonEmpty "Inputs:" "-" (Maybe.fromJust . prettyTxSkelIn managedTxOuts managedDatums <$> Map.toList ins),
            prettyEnumNonEmpty "Outputs:" "-" (prettyTxSkelOut <$> outs)
          ]
      )

-- prettyPubKeyHash
--
-- If the pubkey is a know wallet
-- #abcdef (wallet 3)
--
-- Otherwise
-- #123456
--
prettyPubKeyHash :: Pl.PubKeyHash -> Doc ann
prettyPubKeyHash pkh =
  case walletPKHashToId pkh of
    Nothing -> prettyHash pkh
    Just walletId ->
      prettyHash pkh
        <+> PP.parens ("wallet" <+> PP.viaShow walletId)

-- prettyMints
--
-- Examples without and with redeemer
-- #abcdef "Foo" -> 500
-- #123456 "Bar" | Redeemer -> 1000
prettyMints :: (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) -> Doc ann
prettyMints (Pl.Versioned policy _, NoMintsRedeemer, tokenName, NonZero amount) =
  prettyMintingPolicy policy
    <+> PP.viaShow tokenName
    <+> "->"
    <+> PP.viaShow amount
prettyMints (Pl.Versioned policy _, SomeMintsRedeemer redeemer, tokenName, NonZero amount) =
  prettyMintingPolicy policy
    <+> PP.viaShow tokenName
    <+> "|"
    <+> PP.viaShow redeemer
    <+> "->"
    <+> PP.viaShow amount

prettyAddress :: Pl.Address -> Doc ann
prettyAddress (Pl.Address addrCr _stakingCred) =
  -- TODO print staking credentials
  case addrCr of
    (Pl.ScriptCredential vh) -> "script" <+> prettyHash vh
    (Pl.PubKeyCredential pkh) -> "pubkey" <+> prettyPubKeyHash pkh

prettyTxSkelOut :: TxSkelOut -> Doc ann
prettyTxSkelOut (Pays output) =
  prettyEnum
    ("Pays to" <+> prettyAddress (outputAddress output))
    "-"
    ( prettyValue (outputValue output) :
      case outputOutputDatum output of
        Pl.OutputDatum _datum -> ["Datum (inlined):" <+> PP.viaShow (output ^. outputDatumL)]
        Pl.OutputDatumHash _datum -> ["Datum (hashed):" <+> PP.viaShow (output ^. outputDatumL)]
        Pl.NoOutputDatum -> []
    )

prettyTxSkelIn :: Map Pl.TxOutRef Pl.TxOut -> Map Pl.DatumHash (Pl.Datum, String) -> (Pl.TxOutRef, TxSkelRedeemer) -> Maybe (Doc ann)
prettyTxSkelIn managedTxOuts managedDatums (txOutRef, txSkelRedeemer) = do
  output <- Map.lookup txOutRef managedTxOuts
  datumDoc <-
    case outputOutputDatum output of
      Pl.OutputDatum datum ->
        do
          datumStr <- Map.lookup (Pl.datumHash datum) managedDatums
          return $ Just ("Datum (inlined):" <+> PP.pretty datumStr)
      Pl.OutputDatumHash datumHash ->
        do
          datumStr <- Map.lookup datumHash managedDatums
          return $ Just ("Datum (hashed):" <+> PP.pretty datumStr)
      Pl.NoOutputDatum -> return Nothing
  let redeemerDoc =
        case txSkelRedeemer of
          TxSkelRedeemerForScript redeemer -> Just ("Redeemer:" <+> PP.viaShow redeemer)
          _ -> Nothing
  return $
    prettyEnum
      ("Spends from" <+> prettyAddress (outputAddress output))
      "-"
      (prettyValue (outputValue output) : catMaybes [redeemerDoc, datumDoc])

-- prettyHash 28a3d93cc3daac
-- #28a3d9
prettyHash :: (Show a) => a -> Doc ann
prettyHash = PP.pretty . ('#' :) . take 7 . show

prettyMintingPolicy :: Pl.MintingPolicy -> Doc ann
prettyMintingPolicy = prettyHash . Pl.mintingPolicyHash

-- prettyValue example output:
--
-- Value:
--   - Lovelace: 45_000_000
--   - [Q] "hello": 3
--   - #12bc3d "usertoken": 1
--
-- In case of an empty value (even though not an empty map):
-- Empty value
--
prettyValue :: Pl.Value -> Doc ann
prettyValue =
  Maybe.fromMaybe "Empty value"
    . prettyEnumNonEmpty "Value:" "-"
    . map prettySingletonValue
    . filter (\(_, _, n) -> n /= 0)
    . Pl.flattenValue
  where
    prettySingletonValue :: (Pl.CurrencySymbol, Pl.TokenName, Integer) -> Doc ann
    prettySingletonValue (symbol, name, amount) =
      prettyAssetClass <> ":" <+> prettyNumericUnderscore amount
      where
        prettyAssetClass
          | symbol == Pl.CurrencySymbol "" = "Lovelace"
          | symbol == quickCurrencySymbol = "[Q]" <+> PP.pretty name
          | symbol == permanentCurrencySymbol = "[P]" <+> PP.pretty name
          | otherwise = prettyHash symbol <+> PP.pretty name

    -- prettyNumericUnderscore 23798423723
    -- 23_798_423_723
    prettyNumericUnderscore :: Integer -> Doc ann
    prettyNumericUnderscore i
      | 0 == i = "0"
      | i > 0 = psnTerm "" 0 i
      | otherwise = "-" <> psnTerm "" 0 (- i)
      where
        psnTerm :: Doc ann -> Integer -> Integer -> Doc ann
        psnTerm acc _ 0 = acc
        psnTerm acc 3 nb = psnTerm (PP.pretty (nb `mod` 10) <> "_" <> acc) 1 (nb `div` 10)
        psnTerm acc n nb = psnTerm (PP.pretty (nb `mod` 10) <> acc) (n + 1) (nb `div` 10)

-- | Prettifies a 'TxOpts'; returns 'Nothing' if we're looking at default options.
-- prettyOpts :: TxOpts -> Maybe (Doc ann)
-- prettyOpts opts = case mapMaybe cmpAgainstDefAndPrint fields of
--   [] -> Nothing
--   xs -> Just $ PP.sep $ map (PP.semi <+>) xs
--   where
--     cmpAgainstDefAndPrint :: Field TxOpts -> Maybe (Doc ann)
--     cmpAgainstDefAndPrint (Field fn f)
--       | f opts == f def = Nothing
--       | otherwise = Just $ PP.pretty fn <> PP.colon <+> PP.viaShow (f opts)

--     -- Internal: if you add fields to TxOpts, make sure to add them here.
--     fields :: [Field TxOpts]
--     fields =
--       [ Field "adjustUnbalTx" adjustUnbalTx,
--         Field "awaitTxConfirmed" awaitTxConfirmed,
--         Field "autoSlotIncrease" autoSlotIncrease,
--         Field "unsafeModTx" unsafeModTx,
--         Field "balance" balance,
--         Field "balanceOutputPolicy" balanceOutputPolicy
--       ]
