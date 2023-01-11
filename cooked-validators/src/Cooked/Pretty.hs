{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.Pretty where

import Control.Arrow (second)
import Cooked.Currencies (permanentCurrencySymbol, quickCurrencySymbol)
import Cooked.MockChain.UtxoState (UtxoDatum (utxoShow), UtxoState (utxoState), UtxoValueSet (utxoValueSet))
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Default
import Data.Function (on)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (TxOut, mintingPolicyHash, unspentOutputs, validatorHash)
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V2.Scripts as Pl (mintingPolicyHash)
import qualified Plutus.V2.Ledger.Api as Pl
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
prettyTxSkel managedTxOuts managedDatums (TxSkel lbl opts mints validityRange signers ins outs _fee) =
  -- undefined
  prettyEnum
    "Transaction Skeleton:"
    "-"
    ( catMaybes
        [ prettyEnumNonEmpty "Labels:" "-" (PP.viaShow <$> Set.toList lbl),
          mPrettyTxOpts opts,
          prettyEnumNonEmpty "Mints:" "-" (prettyMints <$> (mints ^. mintsListIso)),
          Just $ "Validity interval:" <+> PP.pretty validityRange,
          prettyEnumNonEmpty "Signers:" "-" (prettySigners opts signers),
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

-- | Same as 'prettyPubKeyHash' with a suffix mentionning this is the balancing
-- wallet
prettyBalancingWallet :: Wallet -> Doc ann
prettyBalancingWallet w =
  prettyPubKeyHash (walletPKHash w) <+> "[Balancing]"

-- | Prints a list of pubkeys with a flag next to the balancing wallet
prettySigners :: TxOpts -> NEList.NonEmpty Wallet -> [Doc ann]
prettySigners TxOpts {balanceWallet = BalanceWithFirstSigner} (firstSigner NEList.:| signers) =
  prettyBalancingWallet firstSigner : (prettyPubKeyHash . walletPKHash <$> signers)
prettySigners TxOpts {balanceWallet = BalanceWith balancingWallet} signers =
  aux (NEList.toList signers)
  where
    aux :: [Wallet] -> [Doc ann]
    aux [] = []
    aux (s : ss)
      | s == balancingWallet = prettyBalancingWallet balancingWallet : aux ss
      | otherwise = prettyPubKeyHash (walletPKHash s) : aux ss

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
        Pl.OutputDatum _datum ->
          [ "Datum (inlined):"
              <+> (PP.align . PP.pretty)
                ( unwrapInlinedDatumStr . show $
                    output ^. outputDatumL
                )
          ]
        Pl.OutputDatumHash _datum ->
          [ "Datum (hashed):"
              <+> (PP.align . PP.pretty)
                ( unwrapHashedDatumStr . show $
                    output ^. outputDatumL
                )
          ]
        Pl.NoOutputDatum -> []
    )

prettyTxSkelIn :: Map Pl.TxOutRef Pl.TxOut -> Map Pl.DatumHash (Pl.Datum, String) -> (Pl.TxOutRef, TxSkelRedeemer) -> Maybe (Doc ann)
prettyTxSkelIn managedTxOuts managedDatums (txOutRef, txSkelRedeemer) = do
  output <- Map.lookup txOutRef managedTxOuts
  datumDoc <-
    case outputOutputDatum output of
      Pl.OutputDatum datum ->
        do
          (_, datumStr) <- Map.lookup (Pl.datumHash datum) managedDatums
          return $
            Just
              ( "Datum (inlined):"
                  <+> (PP.align . PP.pretty) (unwrapInlinedDatumStr datumStr)
              )
      Pl.OutputDatumHash datumHash ->
        do
          (_, datumStr) <- Map.lookup datumHash managedDatums
          return $
            Just
              ( "Datum (hashed):"
                  <+> (PP.align . PP.pretty) (unwrapHashedDatumStr datumStr)
              )
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

-- | Datum in transaction skeletons is of type 'TxSkelOutDatum'. We rely on the default 'Show' instance that will in turn rely on the 'Show' instance of the typed datum whose concrete type is unknown. This is a hacky way to get rid of the textual representation of the 'TxSkelOutDatum' constructor.
--
-- E.g. "TxSkelOutInlineDatum ("hello", 42) -> ("hello", 42)
unwrapInlinedDatumStr, unwrapHashedDatumStr :: String -> String
unwrapInlinedDatumStr = drop 21
unwrapHashedDatumStr = drop 19

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
  prettySingletons
    . map prettySingletonValue
    . filter (\(_, _, n) -> n /= 0)
    . Pl.flattenValue
  where
    prettySingletons :: [Doc ann] -> Doc ann
    prettySingletons [] = "Empty value"
    prettySingletons [doc] = doc
    prettySingletons docs = prettyEnum "Value:" "-" docs
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

-- | Pretty-print, if non empty, a list of transaction skeleton options that
-- have non default values. 'awaitRxConfirmed' and 'forceOutputOrdering'
-- (deprecated TODO) are never printed.
mPrettyTxOpts :: TxOpts -> Maybe (Doc ann)
mPrettyTxOpts
  TxOpts
    { adjustUnbalTx,
      autoSlotIncrease,
      unsafeModTx,
      balance,
      balanceOutputPolicy,
      balanceWallet
    } =
    prettyEnumNonEmpty "Options:" "-" $
      catMaybes
        [ prettyIfNot def prettyAdjustUnbalTx adjustUnbalTx,
          prettyIfNot True prettyAutoSlotIncrease autoSlotIncrease,
          prettyIfNot True prettyBalance balance,
          prettyIfNot def prettyBalanceOutputPolicy balanceOutputPolicy,
          prettyIfNot def prettyBalanceWallet balanceWallet,
          prettyIfNot [] prettyUnsafeModTx unsafeModTx
        ]
    where
      prettyIfNot :: Eq a => a -> (a -> Doc ann) -> a -> Maybe (Doc ann)
      prettyIfNot defaultValue f x
        | x == defaultValue = Nothing
        | otherwise = Just $ f x
      prettyAdjustUnbalTx :: Bool -> Doc ann
      prettyAdjustUnbalTx True = "AdjustUnbalTx (min Ada per transaction)"
      prettyAdjustUnbalTx False = "No AdjustUnbalTx"
      prettyAutoSlotIncrease :: Bool -> Doc ann
      prettyAutoSlotIncrease True = "Automatic slot increase"
      prettyAutoSlotIncrease False = "No automatic slot increase"
      prettyBalance :: Bool -> Doc ann
      prettyBalance True = "Automatic balancing"
      prettyBalance False = "No automatic balancing"
      prettyBalanceOutputPolicy :: BalanceOutputPolicy -> Doc ann
      prettyBalanceOutputPolicy AdjustExistingOutput = "Adjust existing outputs"
      prettyBalanceOutputPolicy DontAdjustExistingOutput = "Don't adjust existing outputs"
      prettyBalanceWallet :: BalancingWallet -> Doc ann
      prettyBalanceWallet BalanceWithFirstSigner = "Balance with first signer"
      prettyBalanceWallet (BalanceWith w) = "Balance with" <+> prettyPubKeyHash (walletPKHash w)
      prettyUnsafeModTx :: [RawModTx] -> Doc ann
      prettyUnsafeModTx [] = "No transaction modifications"
      prettyUnsafeModTx xs = PP.pretty (length xs) <+> "transaction modifications"

-- * Pretty-printing

-- | Pretty prints a 'UtxoState'.
prettyUtxoState :: UtxoState -> Doc ann
prettyUtxoState =
  prettyEnum "UTxO state:" "•"
    . map (uncurry prettyAddressState . second utxoValueSet)
    . Map.toList
    . utxoState

instance Show UtxoState where
  show = show . prettyUtxoState

-- | Pretty prints the state of an address, that is the list of utxos
-- (including value and datum), grouped
prettyAddressState :: Pl.Address -> [(Pl.Value, Maybe UtxoDatum)] -> Doc ann
prettyAddressState address payloads =
  prettyEnum
    (prettyAddress address)
    "-"
    ( mapMaybe prettyPayloadGrouped . List.group
        . List.sortBy (compare `on` (Ada.fromValue . fst))
        $ payloads
    )

-- | Pretty prints payloads (datum and value corresponding to 1 utxo) that have
-- been grouped together when they are the same
prettyPayloadGrouped :: [(Pl.Value, Maybe UtxoDatum)] -> Maybe (Doc ann)
prettyPayloadGrouped [] = Nothing
prettyPayloadGrouped [payload] = uncurry prettyPayload payload
prettyPayloadGrouped (payload : rest) =
  let cardinality = 1 + length rest
   in (PP.parens ("×" <> PP.pretty cardinality) <+>) <$> uncurry prettyPayload payload

prettyPayload :: Pl.Value -> Maybe UtxoDatum -> Maybe (Doc ann)
prettyPayload value mDatum =
  case catMaybes
    [ Just (prettyValue value),
      -- TODO Upgrade UtxoState to carry information about whether the datum
      -- is hashed or inlined
      ("Datum:" <+>) . PP.align . PP.pretty . utxoShow <$> mDatum
    ] of
    [] -> Nothing
    [doc] -> Just $ PP.align doc
    docs -> Just . PP.align . PP.vsep $ docs
