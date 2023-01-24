{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module centralizes functions to pretty-print transaction skeletons,
-- utxo states, addresses, pubkey hashes, values, etc.
--
-- We provide the 'PrettyCooked' class and instances for common Plutus types.
-- We don't rely on 'Pretty' from "Prettyprinter" in order to define better
-- printers for Plutus types which already have instances of 'Pretty'. Also,
-- 'PrettyCooked' makes it possible to optionally modify pretty printing
-- settings 'PrettyCookedOpts' (e.g. length of printed hashes).
--
-- == Requirements on datum and redeemers
--
-- Datums and redeemers are required to have a 'PrettyCooked' instance.
-- 
-- For trivial datatypes, you can rely on Show by using 'viaShow' from
-- "Prettyprinter": 'prettyCooked = Prettyprinter.viaShow'.
--
-- For more complex datatypes, you can rely on existing 'PrettyCooked'
-- instances. Prefer implementing the 'prettyCookedOpt' function and relay the
-- 'PrettyCookedOpts' settings to other printers.
--
-- @
--     data Foo = Bar Pl.Value | Baz Pl.PubkeyHash Pl.Value
--
--     instance PrettyCooked Foo where
--       prettyCookedOpt pcOpts (Bar value) =
--         "Bar" <+> prettyCookedOpt pcOpts value
--       prettyCookedOpt pcOpts (Baz pkh value) =
--         prettyEnum
--           "Baz"
--           "-"
--           [ "user:" <+> prettyCookedOpt pcOpts pkh,
--             "deposit:" <+> prettyCookedOpt pcOpts value ]
-- @
--
-- The 'prettyEnum' function is useful to nicely lay down nested lists of
-- elements. Since we manipulate regular 'Doc' values, any function from
-- "Prettyprinter" can be used to implement your printers.
--
-- == How to pretty print?
--
-- Pretty printing of transaction skeletons and UTxO states is done
-- automatically by the end-user functions provided in
-- "Cooked.MockChain.Testing".
--
-- To do it manually, use 'prettyTxSkel' or 'prettyUtxoState'.
--
module Cooked.Pretty where

import Control.Arrow (second)
import Cooked.Currencies (permanentCurrencySymbol, quickCurrencySymbol)
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import Data.Function (on)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (TxOut, mintingPolicyHash, unspentOutputs, validatorHash)
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V2.Scripts as Pl (mintingPolicyHash)
import qualified Plutus.V2.Ledger.Api as Pl
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Test.QuickCheck (NonZero)
import Test.Tasty.QuickCheck (NonZero (..))

-- prettyEnum "Foo" "-" ["bar1", "bar2", "bar3"]
--    Foo
--      - bar1
--      - bar2
--      - bar3
prettyEnum :: DocCooked -> DocCooked -> [DocCooked] -> DocCooked
prettyEnum title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 . PP.vsep $
        map (bullet <+>) items
    ]

prettyEnumNonEmpty :: DocCooked -> DocCooked -> [DocCooked] -> Maybe (DocCooked)
prettyEnumNonEmpty _ _ [] = Nothing
prettyEnumNonEmpty title bullet items = Just $ prettyEnum title bullet items

prettyTxSkel :: Map Pl.TxOutRef Pl.TxOut -> Map Pl.DatumHash (Pl.Datum, DocCooked) -> TxSkel -> DocCooked
prettyTxSkel managedTxOuts managedDatums (TxSkel lbl opts mints validityRange signers ins insReference outs) =
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
          prettyEnumNonEmpty "Inputs:" "-" (mapMaybe (prettyTxSkelIn managedTxOuts managedDatums) $ Map.toList ins),
          prettyEnumNonEmpty "Reference inputs:" "-" (mapMaybe (prettyTxSkelInReference managedTxOuts managedDatums) $ Set.toList insReference),
          prettyEnumNonEmpty "Outputs:" "-" (prettyTxSkelOut <$> outs)
        ]
    )

instance PrettyCooked Pl.PubKeyHash where
  -- If the pubkey is a known wallet
  -- #abcdef (wallet 3)
  --
  -- Otherwise
  -- #123456
  --
  prettyCooked pkh =
    case walletPKHashToId pkh of
      Nothing -> prettyHash pkh
      Just walletId ->
        prettyHash pkh
          <+> PP.parens ("wallet" <+> PP.viaShow walletId)

-- | Same as 'prettyPubKeyHash' with a suffix mentionning this is the balancing
-- wallet
prettyBalancingWallet :: Wallet -> DocCooked
prettyBalancingWallet w =
  prettyCooked (walletPKHash w) <+> "[Balancing]"

-- | Prints a list of pubkeys with a flag next to the balancing wallet
prettySigners :: TxOpts -> NEList.NonEmpty Wallet -> [DocCooked]
prettySigners TxOpts {txOptBalanceWallet = BalanceWithFirstSigner} (firstSigner NEList.:| signers) =
  prettyBalancingWallet firstSigner : (prettyCooked . walletPKHash <$> signers)
prettySigners TxOpts {txOptBalanceWallet = BalanceWith balancingWallet} signers =
  aux (NEList.toList signers)
  where
    aux :: [Wallet] -> [DocCooked]
    aux [] = []
    aux (s : ss)
      | s == balancingWallet = prettyBalancingWallet balancingWallet : aux ss
      | otherwise = prettyCooked (walletPKHash s) : aux ss

-- prettyMints
--
-- Examples without and with redeemer
-- #abcdef "Foo" -> 500
-- #123456 "Bar" | Redeemer -> 1000
prettyMints :: (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) -> DocCooked
prettyMints (Pl.Versioned policy _, NoMintsRedeemer, tokenName, NonZero amount) =
  prettyCooked policy
    <+> PP.viaShow tokenName
    <+> "->"
    <+> PP.viaShow amount
prettyMints (Pl.Versioned policy _, SomeMintsRedeemer redeemer, tokenName, NonZero amount) =
  prettyCooked policy
    <+> PP.viaShow tokenName
    <+> "|"
    <+> prettyCooked redeemer
    <+> "->"
    <+> PP.viaShow amount

instance PrettyCooked Pl.Address where
  prettyCooked (Pl.Address addrCr Nothing) = prettyCooked addrCr
  prettyCooked (Pl.Address addrCr (Just (Pl.StakingHash stakCr))) =
    prettyCooked addrCr <+> PP.angles ("staking:" <+> prettyCooked stakCr)
  prettyCooked (Pl.Address addrCr (Just (Pl.StakingPtr p1 p2 p3))) =
    prettyCooked addrCr <+> PP.angles ("staking:" <+> PP.pretty (p1, p2, p3))

instance PrettyCooked Pl.Credential where
  prettyCooked (Pl.ScriptCredential vh) = "script" <+> prettyHash vh
  prettyCooked (Pl.PubKeyCredential pkh) = "pubkey" <+> prettyCooked pkh

prettyTxSkelOut :: TxSkelOut -> DocCooked
prettyTxSkelOut (Pays output) =
  prettyEnum
    ("Pays to" <+> prettyCooked (outputAddress output))
    "-"
    ( prettyCooked (outputValue output) :
      catMaybes
        [ case outputOutputDatum output of
            Pl.OutputDatum _datum ->
              Just $
                "Datum (inlined):"
                  <+> (PP.align . prettyCooked)
                    (output ^. outputDatumL)
            Pl.OutputDatumHash _datum ->
              Just $
                "Datum (hashed):"
                  <+> (PP.align . prettyCooked)
                    (output ^. outputDatumL)
            Pl.NoOutputDatum -> Nothing,
          getReferenceScriptDoc output
        ]
    )

prettyTxSkelIn :: Map Pl.TxOutRef Pl.TxOut -> Map Pl.DatumHash (Pl.Datum, DocCooked) -> (Pl.TxOutRef, TxSkelRedeemer) -> Maybe (DocCooked)
prettyTxSkelIn managedTxOuts managedDatums (txOutRef, txSkelRedeemer) = do
  (output, datumDoc) <- lookupOutputWithDatumDoc managedTxOuts managedDatums txOutRef
  let redeemerDoc =
        case txSkelRedeemer of
          TxSkelRedeemerForScript redeemer -> Just ("Redeemer:" <+> prettyCooked redeemer)
          _ -> Nothing
  return $
    prettyEnum
      ("Spends from" <+> prettyCooked (outputAddress output))
      "-"
      (prettyCooked (outputValue output) : catMaybes [redeemerDoc, datumDoc, getReferenceScriptDoc output])

prettyTxSkelInReference :: Map Pl.TxOutRef Pl.TxOut -> Map Pl.DatumHash (Pl.Datum, DocCooked) -> Pl.TxOutRef -> Maybe DocCooked
prettyTxSkelInReference managedTxOuts managedDatums txOutRef = do
  (output, datumDoc) <- lookupOutputWithDatumDoc managedTxOuts managedDatums txOutRef

  return $
    prettyEnum
      ("References output from" <+> prettyCooked (outputAddress output))
      "-"
      (prettyCooked (outputValue output) : catMaybes [datumDoc, getReferenceScriptDoc output])

getReferenceScriptDoc :: (IsAbstractOutput output, ToScriptHash (ReferenceScriptType output)) => output -> Maybe DocCooked
getReferenceScriptDoc output =
  case output ^. outputReferenceScriptL of
    Nothing -> Nothing
    Just refScript -> Just $ "Reference script hash:" <+> prettyHash (toScriptHash refScript)

lookupOutputWithDatumDoc ::
  Map Pl.TxOutRef Pl.TxOut ->
  Map Pl.DatumHash (Pl.Datum, DocCooked) ->
  Pl.TxOutRef ->
  Maybe (Pl.TxOut, Maybe DocCooked)
lookupOutputWithDatumDoc managedTxOuts managedDatums txOutRef = do
  output <- Map.lookup txOutRef managedTxOuts
  datumDoc <-
    case outputOutputDatum output of
      Pl.OutputDatum datum ->
        do
          (_, datumDoc) <- Map.lookup (Pl.datumHash datum) managedDatums
          return $ Just ("Datum (inlined):" <+> PP.align datumDoc)
      Pl.OutputDatumHash datumHash ->
        do
          (_, datumDoc) <- Map.lookup datumHash managedDatums
          return $ Just ("Datum (hashed):" <+> PP.align datumDoc)
      Pl.NoOutputDatum -> return Nothing
  return (output, datumDoc)

-- prettyHash 28a3d93cc3daac
-- #28a3d9
prettyHash :: (Show a) => a -> DocCooked
prettyHash = PP.pretty . ('#' :) . take 7 . show

instance PrettyCooked Pl.TxId where
  prettyCooked = prettyHash

instance PrettyCooked Pl.TxOutRef where
  prettyCooked (Pl.TxOutRef txId index) = prettyHash txId <> "!" <> PP.pretty index

instance PrettyCooked Pl.MintingPolicy where
  prettyCooked = prettyHash . Pl.mintingPolicyHash

instance PrettyCooked Pl.Value where
  -- prettyValue example output:
  --
  -- Value:
  --   - Lovelace: 45_000_000
  --   - Quick "hello": 3
  --   - #12bc3d "usertoken": 1
  --
  -- In case of an empty value (even though not an empty map):
  -- Empty value
  --
  prettyCooked =
    prettySingletons
      . map prettySingletonValue
      . filter (\(_, _, n) -> n /= 0)
      . Pl.flattenValue
    where
      prettySingletons :: [DocCooked] -> DocCooked
      prettySingletons [] = "Empty value"
      prettySingletons [doc] = doc
      prettySingletons docs = prettyEnum "Value:" "-" docs
      prettySingletonValue :: (Pl.CurrencySymbol, Pl.TokenName, Integer) -> DocCooked
      prettySingletonValue (symbol, name, amount) =
        prettyAssetClass <> ":" <+> prettyNumericUnderscore amount
        where
          prettyAssetClass
            | symbol == Pl.CurrencySymbol "" = "Lovelace"
            | symbol == quickCurrencySymbol = "Quick" <+> PP.pretty name
            | symbol == permanentCurrencySymbol = "Permanent" <+> PP.pretty name
            | otherwise = prettyHash symbol <+> PP.pretty name

      -- prettyNumericUnderscore 23798423723
      -- 23_798_423_723
      prettyNumericUnderscore :: Integer -> DocCooked
      prettyNumericUnderscore i
        | 0 == i = "0"
        | i > 0 = psnTerm "" 0 i
        | otherwise = "-" <> psnTerm "" 0 (- i)
        where
          psnTerm :: DocCooked -> Integer -> Integer -> DocCooked
          psnTerm acc _ 0 = acc
          psnTerm acc 3 nb = psnTerm (PP.pretty (nb `mod` 10) <> "_" <> acc) 1 (nb `div` 10)
          psnTerm acc n nb = psnTerm (PP.pretty (nb `mod` 10) <> acc) (n + 1) (nb `div` 10)

-- | Pretty-print a list of transaction skeleton options, only printing an option if its value is non-default.
-- If no non-default options are in the list, return nothing.
mPrettyTxOpts :: TxOpts -> Maybe (DocCooked)
mPrettyTxOpts
  TxOpts
    { txOptEnsureMinAda,
      txOptAutoSlotIncrease,
      txOptUnsafeModTx,
      txOptBalance,
      txOptBalanceOutputPolicy,
      txOptBalanceWallet
    } =
    prettyEnumNonEmpty "Options:" "-" $
      catMaybes
        [ prettyIfNot def prettyEnsureMinAda txOptEnsureMinAda,
          prettyIfNot True prettyAutoSlotIncrease txOptAutoSlotIncrease,
          prettyIfNot True prettyBalance txOptBalance,
          prettyIfNot def prettyBalanceOutputPolicy txOptBalanceOutputPolicy,
          prettyIfNot def prettyBalanceWallet txOptBalanceWallet,
          prettyIfNot [] prettyUnsafeModTx txOptUnsafeModTx
        ]
    where
      prettyIfNot :: Eq a => a -> (a -> DocCooked) -> a -> Maybe (DocCooked)
      prettyIfNot defaultValue f x
        | x == defaultValue = Nothing
        | otherwise = Just $ f x
      prettyEnsureMinAda :: Bool -> DocCooked
      prettyEnsureMinAda True = "Adjust to ensure min Ada per transaction"
      prettyEnsureMinAda False = "Don't adjust to ensure min Ada per transaction"
      prettyAutoSlotIncrease :: Bool -> DocCooked
      prettyAutoSlotIncrease True = "Automatic slot increase"
      prettyAutoSlotIncrease False = "No automatic slot increase"
      prettyBalance :: Bool -> DocCooked
      prettyBalance True = "Automatic balancing"
      prettyBalance False = "No automatic balancing"
      prettyBalanceOutputPolicy :: BalanceOutputPolicy -> DocCooked
      prettyBalanceOutputPolicy AdjustExistingOutput = "Adjust existing outputs"
      prettyBalanceOutputPolicy DontAdjustExistingOutput = "Don't adjust existing outputs"
      prettyBalanceWallet :: BalancingWallet -> DocCooked
      prettyBalanceWallet BalanceWithFirstSigner = "Balance with first signer"
      prettyBalanceWallet (BalanceWith w) = "Balance with" <+> prettyCooked (walletPKHash w)
      prettyUnsafeModTx :: [RawModTx] -> DocCooked
      prettyUnsafeModTx [] = "No transaction modifications"
      prettyUnsafeModTx xs = PP.pretty (length xs) <+> "transaction modifications"

-- * Pretty-printing

-- | Pretty prints a 'UtxoState'. Print the known wallets first, then unknown
-- pks, then scripts.
prettyUtxoState :: UtxoState -> DocCooked
prettyUtxoState =
  prettyEnum "UTxO state:" "•"
    . map (uncurry prettyAddressState . second utxoValueSet)
    . List.sortBy addressOrdering
    . Map.toList
    . utxoState
  where
    addressOrdering :: (Pl.Address, a) -> (Pl.Address, a) -> Ordering
    addressOrdering
      (a1@(Pl.Address (Pl.PubKeyCredential pkh1) _), _)
      (a2@(Pl.Address (Pl.PubKeyCredential pkh2) _), _) =
        case (walletPKHashToId pkh1, walletPKHashToId pkh2) of
          (Just i, Just j) -> compare i j
          (Just _, Nothing) -> LT
          (Nothing, Just _) -> GT
          (Nothing, Nothing) -> compare a1 a2
    addressOrdering
      (Pl.Address (Pl.PubKeyCredential _) _, _)
      (Pl.Address (Pl.ScriptCredential _) _, _) = LT
    addressOrdering (a1, _) (a2, _) = compare a1 a2

instance Show UtxoState where
  show = show . prettyUtxoState

-- | Pretty prints the state of an address, that is the list of utxos
-- (including value and datum), grouped
prettyAddressState :: Pl.Address -> [(Pl.Value, Maybe UtxoDatum)] -> DocCooked
prettyAddressState address payloads =
  prettyEnum
    (prettyCooked address)
    "-"
    ( mapMaybe prettyPayloadGrouped . List.group
        . List.sortBy (compare `on` (Ada.fromValue . fst))
        $ payloads
    )

-- | Pretty prints payloads (datum and value corresponding to 1 utxo) that have
-- been grouped together when they belong to the same utxo
prettyPayloadGrouped :: [(Pl.Value, Maybe UtxoDatum)] -> Maybe (DocCooked)
prettyPayloadGrouped [] = Nothing
prettyPayloadGrouped [payload] = uncurry prettyPayload payload
prettyPayloadGrouped (payload : rest) =
  let cardinality = 1 + length rest
   in (PP.parens ("×" <> PP.pretty cardinality) <+>) <$> uncurry prettyPayload payload

prettyPayload :: Pl.Value -> Maybe UtxoDatum -> Maybe (DocCooked)
prettyPayload value mDatum =
  case catMaybes
    [ Just (prettyCooked value),
      prettyPayloadDatum <$> mDatum
    ] of
    [] -> Nothing
    [doc] -> Just $ PP.align doc
    docs -> Just . PP.align . PP.vsep $ docs
  where
    prettyPayloadDatum :: UtxoDatum -> DocCooked
    prettyPayloadDatum d =
      "Datum"
        <+> PP.parens (if utxoInlined d then "inlined" else "hashed")
        <> ":"
        <+> PP.align (utxoDoc d)
