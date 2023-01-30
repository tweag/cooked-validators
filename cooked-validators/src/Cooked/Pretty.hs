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
module Cooked.Pretty where

import Control.Arrow (second)
import Cooked.Currencies (permanentCurrencySymbol, quickCurrencySymbol)
import Cooked.MockChain.Direct
import Cooked.MockChain.GenerateTx (GenerateTxError (..))
import Cooked.MockChain.Staged
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import Data.Function (on)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
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
import qualified Prettyprinter.Render.String as PP
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

prettyEnumerate :: DocCooked -> DocCooked -> [DocCooked] -> DocCooked
prettyEnumerate title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 . PP.vsep $
        zipWith (\index item -> PP.pretty index <> bullet <+> PP.align item) [1 :: Int ..] items
    ]

instance PrettyCooked MockChainError where
  prettyCookedOpt _ (MCEValidationError plutusError) =
    PP.vsep ["Validation error", PP.indent 2 (PP.pretty plutusError)]
  -- Here we don't print the skel because we lack its context and this error is
  -- printed alongside the skeleton when a test fails
  prettyCookedOpt _ (MCEUnbalanceable msg balanceStage _) =
    prettyEnum
      "Unbalanceable"
      "-"
      [PP.pretty msg, prettyBalanceStage balanceStage]
    where
      prettyBalanceStage BalCalcFee = "Fee calculation stage"
      prettyBalanceStage BalFinalizing = "Finalizing stage"
  prettyCookedOpt _ MCENoSuitableCollateral =
    "No suitable collateral"
  prettyCookedOpt _ (MCEGenerationError (ToCardanoError msg cardanoError)) =
    prettyEnum
      "Transaction generation error:"
      "-"
      [PP.pretty msg, PP.pretty cardanoError]
  prettyCookedOpt _ (MCEGenerationError (GenerateTxErrorGeneral msg)) =
    prettyEnum
      "Transaction generation error:"
      "-"
      [PP.pretty msg]
  prettyCookedOpt opts (MCECalcFee err) =
    PP.vsep ["Fee calculation error:", PP.indent 2 (prettyCookedOpt opts err)]
  prettyCookedOpt opts (MCEUnknownOutRefError msg txOutRef) =
    prettyEnum
      "Unknown transaction output ref:"
      "-"
      [PP.pretty msg, prettyCookedOpt opts txOutRef]
  prettyCookedOpt _ (FailWith msg) =
    "Failed with:" <+> PP.pretty msg

-- | Use this to convert a pretty-printer to a regular show function using
-- default layout options. This is used in "Testing" because Tasty uses
-- strings.
renderString :: (a -> DocCooked) -> a -> String
renderString printer = PP.renderString . PP.layoutPretty PP.defaultLayoutOptions . printer

prettyEndState :: Show a => PrettyCookedOpts -> (a, UtxoState) -> DocCooked
prettyEndState opts (res, state) =
  prettyEnum
    "End state:"
    "-"
    ["Returned value:" <+> PP.viaShow res, prettyUtxoState opts state]

-- | This pretty prints a mock chain log that usually consists of the list of
-- validated or submitted transactions. In the log, we know a transaction has
-- been validated if the 'MCLogSubmittedTxSkel' is followed by a 'MCLogNewTx'.
prettyMockChainLog :: PrettyCookedOpts -> MockChainLog -> DocCooked
prettyMockChainLog opts =
  prettyEnumerate "MockChain run:" "."
    . go []
  where
    -- In order to avoid printing 'MockChainLogValidateTxSkel' then
    -- 'MockChainLogNewTx' as two different items, we combine them into one
    -- single 'DocCooked'
    go :: [DocCooked] -> [MockChainLogEntry] -> [DocCooked]
    go
      acc
      ( MCLogSubmittedTxSkel skelContext skel
          : MCLogNewTx txId
          : entries
        )
        | pcOptPrintTxHashes opts =
          go
            ( "Validated"
                <+> PP.parens ("TxId:" <+> prettyCookedOpt opts txId)
                <+> prettyTxSkel opts skelContext skel :
              acc
            )
            entries
        | otherwise = go ("Validated" <+> prettyTxSkel opts skelContext skel : acc) entries
    go
      acc
      ( MCLogSubmittedTxSkel skelContext skel
          : entries
        ) =
        go ("Submitted" <+> prettyTxSkel opts skelContext skel : acc) entries
    go acc (MCLogFail msg : entries) =
      go ("Fail:" <+> PP.pretty msg : acc) entries
    -- This case is not supposed to occur because it should follow a
    -- 'MCLogSubmittedTxSkel'
    go acc (MCLogNewTx txId : entries) =
      go ("New transaction:" <+> prettyCookedOpt opts txId : acc) entries
    go acc [] = reverse acc

prettyTxSkel :: PrettyCookedOpts -> SkelContext -> TxSkel -> DocCooked
prettyTxSkel opts skelContext (TxSkel lbl txopts mints validityRange signers ins insReference outs) =
  prettyEnum
    "transaction skeleton:"
    "-"
    ( catMaybes
        [ prettyEnumNonEmpty "Labels:" "-" (PP.viaShow <$> Set.toList lbl),
          mPrettyTxOpts opts txopts,
          prettyEnumNonEmpty "Mints:" "-" (prettyMints opts <$> (mints ^. mintsListIso)),
          Just $ "Validity interval:" <+> PP.pretty validityRange,
          prettyEnumNonEmpty "Signers:" "-" (prettySigners opts txopts signers),
          -- TODO handle unsafe 'fromJust' better
          prettyEnumNonEmpty "Inputs:" "-" (mapMaybe (prettyTxSkelIn opts skelContext) $ Map.toList ins),
          prettyEnumNonEmpty "Reference inputs:" "-" (mapMaybe (prettyTxSkelInReference opts skelContext) $ Set.toList insReference),
          prettyEnumNonEmpty "Outputs:" "-" (prettyTxSkelOut opts <$> outs)
        ]
    )

instance PrettyCooked Pl.PubKeyHash where
  -- If the pubkey is a known wallet
  -- #abcdef (wallet 3)
  --
  -- Otherwise
  -- #123456
  --
  prettyCookedOpt opts pkh =
    case walletPKHashToId pkh of
      Nothing -> prettyHash (pcOptPrintedHashLength opts) pkh
      Just walletId ->
        prettyHash (pcOptPrintedHashLength opts) pkh
          <+> PP.parens ("wallet" <+> PP.viaShow walletId)

-- | Same as 'prettyPubKeyHash' with a suffix mentionning this is the balancing
-- wallet
prettyBalancingWallet :: PrettyCookedOpts -> Wallet -> DocCooked
prettyBalancingWallet opts w =
  prettyCookedOpt opts (walletPKHash w) <+> "[Balancing]"

-- | Prints a list of pubkeys with a flag next to the balancing wallet
prettySigners :: PrettyCookedOpts -> TxOpts -> NEList.NonEmpty Wallet -> [DocCooked]
prettySigners opts TxOpts {txOptBalanceWallet = BalanceWithFirstSigner} (firstSigner NEList.:| signers) =
  prettyBalancingWallet opts firstSigner : (prettyCookedOpt opts . walletPKHash <$> signers)
prettySigners opts TxOpts {txOptBalanceWallet = BalanceWith balancingWallet} signers =
  aux (NEList.toList signers)
  where
    aux :: [Wallet] -> [DocCooked]
    aux [] = []
    aux (s : ss)
      | s == balancingWallet = prettyBalancingWallet opts balancingWallet : aux ss
      | otherwise = prettyCookedOpt opts (walletPKHash s) : aux ss

-- prettyMints
--
-- Examples without and with redeemer
-- #abcdef "Foo" -> 500
-- #123456 "Bar" | Redeemer -> 1000
prettyMints :: PrettyCookedOpts -> (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) -> DocCooked
prettyMints opts (Pl.Versioned policy _, NoMintsRedeemer, tokenName, NonZero amount) =
  prettyCookedOpt opts policy
    <+> PP.viaShow tokenName
    <+> "->"
    <+> PP.viaShow amount
prettyMints opts (Pl.Versioned policy _, SomeMintsRedeemer redeemer, tokenName, NonZero amount) =
  prettyCookedOpt opts policy
    <+> PP.viaShow tokenName
    <+> "|"
    <+> prettyCookedOpt opts redeemer
    <+> "->"
    <+> PP.viaShow amount

instance PrettyCooked Pl.Address where
  prettyCookedOpt opts (Pl.Address addrCr Nothing) = prettyCookedOpt opts addrCr
  prettyCookedOpt opts (Pl.Address addrCr (Just (Pl.StakingHash stakCr))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> prettyCookedOpt opts stakCr)
  prettyCookedOpt opts (Pl.Address addrCr (Just (Pl.StakingPtr p1 p2 p3))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> PP.pretty (p1, p2, p3))

instance PrettyCooked Pl.Credential where
  prettyCookedOpt opts (Pl.ScriptCredential vh) = "script" <+> prettyHash (pcOptPrintedHashLength opts) vh
  prettyCookedOpt opts (Pl.PubKeyCredential pkh) = "pubkey" <+> prettyCookedOpt opts pkh

prettyTxSkelOut :: PrettyCookedOpts -> TxSkelOut -> DocCooked
prettyTxSkelOut opts (Pays output) =
  prettyEnum
    ("Pays to" <+> prettyCookedOpt opts (outputAddress output))
    "-"
    ( prettyCookedOpt opts (outputValue output) :
      catMaybes
        [ case outputOutputDatum output of
            Pl.OutputDatum _datum ->
              Just $
                "Datum (inlined):"
                  <+> (PP.align . prettyCookedOpt opts)
                    (output ^. outputDatumL)
            Pl.OutputDatumHash _datum ->
              Just $
                "Datum (hashed):"
                  <+> (PP.align . prettyCookedOpt opts)
                    (output ^. outputDatumL)
            Pl.NoOutputDatum -> Nothing,
          getReferenceScriptDoc opts output
        ]
    )

prettyTxSkelIn :: PrettyCookedOpts -> SkelContext -> (Pl.TxOutRef, TxSkelRedeemer) -> Maybe (DocCooked)
prettyTxSkelIn opts skelContext (txOutRef, txSkelRedeemer) = do
  (output, datumDoc) <- lookupOutputWithDatumDoc skelContext txOutRef
  let redeemerDoc =
        case txSkelRedeemer of
          TxSkelRedeemerForScript redeemer -> Just ("Redeemer:" <+> prettyCookedOpt opts redeemer)
          _ -> Nothing
  return $
    prettyEnum
      ("Spends from" <+> prettyCookedOpt opts (outputAddress output))
      "-"
      (prettyCookedOpt opts (outputValue output) : catMaybes [redeemerDoc, datumDoc, getReferenceScriptDoc opts output])

prettyTxSkelInReference :: PrettyCookedOpts -> SkelContext -> Pl.TxOutRef -> Maybe DocCooked
prettyTxSkelInReference opts skelContext txOutRef = do
  (output, datumDoc) <- lookupOutputWithDatumDoc skelContext txOutRef
  return $
    prettyEnum
      ("References output from" <+> prettyCookedOpt opts (outputAddress output))
      "-"
      (prettyCookedOpt opts (outputValue output) : catMaybes [datumDoc, getReferenceScriptDoc opts output])

getReferenceScriptDoc :: (IsAbstractOutput output, ToScriptHash (ReferenceScriptType output)) => PrettyCookedOpts -> output -> Maybe DocCooked
getReferenceScriptDoc opts output =
  case output ^. outputReferenceScriptL of
    Nothing -> Nothing
    Just refScript -> Just $ "Reference script hash:" <+> prettyHash (pcOptPrintedHashLength opts) (toScriptHash refScript)

lookupOutputWithDatumDoc ::
  SkelContext ->
  Pl.TxOutRef ->
  Maybe (Pl.TxOut, Maybe DocCooked)
lookupOutputWithDatumDoc (SkelContext managedTxOuts managedDatums) txOutRef = do
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
prettyHash :: (Show a) => Int -> a -> DocCooked
prettyHash printedLength = PP.pretty . ('#' :) . take printedLength . show

instance PrettyCooked Pl.TxId where
  prettyCookedOpt opts = prettyHash (pcOptPrintedHashLength opts)

instance PrettyCooked Pl.TxOutRef where
  prettyCookedOpt opts (Pl.TxOutRef txId index) =
    prettyHash (pcOptPrintedHashLength opts) txId <> "!" <> PP.pretty index

instance PrettyCooked Pl.MintingPolicy where
  prettyCookedOpt opts = prettyHash (pcOptPrintedHashLength opts) . Pl.mintingPolicyHash

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
  prettyCookedOpt opts =
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
            | otherwise = prettyHash (pcOptPrintedHashLength opts) symbol <+> PP.pretty name

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
mPrettyTxOpts :: PrettyCookedOpts -> TxOpts -> Maybe DocCooked
mPrettyTxOpts
  opts
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
      prettyIfNot :: Eq a => a -> (a -> DocCooked) -> a -> Maybe DocCooked
      prettyIfNot defaultValue f x
        | x == defaultValue && not (pcOptPrintDefaultTxOpts opts) = Nothing
        | otherwise = Just $ f x
      prettyEnsureMinAda :: Bool -> DocCooked
      prettyEnsureMinAda True = "Ensure min Ada per transaction"
      prettyEnsureMinAda False = "Do not ensure min Ada per transaction"
      prettyAutoSlotIncrease :: Bool -> DocCooked
      prettyAutoSlotIncrease True = "Automatic slot increase"
      prettyAutoSlotIncrease False = "No automatic slot increase"
      prettyBalance :: Bool -> DocCooked
      prettyBalance True = "Automatic balancing"
      prettyBalance False = "No automatic balancing"
      prettyBalanceOutputPolicy :: BalanceOutputPolicy -> DocCooked
      prettyBalanceOutputPolicy AdjustExistingOutput = "Balance policy: Adjust existing outputs"
      prettyBalanceOutputPolicy DontAdjustExistingOutput = "Balance policy: Don't adjust existing outputs"
      prettyBalanceWallet :: BalancingWallet -> DocCooked
      prettyBalanceWallet BalanceWithFirstSigner = "Balance with first signer"
      prettyBalanceWallet (BalanceWith w) = "Balance with" <+> prettyCookedOpt opts (walletPKHash w)
      prettyUnsafeModTx :: [RawModTx] -> DocCooked
      prettyUnsafeModTx [] = "No transaction modifications"
      prettyUnsafeModTx xs =
        let n = length xs
         in PP.pretty n
              <+> "transaction"
              <+> PP.plural "modification" "modifications" n

-- * Pretty-printing

-- | Pretty prints a 'UtxoState'. Print the known wallets first, then unknown
-- pks, then scripts.
prettyUtxoState :: PrettyCookedOpts -> UtxoState -> DocCooked
prettyUtxoState opts =
  prettyEnum "UTxO state:" "•"
    . map (uncurry (prettyAddressState opts) . second utxoValueSet)
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

-- TODO find a way to use options
instance Show UtxoState where
  show = show . (prettyUtxoState def)

-- | Pretty prints the state of an address, that is the list of utxos
-- (including value and datum), grouped
prettyAddressState :: PrettyCookedOpts -> Pl.Address -> [(Pl.Value, Maybe UtxoDatum)] -> DocCooked
prettyAddressState opts address payloads =
  prettyEnum
    (prettyCookedOpt opts address)
    "-"
    ( mapMaybe (prettyPayloadGrouped opts) . List.group
        . List.sortBy (compare `on` (Ada.fromValue . fst))
        $ payloads
    )

-- | Pretty prints payloads (datum and value corresponding to 1 utxo) that have
-- been grouped together when they belong to the same utxo
prettyPayloadGrouped :: PrettyCookedOpts -> [(Pl.Value, Maybe UtxoDatum)] -> Maybe (DocCooked)
prettyPayloadGrouped _ [] = Nothing
prettyPayloadGrouped opts [payload] = uncurry (prettyPayload opts) payload
prettyPayloadGrouped opts (payload : rest) =
  let cardinality = 1 + length rest
   in (PP.parens ("×" <> PP.pretty cardinality) <+>) <$> uncurry (prettyPayload opts) payload

prettyPayload :: PrettyCookedOpts -> Pl.Value -> Maybe UtxoDatum -> Maybe (DocCooked)
prettyPayload opts value mDatum =
  case catMaybes
    [ Just (prettyCookedOpt opts value),
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
