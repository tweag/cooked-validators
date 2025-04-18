{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module implements 'PrettyCooked', 'PrettyCookedList' and
-- 'PrettyCookedMaybe' instances for data types returned by a @MockChain@ run.
module Cooked.Pretty.MockChain () where

import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Cooked.MockChain.UtxoState
import Cooked.Pretty.Class
import Cooked.Pretty.Options
import Cooked.Pretty.Skeleton
import Cooked.Wallet
import Data.Function (on)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

instance PrettyCooked MockChainError where
  prettyCookedOpt opts (MCEValidationError plutusPhase plutusError) =
    PP.vsep ["Validation error " <+> prettyCookedOpt opts plutusPhase, PP.indent 2 (prettyCookedOpt opts plutusError)]
  -- Here we don't print the skel because we lack its context and this error is
  -- printed alongside the skeleton when a test fails
  prettyCookedOpt opts (MCEUnbalanceable balWallet missingValue _) =
    prettyItemize
      opts
      "Unbalanceable:"
      "-"
      [ prettyCookedOpt opts balWallet <+> "does not have enough funds",
        if missingValue == mempty
          then "Not enough funds to sustain the minimal ada of the return utxo"
          else "Unable to find" <+> prettyCookedOpt opts missingValue
      ]
  prettyCookedOpt opts (MCENoSuitableCollateral fee percentage colVal) =
    prettyItemize
      opts
      "No suitable collateral"
      "-"
      [ "Fee was" <+> prettyCookedOpt opts fee,
        "Percentage in params was" <+> prettyCookedOpt opts percentage,
        "Resulting minimal collateral value was" <+> prettyCookedOpt opts colVal
      ]
  prettyCookedOpt opts (MCEGenerationError (ToCardanoError msg cardanoError)) =
    prettyItemize @[DocCooked]
      opts
      "Transaction generation error:"
      "-"
      [PP.pretty msg, PP.pretty cardanoError]
  prettyCookedOpt opts (MCEGenerationError (GenerateTxErrorGeneral msgs)) =
    prettyItemize @[DocCooked] opts "Transaction generation error:" "-" [PP.pretty msgs]
  prettyCookedOpt opts (MCEGenerationError (TxBodyError msg err)) =
    prettyItemize @[DocCooked] opts "Transaction generation error:" "-" [PP.pretty msg, PP.viaShow err]
  prettyCookedOpt opts (MCEUnknownOutRefError msg txOutRef) =
    prettyItemize opts "Unknown transaction output ref:" "-" [PP.pretty msg, prettyCookedOpt opts txOutRef]
  prettyCookedOpt _ (FailWith msg) = "Failed with:" <+> PP.pretty msg
  prettyCookedOpt opts (MCEUnknownValidator msg valHash) =
    prettyItemize
      opts
      "Unknown validator hash:"
      "-"
      [PP.pretty msg, "hash:" <+> prettyHash opts valHash]
  prettyCookedOpt opts (MCEUnknownDatum msg dHash) =
    prettyItemize
      opts
      "Unknown datum hash:"
      "-"
      [PP.pretty msg, "hash:" <+> prettyHash opts dHash]

instance (Show a) => PrettyCooked (a, UtxoState) where
  prettyCookedOpt opts (res, state) =
    prettyItemize
      opts
      "End state:"
      "-"
      ["Returns:" <+> PP.viaShow res, prettyCookedOpt opts state]

instance (Show a) => PrettyCooked (MockChainReturn a UtxoState) where
  prettyCookedOpt opts' (res, MockChainBook entries ((`addHashNames` opts') -> opts)) =
    let mcEndResult = case res of
          Left err -> "🔴" <+> prettyCookedOpt opts err
          Right (a, s) -> "🟢" <+> prettyCookedOpt opts (a, s)
     in PP.vsep $ if pcOptPrintLog opts then [prettyCookedOpt opts entries, mcEndResult] else [mcEndResult]

instance PrettyCooked [MockChainLogEntry] where
  prettyCookedOpt opts = ("📘" <+>) . prettyItemize opts "MockChain run log:" "⁍"

-- | This prints a 'MockChainLogEntry'. In the log, we know a transaction has
-- been validated if the 'MCLogSubmittedTxSkel' is followed by a 'MCLogNewTx'.
instance PrettyCooked MockChainLogEntry where
  prettyCookedOpt opts (MCLogAdjustedTxSkelOut skelOut newAda) =
    "The ADA amount of "
      <> prettyCookedOpt opts skelOut
      <> " has been automatically adjusted to "
      <> prettyCookedOpt opts (Script.toValue newAda)
  prettyCookedOpt opts (MCLogSubmittedTxSkel outputs datums skel) = prettyItemize opts "Submitted skeleton:" "-" $ Contextualized outputs datums skel
  prettyCookedOpt opts (MCLogAdjustedTxSkel outputs datums skel fee mCollaterals) =
    let mCollateralsDoc =
          ( \(collaterals, returnWallet) ->
              [ prettyItemize opts "Collateral inputs:" "-" (Contextualized outputs datums . CollateralInput <$> Set.toList collaterals),
                "Return collateral target:" <+> prettyCookedOpt opts returnWallet
              ]
          )
            <$> mCollaterals
     in prettyItemize opts "Adjusted skeleton:" "-" $
          prettyCookedOptList opts (Contextualized outputs datums skel)
            ++ (("Fee:" <+> prettyCookedOpt opts (Script.lovelace fee)) : fromMaybe [] mCollateralsDoc)
  prettyCookedOpt opts (MCLogNewTx txId) = "New transaction:" <+> prettyHash opts txId
  prettyCookedOpt opts (MCLogDiscardedUtxos n s) = prettyCookedOpt opts n <+> "balancing utxos were discarded:" <+> PP.pretty s
  prettyCookedOpt opts (MCLogUnusedCollaterals (Left cWallet)) =
    "Specific request to fetch collateral utxos from "
      <> prettyCookedOpt opts cWallet
      <> " has been disregarded because the transaction does not require collaterals"
  prettyCookedOpt opts (MCLogUnusedCollaterals (Right (length -> n))) =
    "Specific request to fetch collateral utxos from the given set of "
      <> prettyCookedOpt opts n
      <> " elements has been disregarded because the transaction does not require collaterals"
  prettyCookedOpt opts (MCLogAddedReferenceScript red oRef sHash) =
    "A reference script located in "
      <> prettyCookedOpt opts oRef
      <> " has been automatically associated to redeemer "
      <> prettyItemizeNoTitle opts "-" red
      <> " for script "
      <> prettyHash opts sHash

-- | Pretty print a 'UtxoState'. Print the known wallets first, then unknown
-- pubkeys, then scripts.
instance PrettyCooked UtxoState where
  prettyCookedOpt opts =
    prettyItemize opts "UTxO state:" "•"
      . map (\(addr, plSet) -> prettyItemize opts (prettyCookedOpt opts addr) "-" plSet)
      . List.sortBy addressOrdering
      . Map.toList
      . utxoState
    where
      addressOrdering :: (Api.Address, a) -> (Api.Address, a) -> Ordering
      addressOrdering
        (a1@(Api.Address (Api.PubKeyCredential pkh1) _), _)
        (a2@(Api.Address (Api.PubKeyCredential pkh2) _), _) =
          case (walletPKHashToId pkh1, walletPKHashToId pkh2) of
            (Just i, Just j) -> compare i j
            (Just _, Nothing) -> LT
            (Nothing, Just _) -> GT
            (Nothing, Nothing) -> compare a1 a2
      addressOrdering
        (Api.Address (Api.PubKeyCredential _) _, _)
        (Api.Address (Api.ScriptCredential _) _, _) = LT
      addressOrdering (a1, _) (a2, _) = compare a1 a2

-- | Pretty prints the state of an address, that is the list of UTxOs (including
-- value and datum), grouped
instance PrettyCookedList UtxoPayloadSet where
  prettyCookedOptListMaybe opts =
    (prettyPayloadGrouped <$>)
      . group
      . List.sortBy (compare `on` (Api.lovelaceValueOf . utxoPayloadValue))
      . utxoPayloadSet
    where
      similar :: UtxoPayload -> UtxoPayload -> Bool
      similar
        (UtxoPayload _ value1 skelOutDatum1 refScript1)
        (UtxoPayload _ value2 skelOutDatum2 refScript2) =
          value1 == value2
            && skelOutDatum1 == skelOutDatum2
            && refScript1 == refScript2

      group :: [UtxoPayload] -> [[UtxoPayload]]
      group =
        case pcOptPrintTxOutRefs opts of
          PCOptTxOutRefsFull -> map (: [])
          _ -> List.groupBy similar

      -- Pretty prints payloads (datum and value corresponding to 1 UTxO)
      -- grouped together when they carry same value and datum
      prettyPayloadGrouped :: [UtxoPayload] -> Maybe DocCooked
      prettyPayloadGrouped [] = Nothing
      prettyPayloadGrouped [payload] = prettyPayload (pcOptPrintTxOutRefs opts /= PCOptTxOutRefsHidden) payload
      prettyPayloadGrouped (payload : rest) =
        (PP.parens ("×" <> prettyCookedOpt opts (1 + length rest)) <+>)
          <$> prettyPayload False payload

      -- Optionally prints a 'UtxoPayload' with an option piloting whether
      -- 'Api.TxOutRef's should be shown.
      prettyPayload :: Bool -> UtxoPayload -> Maybe DocCooked
      prettyPayload showTxOutRef UtxoPayload {..} =
        case catMaybes
          [ if showTxOutRef
              then Just $ prettyCookedOpt opts utxoPayloadTxOutRef
              else Nothing,
            Just (prettyCookedOpt opts utxoPayloadValue),
            prettyCookedOptMaybe opts utxoPayloadSkelOutDatum,
            ("Reference script hash:" <+>) . prettyHash opts <$> utxoPayloadReferenceScript
          ] of
          [] -> Nothing
          [doc] -> Just $ PP.align doc
          docs -> Just . PP.align . PP.vsep $ docs

newtype CollateralInput = CollateralInput {unCollateralInput :: Api.TxOutRef}

instance PrettyCooked (Contextualized CollateralInput) where
  prettyCookedOpt opts cColIn@(Contextualized _ _ (CollateralInput txOutRef)) =
    case prettyCookedOptList opts (unCollateralInput <$> cColIn) of
      (addressDoc : valueDoc : otherDocs) ->
        prettyItemize opts ("Belonging to" <+> addressDoc) "-" (valueDoc : otherDocs)
      _ -> "Uses" <+> prettyCookedOpt opts txOutRef <+> "(non resolved)"
