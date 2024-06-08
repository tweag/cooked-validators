{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module implements pretty-printing for Cooked structures such as
-- skeletons and chain state.
--
-- It contains orphaned instances of 'PrettyCooked' for Cooked datatypes. They
-- cannot be provided in "Cooked.Pretty.Class" because of dependency cycles and,
-- for ease of maintainability, we chose to centralize all pretty-printing
-- related code in submodules of "Cooked.Pretty" instead of having
-- 'PrettyCooked' instances scattered around.
--
-- Some structure require additional arguments to be pretty-printed and have
-- therefore no instances 'PrettyCooked' (for example 'TxSkel' needs some
-- 'TxSkelContext').
module Cooked.Pretty.Cooked
  ( prettyTxSkel,
    prettyBalancingWallet,
    prettySigners,
    prettyMints,
    mPrettyTxOpts,
    prettyTxSkelOut,
    prettyTxSkelOutDatumMaybe,
    prettyTxSkelIn,
    prettyTxSkelInReference,
    prettyAddressState,
    prettyPayloadGrouped,
    prettyPayload,
    prettyReferenceScriptHash,
  )
where

import Cooked.Conversion.ToScriptHash
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.Staged
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.Pretty.Common
import Cooked.Pretty.Hashable
import Cooked.Pretty.Options
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import Data.Function (on)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

-- | The 'PrettyCooked' instance for 'TxSkelOutDatum' prints the datum it
-- contains according to its own 'PrettyCooked' instance.
instance PrettyCooked TxSkelOutDatum where
  prettyCookedOpt _ TxSkelOutNoDatum = mempty
  prettyCookedOpt opts (TxSkelOutDatumHash datum) = prettyCookedOpt opts datum
  prettyCookedOpt opts (TxSkelOutDatum datum) = prettyCookedOpt opts datum
  prettyCookedOpt opts (TxSkelOutInlineDatum datum) = prettyCookedOpt opts datum

instance PrettyCooked MockChainError where
  prettyCookedOpt opts (MCEValidationError plutusPhase plutusError) =
    PP.vsep ["Validation error " <+> prettyCookedOpt opts plutusPhase, PP.indent 2 (prettyCookedOpt opts plutusError)]
  -- Here we don't print the skel because we lack its context and this error is
  -- printed alongside the skeleton when a test fails
  prettyCookedOpt opts (MCEUnbalanceable (MCEUnbalNotEnoughFunds balWallet targetValue) _) =
    prettyItemize
      "Unbalanceable:"
      "-"
      [ prettyCookedOpt opts (walletPKHash balWallet) <+> "has not enough funds",
        "Required payment is" <+> prettyCookedOpt opts targetValue
      ]
  prettyCookedOpt opts (MCEUnbalanceable (MCEUnbalNotEnoughReturning (spentValue, spentTxOuts) (remainingValue, remainingTxOuts) returnValue) _) =
    prettyItemize
      "Unbalanceable:"
      "-"
      [ "Value to return is below the min ada per UTxO:"
          <+> prettyCookedOpt opts returnValue,
        prettyItemize
          "Spent for balancing:"
          "-"
          [ prettyCookedOpt opts spentValue,
            prettyItemize
              "Outputs:"
              "-"
              (prettyCookedOpt opts <$> spentTxOuts)
          ],
        prettyItemize
          "Remaining candidates:"
          "-"
          [ prettyCookedOpt opts remainingValue,
            prettyItemize
              "Outputs:"
              "-"
              (prettyCookedOpt opts <$> remainingTxOuts)
          ]
      ]
  prettyCookedOpt _ MCENoSuitableCollateral =
    "No suitable collateral"
  prettyCookedOpt _ (MCEGenerationError (ToCardanoError msg cardanoError)) =
    prettyItemize
      "Transaction generation error:"
      "-"
      [PP.pretty msg, PP.pretty cardanoError]
  prettyCookedOpt _ (MCEGenerationError (GenerateTxErrorGeneral msg)) =
    prettyItemize
      "Transaction generation error:"
      "-"
      [PP.pretty msg]
  prettyCookedOpt _ (MCEGenerationError (TxBodyError msg err)) =
    prettyItemize
      "Transaction generation error:"
      "-"
      [PP.pretty msg, PP.viaShow err]
  prettyCookedOpt opts (MCEUnknownOutRefError msg txOutRef) =
    prettyItemize
      "Unknown transaction output ref:"
      "-"
      [PP.pretty msg, prettyCookedOpt opts txOutRef]
  prettyCookedOpt _ (FailWith msg) =
    "Failed with:" <+> PP.pretty msg
  prettyCookedOpt opts (MCEUnknownValidator msg valHash) =
    prettyItemize
      "Unknown validator hash:"
      "-"
      [PP.pretty msg, "hash:" <+> prettyHash (pcOptHashes opts) (toHash valHash)]
  prettyCookedOpt opts (MCEUnknownDatum msg dHash) =
    prettyItemize
      "Unknown datum hash:"
      "-"
      [PP.pretty msg, "hash:" <+> prettyHash (pcOptHashes opts) (toHash dHash)]
  prettyCookedOpt _ (OtherMockChainError err) =
    prettyItemize
      "Miscellaneous MockChainError:"
      "-"
      [PP.viaShow err]

instance (Show a) => PrettyCooked (a, UtxoState) where
  prettyCookedOpt opts (res, state) =
    prettyItemize
      "End state:"
      "-"
      ["Returns:" <+> PP.viaShow res, prettyCookedOpt opts state]

instance (Show a) => PrettyCooked (Either MockChainError (a, UtxoState)) where
  prettyCookedOpt opts (Left err) = "🔴" <+> prettyCookedOpt opts err
  prettyCookedOpt opts (Right endState) = "🟢" <+> prettyCookedOpt opts endState

-- | This pretty prints a 'MockChainLog' that usually consists of the list of
-- validated or submitted transactions. In the log, we know a transaction has
-- been validated if the 'MCLogSubmittedTxSkel' is followed by a 'MCLogNewTx'.
instance PrettyCooked MockChainLog where
  prettyCookedOpt opts =
    prettyEnumerate "MockChain run:" "."
      . go []
      . unMockChainLog
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
                    <+> prettyTxSkel opts skelContext skel
                    : acc
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
prettyTxSkel opts skelContext (TxSkel lbl txopts mints signers validityRange ins insReference outs) =
  prettyItemize
    "transaction skeleton:"
    "-"
    ( catMaybes
        [ prettyItemizeNonEmpty "Labels:" "-" (prettyCookedOpt opts <$> Set.toList lbl),
          mPrettyTxOpts opts txopts,
          prettyItemizeNonEmpty "Mints:" "-" (prettyMints opts <$> txSkelMintsToList mints),
          Just $ "Validity interval:" <+> PP.pretty validityRange,
          prettyItemizeNonEmpty "Signers:" "-" (prettySigners opts txopts signers),
          prettyItemizeNonEmpty "Inputs:" "-" (prettyTxSkelIn opts skelContext <$> Map.toList ins),
          prettyItemizeNonEmpty "Reference inputs:" "-" (mapMaybe (prettyTxSkelInReference opts skelContext) $ Set.toList insReference),
          prettyItemizeNonEmpty "Outputs:" "-" (prettyTxSkelOut opts <$> outs)
        ]
    )

-- | Same as the 'PrettyCooked' instance for 'Wallet' with a suffix mentioning
-- this is the balancing wallet
prettyBalancingWallet :: PrettyCookedOpts -> Wallet -> DocCooked
prettyBalancingWallet opts w =
  prettyCookedOpt opts (walletPKHash w) <+> "[Balancing]"

-- | Prints a list of pubkeys with a flag next to the balancing wallet
prettySigners :: PrettyCookedOpts -> TxOpts -> [Wallet] -> [DocCooked]
prettySigners opts TxOpts {txOptBalanceWallet = BalanceWithFirstSigner} (firstSigner : signers) =
  prettyBalancingWallet opts firstSigner : (prettyCookedOpt opts . walletPKHash <$> signers)
prettySigners opts TxOpts {txOptBalanceWallet = BalanceWith balancingWallet} signers =
  aux signers
  where
    aux :: [Wallet] -> [DocCooked]
    aux [] = []
    aux (s : ss)
      | s == balancingWallet = prettyBalancingWallet opts balancingWallet : aux ss
      | otherwise = prettyCookedOpt opts (walletPKHash s) : aux ss
-- The following case should never happen for real transactions
prettySigners _ _ [] = []

-- | Prints a minting specification
--
-- Examples without and with redeemer
-- > #abcdef "Foo" -> 500
-- > #123456 "Bar" | Redeemer -> 1000
prettyMints :: PrettyCookedOpts -> (Script.Versioned Script.MintingPolicy, MintsRedeemer, Api.TokenName, Integer) -> DocCooked
prettyMints opts (policy, NoMintsRedeemer, tokenName, amount) =
  prettyCookedOpt opts policy
    <+> PP.viaShow tokenName
    <+> "->"
    <+> PP.viaShow amount
prettyMints opts (policy, SomeMintsRedeemer redeemer, tokenName, amount) =
  prettyCookedOpt opts policy
    <+> PP.viaShow tokenName
    <+> "|"
    <+> prettyCookedOpt opts redeemer
    <+> "->"
    <+> PP.viaShow amount

prettyTxSkelOut :: PrettyCookedOpts -> TxSkelOut -> DocCooked
prettyTxSkelOut opts (Pays output) =
  prettyItemize
    ("Pays to" <+> prettyCookedOpt opts (outputAddress output))
    "-"
    ( prettyCookedOpt opts (outputValue output)
        : catMaybes
          [ case outputOutputDatum output of
              Api.OutputDatum _datum ->
                Just $
                  "Datum (inlined):"
                    <+> (PP.align . prettyCookedOpt opts)
                      (output ^. outputDatumL)
              Api.OutputDatumHash _datum ->
                Just $
                  "Datum (hashed):"
                    <+> (PP.align . prettyCookedOpt opts)
                      (output ^. outputDatumL)
              Api.NoOutputDatum -> Nothing,
            getReferenceScriptDoc opts output
          ]
    )

prettyTxSkelOutDatumMaybe :: PrettyCookedOpts -> TxSkelOutDatum -> Maybe DocCooked
prettyTxSkelOutDatumMaybe _ TxSkelOutNoDatum = Nothing
prettyTxSkelOutDatumMaybe opts txSkelOutDatum@(TxSkelOutInlineDatum _) =
  Just $
    "Datum (inlined):"
      <+> PP.align (prettyCookedOpt opts txSkelOutDatum)
prettyTxSkelOutDatumMaybe opts txSkelOutDatum =
  Just $
    "Datum (hashed):"
      <+> PP.align (prettyCookedOpt opts txSkelOutDatum)

prettyTxSkelIn :: PrettyCookedOpts -> SkelContext -> (Api.TxOutRef, TxSkelRedeemer) -> DocCooked
prettyTxSkelIn opts skelContext (txOutRef, txSkelRedeemer) = do
  case lookupOutput skelContext txOutRef of
    Nothing -> "Spends" <+> prettyCookedOpt opts txOutRef <+> "(non resolved)"
    Just (output, txSkelOutDatum) ->
      let (redeemerDoc, ownerDoc) =
            case txSkelRedeemer of
              TxSkelRedeemerForScript redeemer ->
                ( Just ("Redeemer:" <+> prettyCookedOpt opts redeemer),
                  prettyCookedOpt opts (outputAddress output)
                )
              TxSkelRedeemerForReferencedScript refScriptOref redeemer ->
                ( Just ("Redeemer:" <+> prettyCookedOpt opts redeemer),
                  prettyCookedOpt opts (outputAddress output)
                    <+> PP.parens ("Reference Script at" <+> prettyCookedOpt opts refScriptOref)
                )
              TxSkelNoRedeemerForPK -> (Nothing, prettyCookedOpt opts (outputAddress output))
       in prettyItemize
            ("Spends from" <+> ownerDoc)
            "-"
            ( prettyCookedOpt opts (outputValue output)
                : catMaybes
                  [ redeemerDoc,
                    prettyTxSkelOutDatumMaybe opts txSkelOutDatum,
                    getReferenceScriptDoc opts output
                  ]
            )

prettyTxSkelInReference :: PrettyCookedOpts -> SkelContext -> Api.TxOutRef -> Maybe DocCooked
prettyTxSkelInReference opts skelContext txOutRef = do
  (output, txSkelOutDatum) <- lookupOutput skelContext txOutRef
  return $
    prettyItemize
      ("References output from" <+> prettyCookedOpt opts (outputAddress output))
      "-"
      ( prettyCookedOpt opts (outputValue output)
          : catMaybes
            [ prettyTxSkelOutDatumMaybe opts txSkelOutDatum,
              getReferenceScriptDoc opts output
            ]
      )

getReferenceScriptDoc :: (IsAbstractOutput output, ToScriptHash (ReferenceScriptType output)) => PrettyCookedOpts -> output -> Maybe DocCooked
getReferenceScriptDoc opts output = prettyReferenceScriptHash opts . toScriptHash <$> output ^. outputReferenceScriptL

lookupOutput :: SkelContext -> Api.TxOutRef -> Maybe (Api.TxOut, TxSkelOutDatum)
lookupOutput (SkelContext managedTxOuts managedTxSkelOutDatums) txOutRef = do
  output <- Map.lookup txOutRef managedTxOuts
  return
    ( output,
      case outputOutputDatum output of
        Api.OutputDatum datum -> Map.findWithDefault TxSkelOutNoDatum (Script.datumHash datum) managedTxSkelOutDatums
        Api.OutputDatumHash datumHash -> Map.findWithDefault TxSkelOutNoDatum datumHash managedTxSkelOutDatums
        Api.NoOutputDatum -> TxSkelOutNoDatum
    )

-- | Pretty-print a list of transaction skeleton options, only printing an
-- option if its value is non-default. If no non-default options are in the
-- list, return nothing.
mPrettyTxOpts :: PrettyCookedOpts -> TxOpts -> Maybe DocCooked
mPrettyTxOpts
  opts
  TxOpts
    { txOptEnsureMinAda,
      txOptAutoSlotIncrease,
      txOptUnsafeModTx,
      txOptBalance,
      txOptBalanceOutputPolicy,
      txOptBalanceWallet,
      txOptBalancingUtxos,
      txOptEmulatorParamsModification,
      txOptCollateralUtxos
    } =
    prettyItemizeNonEmpty "Options:" "-" $
      catMaybes
        [ prettyIfNot def prettyEnsureMinAda txOptEnsureMinAda,
          prettyIfNot True prettyAutoSlotIncrease txOptAutoSlotIncrease,
          prettyIfNot True prettyBalance txOptBalance,
          prettyIfNot def prettyBalanceOutputPolicy txOptBalanceOutputPolicy,
          prettyIfNot def prettyBalanceWallet txOptBalanceWallet,
          prettyIfNot def prettyBalancingUtxos txOptBalancingUtxos,
          prettyIfNot [] prettyUnsafeModTx txOptUnsafeModTx,
          prettyIfNot def prettyEmulatorParamsModification txOptEmulatorParamsModification,
          prettyIfNot def prettyCollateralUtxos txOptCollateralUtxos
        ]
    where
      prettyIfNot :: (Eq a) => a -> (a -> DocCooked) -> a -> Maybe DocCooked
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
      prettyUnsafeModTx (length -> n) = PP.pretty n <+> "transaction" <+> PP.plural "modification" "modifications" n
      prettyEmulatorParamsModification :: Maybe EmulatorParamsModification -> DocCooked
      prettyEmulatorParamsModification Nothing = "No modifications of protocol paramters"
      prettyEmulatorParamsModification Just {} = "With modifications of protocol parameters"
      prettyCollateralUtxos :: CollateralUtxos -> DocCooked
      prettyCollateralUtxos CollateralUtxosFromBalancingWallet =
        prettyItemize "Collateral policy:" "-" ["Use vanilla utxos from balancing wallet", "Send return collaterals to balancing wallet"]
      prettyCollateralUtxos (CollateralUtxosFromWallet w)
        | prettyWallet <- prettyCookedOpt opts (walletPKHash w) =
            prettyItemize "Collateral policy:" "-" ["Use vanilla utxos from" <+> prettyWallet, "Send return collaterals to" <+> prettyWallet]
      prettyCollateralUtxos (CollateralUtxosFromSet txOutRefs w) =
        prettyItemize
          "Collateral policy:"
          "-"
          [ prettyItemize "Choose among the following TxOutRefs:" "-" (prettyCookedOpt opts <$> Set.toList txOutRefs),
            "Send return collaterals to" <+> prettyCookedOpt opts (walletPKHash w)
          ]
      prettyBalancingUtxos :: BalancingUtxos -> DocCooked
      prettyBalancingUtxos BalancingUtxosAutomatic = "Balance with 'only value' utxos from the balancing wallet"
      prettyBalancingUtxos (BalancingUtxosWith utxos) = prettyItemize "Balance with the following utxos:" "-" (prettyCookedOpt opts <$> Set.toList utxos)

-- * Pretty-printing

-- | Pretty print a 'UtxoState'. Print the known wallets first, then unknown
-- pubkeys, then scripts.
instance PrettyCooked UtxoState where
  prettyCookedOpt opts =
    prettyItemize "UTxO state:" "•"
      . map (uncurry (prettyAddressState opts))
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
prettyAddressState :: PrettyCookedOpts -> Api.Address -> UtxoPayloadSet -> DocCooked
prettyAddressState opts address payloadSet =
  prettyItemize
    (prettyCookedOpt opts address)
    "-"
    ( mapMaybe (prettyPayloadGrouped opts)
        . group
        . List.sortBy (compare `on` (Script.fromValue . utxoPayloadValue))
        . utxoPayloadSet
        $ payloadSet
    )
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

-- | Pretty prints payloads (datum and value corresponding to 1 UTxO) grouped
-- together when they carry same value and datum
prettyPayloadGrouped :: PrettyCookedOpts -> [UtxoPayload] -> Maybe DocCooked
prettyPayloadGrouped _ [] = Nothing
prettyPayloadGrouped opts [payload] =
  prettyPayload
    opts
    (pcOptPrintTxOutRefs opts /= PCOptTxOutRefsHidden)
    payload
prettyPayloadGrouped opts (payload : rest) =
  let cardinality = 1 + length rest
   in (PP.parens ("×" <> prettyCookedOpt opts cardinality) <+>)
        <$> prettyPayload opts False payload

prettyPayload :: PrettyCookedOpts -> Bool -> UtxoPayload -> Maybe DocCooked
prettyPayload
  opts
  showTxOutRef
  ( UtxoPayload
      { utxoPayloadTxOutRef,
        utxoPayloadValue,
        utxoPayloadSkelOutDatum,
        utxoPayloadReferenceScript
      }
    ) =
    case catMaybes
      [ if showTxOutRef
          then Just $ prettyCookedOpt opts utxoPayloadTxOutRef
          else Nothing,
        Just (prettyCookedOpt opts utxoPayloadValue),
        prettyTxSkelOutDatumMaybe opts utxoPayloadSkelOutDatum,
        prettyReferenceScriptHash opts <$> utxoPayloadReferenceScript
      ] of
      [] -> Nothing
      [doc] -> Just $ PP.align doc
      docs -> Just . PP.align . PP.vsep $ docs

prettyReferenceScriptHash :: PrettyCookedOpts -> Script.ScriptHash -> DocCooked
prettyReferenceScriptHash opts scriptHash =
  "Reference script hash:"
    <+> prettyHash (pcOptHashes opts) (toHash scriptHash)
