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
-- 'SkelContext').
module Cooked.Pretty.Cooked
  ( prettyTxSkel,
    prettyBalancingWallet,
    prettySigners,
    mPrettyTxOpts,
    mPrettyTxSkelOutDatum,
    prettyTxSkelIn,
    prettyTxSkelInReference,
    prettyAddressState,
    prettyPayloadGrouped,
    prettyPayload,
    prettyReferenceScriptHash,
  )
where

import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
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
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
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
  prettyCookedOpt opts (MCEUnbalanceable balWallet missingValue _) =
    prettyItemize
      "Unbalanceable:"
      "-"
      [ prettyCookedOpt opts (walletPKHash balWallet) <+> "does not have enough funds",
        if missingValue == mempty
          then "Not enough funds to sustain the minimal ada of the return utxo"
          else "Unable to find" <+> prettyCookedOpt opts missingValue
      ]
  prettyCookedOpt opts (MCENoSuitableCollateral fee percentage colVal) =
    prettyItemize
      "No suitable collateral"
      "-"
      [ "Fee was" <+> prettyCookedOpt opts fee,
        "Percentage in params was" <+> prettyCookedOpt opts percentage,
        "Resulting minimal collateral value was" <+> prettyCookedOpt opts colVal
      ]
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

instance (Show a) => PrettyCooked (a, UtxoState) where
  prettyCookedOpt opts (res, state) =
    prettyItemize
      "End state:"
      "-"
      ["Returns:" <+> PP.viaShow res, prettyCookedOpt opts state]

instance (Show a) => PrettyCooked (MockChainReturn a UtxoState) where
  prettyCookedOpt opts' (res, MockChainBook entries ((`addHashNames` opts') -> opts)) =
    let mcLog = "📘" <+> prettyItemize "MockChain run log:" "⁍" (prettyCookedOpt opts <$> entries)
        mcEndResult = case res of
          Left err -> "🔴" <+> prettyCookedOpt opts err
          Right (a, s) -> "🟢" <+> prettyCookedOpt opts (a, s)
     in PP.vsep $ if pcOptPrintLog opts then [mcLog, mcEndResult] else [mcEndResult]

-- | This prints a 'MockChainLogEntry'. In the log, we know a transaction has
-- been validated if the 'MCLogSubmittedTxSkel' is followed by a 'MCLogNewTx'.
instance PrettyCooked MockChainLogEntry where
  prettyCookedOpt opts (MCLogAdjustedTxSkelOut skelOut newAda) =
    "The ADA amount of "
      <> prettyCookedOpt opts skelOut
      <> " has been automatically adjusted to "
      <> prettyCookedOpt opts (Script.toValue newAda)
  prettyCookedOpt opts (MCLogSubmittedTxSkel skelContext skel) = prettyItemize "Submitted:" "-" [prettyTxSkel opts skelContext skel]
  prettyCookedOpt opts (MCLogAdjustedTxSkel skelContext skel fee mCollaterals) =
    let mCollateralsDoc =
          ( \(collaterals, returnWallet) ->
              [ prettyItemize "Collateral inputs:" "-" (prettyCollateralIn opts skelContext <$> Set.toList collaterals),
                "Return collateral target:" <+> prettyCookedOpt opts (walletPKHash returnWallet)
              ]
          )
            <$> mCollaterals
     in prettyItemize
          "Adjusted:"
          "-"
          $ [ prettyTxSkel opts skelContext skel,
              "Fee:" <+> prettyCookedOpt opts (Script.lovelace fee)
            ]
            ++ fromMaybe [] mCollateralsDoc
  prettyCookedOpt opts (MCLogNewTx txId) = "New transaction:" <+> prettyCookedOpt opts txId
  prettyCookedOpt opts (MCLogDiscardedUtxos n s) = prettyCookedOpt opts n <+> "balancing utxos were discarded:" <+> PP.pretty s
  prettyCookedOpt opts (MCLogUnusedCollaterals (Left cWallet)) =
    "Specific request to fetch collateral utxos from "
      <> prettyCookedOpt opts (walletPKHash cWallet)
      <> " has been disregarded because the transaction does not require collaterals"
  prettyCookedOpt opts (MCLogUnusedCollaterals (Right (length -> n))) =
    "Specific request to fetch collateral utxos from the given set of "
      <> prettyCookedOpt opts n
      <> " elements has been disregarded because the transaction does not require collaterals"
  prettyCookedOpt opts (MCLogAddedReferenceScript red oRef sHash) =
    "A reference script located in "
      <> prettyCookedOpt opts oRef
      <> " has been automatically associated to redeemer "
      <> prettyItemizeNoTitle "-" (lPrettyTxSkelRedeemer opts red)
      <> " for script "
      <> prettyCookedOpt opts sHash

-- | Prints a 'TxSkel' within a certain 'SkelContext'
prettyTxSkel :: PrettyCookedOpts -> SkelContext -> TxSkel -> DocCooked
prettyTxSkel opts skelContext (TxSkel lbl txopts mints signers validityRange ins insReference outs proposals withdrawals) =
  prettyItemize
    "Transaction skeleton:"
    "-"
    ( catMaybes
        [ prettyItemizeNonEmpty "Labels:" "-" (prettyCookedOpt opts <$> Set.toList lbl),
          mPrettyTxOpts opts txopts,
          prettyItemizeNonEmpty "Mints:" "-" (prettyCookedOpt opts <$> txSkelMintsToList mints),
          Just $ "Validity interval:" <+> PP.pretty validityRange,
          prettyItemizeNonEmpty "Signers:" "-" (prettySigners opts txopts signers),
          prettyItemizeNonEmpty "Inputs:" "-" (prettyTxSkelIn opts skelContext <$> Map.toList ins),
          prettyItemizeNonEmpty "Reference inputs:" "-" (mapMaybe (prettyTxSkelInReference opts skelContext) $ Set.toList insReference),
          prettyItemizeNonEmpty "Outputs:" "-" (prettyCookedOpt opts <$> outs),
          prettyItemizeNonEmpty "Proposals:" "-" (prettyCookedOpt opts <$> proposals),
          mPrettyWithdrawals opts withdrawals
        ]
    )

-- | This prints a 'TxSkelWithdrawals' when it is not empty
mPrettyWithdrawals :: PrettyCookedOpts -> TxSkelWithdrawals -> Maybe DocCooked
mPrettyWithdrawals pcOpts withdrawals =
  prettyItemizeNonEmpty "Withdrawals:" "-" $ prettyWithdrawal <$> Map.toList withdrawals
  where
    prettyWithdrawal :: (Either (Script.Versioned Script.Script) Api.PubKeyHash, (TxSkelRedeemer, Api.Lovelace)) -> DocCooked
    prettyWithdrawal (cred, (red, ada)) =
      prettyItemizeNoTitle "-" $
        ( case cred of
            Left script -> prettyCookedOpt pcOpts script : lPrettyTxSkelRedeemer pcOpts red
            Right pkh -> [prettyCookedOpt pcOpts pkh]
        )
          ++ [prettyCookedOpt pcOpts (Script.toValue ada)]

instance PrettyCooked TxParameterChange where
  prettyCookedOpt opts (FeePerByte n) = "Fee per byte:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (FeeFixed n) = "Fee fixed:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (MaxBlockBodySize n) = "Max block body size:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (MaxTxSize n) = "Max transaction size:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (MaxBlockHeaderSize n) = "Max block header size:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (KeyDeposit n) = "Key deposit:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (PoolDeposit n) = "Pool deposit:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (PoolRetirementMaxEpoch n) = "Pool retirement max epoch:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (PoolNumber n) = "Pool number:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (PoolInfluence q) = "Pool influence:" <+> prettyCookedOpt opts q
  prettyCookedOpt opts (MonetaryExpansion q) = "Monetary expansion:" <+> prettyCookedOpt opts q
  prettyCookedOpt opts (TreasuryCut q) = "Treasury cut:" <+> prettyCookedOpt opts q
  prettyCookedOpt opts (MinPoolCost n) = "Min pool cost:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (CoinsPerUTxOByte n) = "Lovelace per utxo byte:" <+> prettyCookedOpt opts n
  prettyCookedOpt _opts (CostModels _pv1 _pv2 _pv3) = "Cost models (unsupported)"
  prettyCookedOpt opts (Prices q r) =
    prettyItemize
      "Prices:"
      "-"
      [ "Memory cost:" <+> prettyCookedOpt opts q,
        "Step cost:" <+> prettyCookedOpt opts r
      ]
  prettyCookedOpt opts (MaxTxExUnits n m) =
    prettyItemize
      "Max transaction execution units:"
      "-"
      [ "Max memory:" <+> prettyCookedOpt opts n,
        "Max steps:" <+> prettyCookedOpt opts m
      ]
  prettyCookedOpt opts (MaxBlockExUnits n m) =
    prettyItemize
      "Max block execution units:"
      "-"
      [ "Max memory:" <+> prettyCookedOpt opts n,
        "Max steps:" <+> prettyCookedOpt opts m
      ]
  prettyCookedOpt opts (MaxValSize n) = "Max value size:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (CollateralPercentage n) = "Collateral percentage:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (MaxCollateralInputs n) = "Max number of collateral inputs:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (PoolVotingThresholds a b c d e) =
    prettyItemize
      "Pool voting thresholds:"
      "-"
      [ "Motion no confidence:" <+> prettyCookedOpt opts a,
        "Committee normal:" <+> prettyCookedOpt opts b,
        "Committee no confidence:" <+> prettyCookedOpt opts c,
        "Hard fork:" <+> prettyCookedOpt opts d,
        "Security group:" <+> prettyCookedOpt opts e
      ]
  prettyCookedOpt opts (DRepVotingThresholds a b c d e f g h i j) =
    prettyItemize
      "DRep voting thresholds:"
      "-"
      [ "Motion no confidence:" <+> prettyCookedOpt opts a,
        "Committee normal:" <+> prettyCookedOpt opts b,
        "Committee no confidence:" <+> prettyCookedOpt opts c,
        "Update constitution:" <+> prettyCookedOpt opts d,
        "Hard fork initialization:" <+> prettyCookedOpt opts e,
        "Network group:" <+> prettyCookedOpt opts f,
        "Economic group:" <+> prettyCookedOpt opts g,
        "Technical group:" <+> prettyCookedOpt opts h,
        "Governance group:" <+> prettyCookedOpt opts i,
        "Treasury withdrawal:" <+> prettyCookedOpt opts j
      ]
  prettyCookedOpt opts (CommitteeMinSize n) = "Committee min size:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (CommitteeMaxTermLength n) = "Committee max term length:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (GovActionLifetime n) = "Governance action life time:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (GovActionDeposit n) = "Governance action deposit:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (DRepRegistrationDeposit n) = "DRep registration deposit:" <+> prettyCookedOpt opts n
  prettyCookedOpt opts (DRepActivity n) = "DRep activity:" <+> prettyCookedOpt opts n

-- | Prints a list of docs corresponding to an instance of 'TxSkelRedeemer'
lPrettyTxSkelRedeemer :: PrettyCookedOpts -> TxSkelRedeemer -> [DocCooked]
lPrettyTxSkelRedeemer opts (TxSkelRedeemer red mRefScript _) =
  catMaybes
    [ Just $ "Redeemer" <+> prettyCookedOpt opts red,
      ("Reference script at:" <+>) . prettyCookedOpt opts <$> mRefScript
    ]

instance PrettyCooked TxSkelProposal where
  prettyCookedOpt opts TxSkelProposal {..} =
    prettyItemizeNoTitle "-" $
      catMaybes
        [ Just $ "Governance action:" <+> prettyCookedOpt opts txSkelProposalAction,
          Just $ "Return address:" <+> prettyCooked txSkelProposalAddress,
          (\(script, redeemer) -> prettyItemize "Witness:" "-" (prettyCookedOpt opts script : lPrettyTxSkelRedeemer opts redeemer)) <$> txSkelProposalWitness,
          ("Anchor:" <+>) . PP.pretty <$> txSkelProposalAnchor
        ]

instance PrettyCooked TxGovAction where
  prettyCookedOpt opts (TxGovActionParameterChange params) = prettyItemize "Parameter changes:" "-" $ prettyCookedOpt opts <$> params
  prettyCookedOpt opts (TxGovActionHardForkInitiation (Api.ProtocolVersion major minor)) =
    "Protocol version:" <+> "(" <+> prettyCookedOpt opts major <+> "," <+> prettyCookedOpt opts minor <+> ")"
  prettyCookedOpt opts (TxGovActionTreasuryWithdrawals withdrawals) =
    prettyItemize "Withdrawals:" "-" $
      (\(cred, lv) -> prettyCookedOpt opts cred <+> "|" <+> prettyCooked (Script.toValue lv)) <$> Map.toList withdrawals
  prettyCookedOpt _ TxGovActionNoConfidence = "No confidence"
  prettyCookedOpt opts (TxGovActionUpdateCommittee toRemoveCreds toAddCreds quorum) =
    prettyItemize
      "Updates in committee:"
      "-"
      [ prettyItemize "Credentials to remove:" "-" $
          (\(Api.ColdCommitteeCredential cred) -> prettyCookedOpt opts cred) <$> toRemoveCreds,
        prettyItemize "Credentials to add:" "-" $
          (\(Api.ColdCommitteeCredential cred, i) -> prettyCookedOpt opts cred <+> "->" <+> prettyCookedOpt opts i) <$> Map.toList toAddCreds,
        "Quorum:" <+> prettyCookedOpt opts (Api.toGHC quorum)
      ]
  prettyCookedOpt opts (TxGovActionNewConstitution (Api.Constitution mScriptHash)) = case mScriptHash of
    Nothing -> "Empty new constitution"
    Just sHash -> "New constitution:" <+> prettyCookedOpt opts sHash

-- | Same as the 'PrettyCooked' instance for 'Wallet' with a suffix mentioning
-- this is the balancing wallet
prettyBalancingWallet :: PrettyCookedOpts -> Wallet -> DocCooked
prettyBalancingWallet opts w =
  prettyCookedOpt opts (walletPKHash w) <+> "[Balancing]"

-- | Prints a list of pubkeys with a flag next to the balancing wallet
prettySigners :: PrettyCookedOpts -> TxOpts -> [Wallet] -> [DocCooked]
prettySigners opts TxOpts {txOptBalancingPolicy = DoNotBalance} signers = prettyCookedOpt opts . walletPKHash <$> signers
prettySigners opts TxOpts {txOptBalancingPolicy = BalanceWithFirstSigner} (firstSigner : signers) =
  prettyBalancingWallet opts firstSigner : (prettyCookedOpt opts . walletPKHash <$> signers)
prettySigners opts TxOpts {txOptBalancingPolicy = BalanceWith balancingWallet} signers =
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
-- Example:
-- > #abcdef
--     - Redeemer: red
--     - Reference script at: txOutRef
--     - "Foo": 500
--     - "Bar": 1000
instance PrettyCooked Mint where
  prettyCookedOpt opts (Mint pol red tks) =
    prettyItemize (prettyCookedOpt opts (Script.toVersioned @Script.MintingPolicy pol)) "-" $
      lPrettyTxSkelRedeemer opts red ++ ((\(tk, n) -> PP.viaShow tk <> ":" <+> PP.viaShow n) <$> tks)

instance PrettyCooked TxSkelOut where
  prettyCookedOpt opts (Pays output) =
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
                Api.OutputDatumHash dHash ->
                  Just $
                    "Datum (hashed)"
                      <+> "("
                      <> prettyHash (pcOptHashes opts) (toHash dHash)
                      <> "):"
                      <+> (PP.align . prettyCookedOpt opts)
                        (output ^. outputDatumL)
                Api.NoOutputDatum -> Nothing,
              getReferenceScriptDoc opts output
            ]
      )

-- | Optionnally prints a 'TxSkelOutDatum' when its different from
-- 'TxSkelOutNoDatum'
mPrettyTxSkelOutDatum :: PrettyCookedOpts -> TxSkelOutDatum -> Maybe DocCooked
mPrettyTxSkelOutDatum _ TxSkelOutNoDatum = Nothing
mPrettyTxSkelOutDatum opts txSkelOutDatum@(TxSkelOutInlineDatum _) =
  Just $
    "Datum (inlined):"
      <+> PP.align (prettyCookedOpt opts txSkelOutDatum)
mPrettyTxSkelOutDatum opts txSkelOutDatum@(TxSkelOutDatumHash dat) =
  Just $
    "Datum (hashed)"
      <+> "("
      <> prettyHash (pcOptHashes opts) (toHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData dat)
      <> "):"
      <+> PP.align (prettyCookedOpt opts txSkelOutDatum)
mPrettyTxSkelOutDatum opts txSkelOutDatum@(TxSkelOutDatum dat) =
  Just $
    "Datum (hashed)"
      <+> "("
      <> prettyHash (pcOptHashes opts) (toHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData dat)
      <> "):"
      <+> PP.align (prettyCookedOpt opts txSkelOutDatum)

-- | Resolves a "TxOutRef" from a given context, builds a doc cooked for its
-- address and value, and also builds a possibly empty list for its datum and
-- reference script when they exist.
utxoToPartsAsDocCooked :: PrettyCookedOpts -> SkelContext -> Api.TxOutRef -> Maybe (DocCooked, DocCooked, [DocCooked])
utxoToPartsAsDocCooked opts skelContext txOutRef =
  ( \(output, txSkelOutDatum) ->
      ( prettyCookedOpt opts (outputAddress output),
        prettyCookedOpt opts (outputValue output),
        catMaybes
          [ mPrettyTxSkelOutDatum opts txSkelOutDatum,
            getReferenceScriptDoc opts output
          ]
      )
  )
    <$> lookupOutput skelContext txOutRef

-- | Prints a collateral input with a certain 'SkelContext'
prettyCollateralIn :: PrettyCookedOpts -> SkelContext -> Api.TxOutRef -> DocCooked
prettyCollateralIn opts skelContext txOutRef =
  case utxoToPartsAsDocCooked opts skelContext txOutRef of
    Nothing -> prettyCookedOpt opts txOutRef <+> "(non resolved)"
    Just (addressDoc, valueDoc, otherDocs) -> prettyItemize ("Belonging to" <+> addressDoc) "-" (valueDoc : otherDocs)

-- | Prints an input within a certain 'SkelContext'
prettyTxSkelIn :: PrettyCookedOpts -> SkelContext -> (Api.TxOutRef, TxSkelRedeemer) -> DocCooked
prettyTxSkelIn opts skelContext (txOutRef, txSkelRedeemer) =
  case utxoToPartsAsDocCooked opts skelContext txOutRef of
    Nothing -> "Spends" <+> prettyCookedOpt opts txOutRef <+> "(non resolved)"
    Just (addressDoc, valueDoc, otherDocs) ->
      prettyItemize ("Spends from" <+> addressDoc) "-" (valueDoc : lPrettyTxSkelRedeemer opts txSkelRedeemer <> otherDocs)

-- | Prints a reference input within a certain 'SkelContext'
prettyTxSkelInReference :: PrettyCookedOpts -> SkelContext -> Api.TxOutRef -> Maybe DocCooked
prettyTxSkelInReference opts skelContext txOutRef = do
  (output, txSkelOutDatum) <- lookupOutput skelContext txOutRef
  return $
    prettyItemize
      ("References output from" <+> prettyCookedOpt opts (outputAddress output))
      "-"
      ( prettyCookedOpt opts (outputValue output)
          : catMaybes
            [ mPrettyTxSkelOutDatum opts txSkelOutDatum,
              getReferenceScriptDoc opts output
            ]
      )

getReferenceScriptDoc :: (IsAbstractOutput output, Script.ToScriptHash (ReferenceScriptType output)) => PrettyCookedOpts -> output -> Maybe DocCooked
getReferenceScriptDoc opts output = prettyReferenceScriptHash opts . Script.toScriptHash <$> output ^. outputReferenceScriptL

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
    { txOptAutoSlotIncrease,
      txOptUnsafeModTx,
      txOptBalanceOutputPolicy,
      txOptFeePolicy,
      txOptBalancingPolicy,
      txOptBalancingUtxos,
      txOptEmulatorParamsModification,
      txOptCollateralUtxos,
      txOptAnchorResolution
    } =
    prettyItemizeNonEmpty "Options:" "-" $
      catMaybes
        [ prettyIfNot True prettyAutoSlotIncrease txOptAutoSlotIncrease,
          prettyIfNot def prettyBalanceOutputPolicy txOptBalanceOutputPolicy,
          prettyIfNot def prettyBalanceFeePolicy txOptFeePolicy,
          prettyIfNot def prettyBalancingPolicy txOptBalancingPolicy,
          prettyIfNot def prettyBalancingUtxos txOptBalancingUtxos,
          prettyIfNot [] prettyUnsafeModTx txOptUnsafeModTx,
          prettyIfNot def prettyEmulatorParamsModification txOptEmulatorParamsModification,
          prettyIfNot def prettyCollateralUtxos txOptCollateralUtxos,
          prettyIfNot def prettyAnchorResolution txOptAnchorResolution
        ]
    where
      prettyIfNot :: (Eq a) => a -> (a -> DocCooked) -> a -> Maybe DocCooked
      prettyIfNot defaultValue f x
        | x == defaultValue && not (pcOptPrintDefaultTxOpts opts) = Nothing
        | otherwise = Just $ f x
      prettyAutoSlotIncrease :: Bool -> DocCooked
      prettyAutoSlotIncrease True = "Automatic slot increase"
      prettyAutoSlotIncrease False = "No automatic slot increase"
      prettyBalanceOutputPolicy :: BalanceOutputPolicy -> DocCooked
      prettyBalanceOutputPolicy AdjustExistingOutput = "Balance policy: Adjust existing outputs"
      prettyBalanceOutputPolicy DontAdjustExistingOutput = "Balance policy: Don't adjust existing outputs"
      prettyBalancingPolicy :: BalancingPolicy -> DocCooked
      prettyBalancingPolicy BalanceWithFirstSigner = "Balance with first signer"
      prettyBalancingPolicy (BalanceWith w) = "Balance with" <+> prettyCookedOpt opts (walletPKHash w)
      prettyBalancingPolicy DoNotBalance = "Do not balance"
      prettyUnsafeModTx :: [RawModTx] -> DocCooked
      prettyUnsafeModTx [] = "No transaction modifications"
      prettyUnsafeModTx (length -> n) = prettyCookedOpt opts n <+> "transaction" <+> PP.plural "modification" "modifications" n
      prettyEmulatorParamsModification :: Maybe EmulatorParamsModification -> DocCooked
      prettyEmulatorParamsModification Nothing = "No modifications of protocol paramters"
      prettyEmulatorParamsModification Just {} = "With modifications of protocol parameters"
      prettyCollateralUtxos :: CollateralUtxos -> DocCooked
      prettyCollateralUtxos CollateralUtxosFromBalancingWallet =
        prettyItemize "Collateral policy:" "-" ["Use value-only utxos from balancing wallet", "Send return collaterals to balancing wallet"]
      prettyCollateralUtxos (CollateralUtxosFromWallet w)
        | prettyWallet <- prettyCookedOpt opts (walletPKHash w) =
            prettyItemize "Collateral policy:" "-" ["Use value-only utxos from" <+> prettyWallet, "Send return collaterals to" <+> prettyWallet]
      prettyCollateralUtxos (CollateralUtxosFromSet txOutRefs w) =
        prettyItemize
          "Collateral policy:"
          "-"
          [ prettyItemize "Choose among the following TxOutRefs:" "-" (prettyCookedOpt opts <$> Set.toList txOutRefs),
            "Send return collaterals to" <+> prettyCookedOpt opts (walletPKHash w)
          ]
      prettyBalancingUtxos :: BalancingUtxos -> DocCooked
      prettyBalancingUtxos BalancingUtxosFromBalancingWallet = "Balance with 'only value' utxos from the balancing wallet"
      prettyBalancingUtxos (BalancingUtxosFromSet utxos) = prettyItemize "Balance with the following utxos:" "-" (prettyCookedOpt opts <$> Set.toList utxos)
      prettyBalanceFeePolicy :: FeePolicy -> DocCooked
      prettyBalanceFeePolicy AutoFeeComputation = "Use automatically computed fee"
      prettyBalanceFeePolicy (ManualFee fee) = "Use the following fee:" <+> prettyCookedOpt opts fee
      prettyAnchorResolution :: AnchorResolution -> DocCooked
      prettyAnchorResolution AnchorResolutionHttp = "Resolve anchor url with an (unsafe) http connection"
      prettyAnchorResolution (AnchorResolutionLocal urlMap) = prettyItemize "Resolve anchor url with the following table keys" "-" (PP.pretty <$> Map.keys urlMap)

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
        . List.sortBy (compare `on` (Api.lovelaceValueOf . utxoPayloadValue))
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

-- | Optionally prints a 'UtxoPayload' with an option piloting whether
-- 'Api.TxOutRef's should be shown.
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
        mPrettyTxSkelOutDatum opts utxoPayloadSkelOutDatum,
        prettyReferenceScriptHash opts <$> utxoPayloadReferenceScript
      ] of
      [] -> Nothing
      [doc] -> Just $ PP.align doc
      docs -> Just . PP.align . PP.vsep $ docs

-- | Prints a reference script hash
prettyReferenceScriptHash :: PrettyCookedOpts -> Script.ScriptHash -> DocCooked
prettyReferenceScriptHash opts scriptHash =
  "Reference script hash:"
    <+> prettyHash (pcOptHashes opts) (toHash scriptHash)
