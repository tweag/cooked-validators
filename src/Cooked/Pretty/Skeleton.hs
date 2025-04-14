{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module implements 'PrettyCooked', 'PrettyCookedL' and 'PrettyCookedM'
-- instances for 'TxSkel's and its components.
module Cooked.Pretty.Skeleton (Contextualized (..), CollateralInput (..)) where

import Cooked.Output
import Cooked.Pretty.Common
import Cooked.Pretty.Options
import Cooked.Pretty.Plutus ()
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

instance PrettyCooked Wallet where
  prettyCookedOpt opts = prettyHash opts . walletPKHash

data Contextualized a = Contextualized
  { _ctxOutputs :: Map Api.TxOutRef Api.TxOut,
    _ctxDatums :: Map Api.DatumHash TxSkelOutDatum,
    ctxContent :: a
  }
  deriving (Functor)

-- | Prints a 'Contextualized' 'TxSkel'
instance PrettyCookedL (Contextualized TxSkel) where
  prettyCookedOptLM opts cTxSkel
    | TxSkel lbl txopts mints signers validityRange ins insReference outs proposals withdrawals <- ctxContent cTxSkel =
        [ prettyItemizeNonEmpty opts "Labels:" "-" lbl,
          prettyItemizeNonEmpty opts "Mints:" "-" (txSkelMintsToList mints),
          Just $ "Validity interval:" <+> PP.pretty validityRange,
          prettyItemizeNonEmpty opts "Signers:" "-" (txopts, signers),
          prettyItemizeNonEmpty opts "Inputs:" "-" ((<$ cTxSkel) . uncurry Input <$> Map.toList ins),
          prettyItemizeNonEmpty opts "Reference inputs:" "-" $ prettyCookedOpt opts . (<$ cTxSkel) . ReferenceInput <$> Set.toList insReference,
          prettyItemizeNonEmpty opts "Outputs:" "-" (prettyCookedOpt opts <$> outs),
          prettyItemizeNonEmpty opts "Proposals:" "-" (prettyItemizeNoTitle opts "-" <$> proposals),
          prettyItemizeNonEmpty opts "Withdrawals:" "-" (mkWithdrawal <$> Map.toList withdrawals),
          prettyItemizeNonEmpty opts "Options:" "-" txopts
        ]

data Withdrawal = Withdrawal (Either (Script.Versioned Script.Script) Api.PubKeyHash) TxSkelRedeemer Api.Lovelace

mkWithdrawal :: (Either (Script.Versioned Script.Script) Api.PubKeyHash, (TxSkelRedeemer, Api.Lovelace)) -> Withdrawal
mkWithdrawal (owner, (red, lv)) = Withdrawal owner red lv

instance PrettyCooked Withdrawal where
  prettyCookedOpt opts (Withdrawal cred red ada) =
    prettyItemizeNoTitle opts "-" $
      ( case cred of
          Left script -> prettyHash opts script : prettyCookedOptL opts red
          Right pkh -> [prettyHash opts pkh]
      )
        ++ [prettyCookedOpt opts (Script.toValue ada)]

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
      opts
      "Prices:"
      "-"
      [ "Memory cost:" <+> prettyCookedOpt opts q,
        "Step cost:" <+> prettyCookedOpt opts r
      ]
  prettyCookedOpt opts (MaxTxExUnits n m) =
    prettyItemize
      opts
      "Max transaction execution units:"
      "-"
      [ "Max memory:" <+> prettyCookedOpt opts n,
        "Max steps:" <+> prettyCookedOpt opts m
      ]
  prettyCookedOpt opts (MaxBlockExUnits n m) =
    prettyItemize
      opts
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
      opts
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
      opts
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
instance PrettyCookedL TxSkelRedeemer where
  prettyCookedOptLM opts (TxSkelRedeemer red mRefScript _) =
    [ Just $ "Redeemer" <+> prettyCookedOpt opts red,
      ("Reference script at:" <+>) . prettyCookedOpt opts <$> mRefScript
    ]

instance PrettyCookedL TxSkelProposal where
  prettyCookedOptLM opts TxSkelProposal {..} =
    [ Just $ "Governance action:" <+> prettyCookedOpt opts txSkelProposalAction,
      Just $ "Return address:" <+> prettyCooked txSkelProposalAddress,
      (\(script, redeemer) -> prettyItemize opts "Witness:" "-" (prettyHash opts script : prettyCookedOptL opts redeemer)) <$> txSkelProposalWitness,
      ("Anchor:" <+>) . PP.pretty <$> txSkelProposalAnchor
    ]

instance PrettyCooked TxGovAction where
  prettyCookedOpt opts (TxGovActionParameterChange params) = prettyItemize opts "Parameter changes:" "-" params
  prettyCookedOpt opts (TxGovActionHardForkInitiation (Api.ProtocolVersion major minor)) =
    "Protocol version:" <+> "(" <+> prettyCookedOpt opts major <+> "," <+> prettyCookedOpt opts minor <+> ")"
  prettyCookedOpt opts (TxGovActionTreasuryWithdrawals withdrawals) =
    prettyItemize opts "Withdrawals:" "-" $
      (\(cred, lv) -> prettyCookedOpt opts cred <+> "|" <+> prettyCooked (Script.toValue lv)) <$> Map.toList withdrawals
  prettyCookedOpt _ TxGovActionNoConfidence = "No confidence"
  prettyCookedOpt opts (TxGovActionUpdateCommittee toRemoveCreds toAddCreds quorum) =
    prettyItemize
      opts
      "Updates in committee:"
      "-"
      [ prettyItemize opts "Credentials to remove:" "-" $
          (\(Api.ColdCommitteeCredential cred) -> prettyCookedOpt opts cred) <$> toRemoveCreds,
        prettyItemize opts "Credentials to add:" "-" $
          (\(Api.ColdCommitteeCredential cred, i) -> prettyCookedOpt opts cred <+> "->" <+> prettyCookedOpt opts i) <$> Map.toList toAddCreds,
        "Quorum:" <+> prettyCookedOpt opts (Api.toGHC quorum)
      ]
  prettyCookedOpt opts (TxGovActionNewConstitution (Api.Constitution mScriptHash)) = case mScriptHash of
    Nothing -> "Empty new constitution"
    Just sHash -> "New constitution:" <+> prettyHash opts sHash

-- | Prints a list of pubkeys with a flag next to the balancing wallet
instance PrettyCookedL (TxOpts, [Wallet]) where
  prettyCookedOptL opts (TxOpts {txOptBalancingPolicy = DoNotBalance}, signers) = prettyCookedOptL opts signers
  prettyCookedOptL opts (TxOpts {txOptBalancingPolicy = BalanceWithFirstSigner}, firstSigner : signers) =
    prettyCookedOpt opts firstSigner <+> "[balancing]" : prettyCookedOptL opts signers
  prettyCookedOptL opts (TxOpts {txOptBalancingPolicy = BalanceWith balancingWallet}, signers) =
    (\s -> if s == balancingWallet then prettyCookedOpt opts s <+> "[balancing]" else prettyCookedOpt opts s) <$> signers
  -- The following case should never happen for real transactions
  prettyCookedOptL _ (_, []) = []

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
    prettyItemize opts (prettyHash opts (Script.toVersioned @Script.MintingPolicy pol)) "-" $
      prettyCookedOptL opts red ++ ((\(tk, n) -> PP.viaShow tk <> ":" <+> PP.viaShow n) <$> tks)

instance PrettyCooked TxSkelOut where
  prettyCookedOpt opts (Pays output) =
    prettyItemize
      opts
      ("Pays to" <+> prettyCookedOpt opts (outputAddress output))
      "-"
      ( prettyCookedOpt opts (outputValue output)
          : catMaybes
            [ prettyCookedOptM opts (output ^. outputDatumL),
              ("Reference script hash:" <+>) . prettyHash opts . Script.toScriptHash <$> output ^. outputReferenceScriptL
            ]
      )

-- | Prints a 'TxSkelOutDatum' when different from 'TxSkelOutNoDatum'
instance PrettyCookedM TxSkelOutDatum where
  prettyCookedOptM _ TxSkelOutNoDatum = Nothing
  prettyCookedOptM opts (TxSkelOutInlineDatum dat) =
    Just $
      "Datum (inlined):"
        <+> PP.align (prettyCookedOpt opts dat)
  prettyCookedOptM opts (TxSkelOutDatumHash dat) =
    Just $
      "Datum (hashed, invisible)"
        <+> "("
        <> prettyHash opts (Script.datumHash $ Api.Datum $ Api.toBuiltinData dat)
        <> "):"
        <+> PP.align (prettyCookedOpt opts dat)
  prettyCookedOptM opts (TxSkelOutDatum dat) =
    Just $
      "Datum (hashed, visible)"
        <+> "("
        <> prettyHash opts (Script.datumHash $ Api.Datum $ Api.toBuiltinData dat)
        <> "):"
        <+> PP.align (prettyCookedOpt opts dat)

-- | Pretty-print a list of transaction skeleton options, only printing an
-- option if its value is non-default.
instance PrettyCookedL TxOpts where
  prettyCookedOptLM
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
        prettyBalancingPolicy (BalanceWith w) = "Balance with" <+> prettyCookedOpt opts w
        prettyBalancingPolicy DoNotBalance = "Do not balance"
        prettyUnsafeModTx :: [RawModTx] -> DocCooked
        prettyUnsafeModTx [] = "No transaction modifications"
        prettyUnsafeModTx (length -> n) = prettyCookedOpt opts n <+> "transaction" <+> PP.plural "modification" "modifications" n
        prettyEmulatorParamsModification :: Maybe EmulatorParamsModification -> DocCooked
        prettyEmulatorParamsModification Nothing = "No modifications of protocol paramters"
        prettyEmulatorParamsModification Just {} = "With modifications of protocol parameters"
        prettyCollateralUtxos :: CollateralUtxos -> DocCooked
        prettyCollateralUtxos CollateralUtxosFromBalancingWallet =
          prettyItemize
            opts
            "Collateral policy:"
            "-"
            [ "Use value-only utxos from balancing wallet" :: DocCooked,
              "Send return collaterals to balancing wallet"
            ]
        prettyCollateralUtxos (CollateralUtxosFromWallet w)
          | prettyWallet <- prettyCookedOpt opts w =
              prettyItemize
                opts
                "Collateral policy:"
                "-"
                [ "Use value-only utxos from" <+> prettyWallet,
                  "Send return collaterals to" <+> prettyWallet
                ]
        prettyCollateralUtxos (CollateralUtxosFromSet txOutRefs w) =
          prettyItemize
            opts
            "Collateral policy:"
            "-"
            [ prettyItemize opts "Choose among the following TxOutRefs:" "-" txOutRefs,
              "Send return collaterals to" <+> prettyCookedOpt opts w
            ]
        prettyBalancingUtxos :: BalancingUtxos -> DocCooked
        prettyBalancingUtxos BalancingUtxosFromBalancingWallet = "Balance with 'only value' utxos from the balancing wallet"
        prettyBalancingUtxos (BalancingUtxosFromSet utxos) = prettyItemize opts "Balance with the following utxos:" "-" utxos
        prettyBalanceFeePolicy :: FeePolicy -> DocCooked
        prettyBalanceFeePolicy AutoFeeComputation = "Use automatically computed fee"
        prettyBalanceFeePolicy (ManualFee fee) = "Use the following fee:" <+> prettyCookedOpt opts fee
        prettyAnchorResolution :: AnchorResolution -> DocCooked
        prettyAnchorResolution AnchorResolutionHttp = "Resolve anchor url with an (unsafe) http connection"
        prettyAnchorResolution (AnchorResolutionLocal urlMap) = prettyItemize @[DocCooked] opts "Resolve anchor url with the following table keys" "-" (PP.viaShow <$> Map.keys urlMap)

-- | Resolves a "TxOutRef" from a given context, builds a doc cooked for its
-- address and value, and also builds a possibly empty list for its datum and
-- reference script when they exist.
instance PrettyCookedL (Contextualized Api.TxOutRef) where
  prettyCookedOptL opts (Contextualized managedTxOuts managedTxSkelOutDatums txOutRef) = fromMaybe [] $ do
    output <- Map.lookup txOutRef managedTxOuts
    txSkelOutDatum <- case outputOutputDatum output of
      Api.OutputDatum datum -> Map.lookup (Script.datumHash datum) managedTxSkelOutDatums
      Api.OutputDatumHash datumHash -> Map.lookup datumHash managedTxSkelOutDatums
      Api.NoOutputDatum -> return TxSkelOutNoDatum
    return
      ( prettyCookedOpt opts (outputAddress output)
          : prettyCookedOpt opts (outputValue output)
          : catMaybes
            [ prettyCookedOptM opts txSkelOutDatum,
              ("Reference script hash:" <+>) . prettyHash opts . Script.toScriptHash <$> output ^. outputReferenceScriptL
            ]
      )

newtype CollateralInput = CollateralInput {unCollateralInput :: Api.TxOutRef}

instance PrettyCooked (Contextualized CollateralInput) where
  prettyCookedOpt opts cColIn@(Contextualized _ _ (CollateralInput txOutRef)) =
    case prettyCookedOptL opts (unCollateralInput <$> cColIn) of
      (addressDoc : valueDoc : otherDocs) ->
        prettyItemize opts ("Belonging to" <+> addressDoc) "-" (valueDoc : otherDocs)
      _ -> "Uses" <+> prettyCookedOpt opts txOutRef <+> "(non resolved)"

data Input = Input
  { inputORef :: Api.TxOutRef,
    inputRed :: TxSkelRedeemer
  }

instance PrettyCooked (Contextualized Input) where
  prettyCookedOpt opts cIn@(Contextualized _ _ input) =
    case prettyCookedOptL opts (inputORef <$> cIn) of
      (addressDoc : valueDoc : otherDocs) ->
        prettyItemize opts ("Spends from" <+> addressDoc) "-" (valueDoc : prettyCookedOptL opts (inputRed input) <> otherDocs)
      _ -> "Spends" <+> prettyCookedOpt opts (inputORef input) <+> "(non resolved)"

newtype ReferenceInput = ReferenceInput {unReferenceInput :: Api.TxOutRef}

instance PrettyCooked (Contextualized ReferenceInput) where
  prettyCookedOpt opts cRefIn@(Contextualized _ _ (ReferenceInput txOutRef)) =
    case prettyCookedOptL opts (unReferenceInput <$> cRefIn) of
      (addressDoc : valueDoc : otherDocs) ->
        prettyItemize opts ("References output from" <+> addressDoc) "-" (valueDoc : otherDocs)
      _ -> "References" <+> prettyCookedOpt opts txOutRef <+> "(non resolved)"
