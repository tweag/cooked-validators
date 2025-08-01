{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module implements 'PrettyCooked', 'PrettyCookedList' and
-- 'PrettyCookedMaybe' instances for 'TxSkel's and its components.
module Cooked.Pretty.Skeleton (Contextualized (..)) where

import Cooked.Pretty.Class
import Cooked.Pretty.Options
import Cooked.Pretty.Plutus ()
import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Ledger.Slot qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

instance PrettyCooked Wallet where
  prettyCookedOpt opts = prettyHash opts . Script.toPubKeyHash

-- | Some elements of a skeleton can only be printed when they are associated
-- with a context. This is typically the case for elements that need some
-- 'Api.TxOutRef's and datums to be resolved.
data Contextualized a = Contextualized
  { _ctxOutputs :: Map Api.TxOutRef (TxSkelOut, Bool),
    ctxContent :: a
  }
  deriving (Functor)

-- | Prints a 'Contextualized' 'TxSkel'
instance PrettyCookedList (Contextualized TxSkel) where
  prettyCookedOptListMaybe opts cTxSkel
    | TxSkel lbl txopts mints signers validityRange ins insReference outs proposals withdrawals certificates <- ctxContent cTxSkel =
        [ prettyItemizeNonEmpty opts "Labels:" "-" lbl,
          prettyItemizeNonEmpty opts "Mints:" "-" (view txSkelMintsListI mints),
          Just $ "Validity interval:" <+> PP.pretty validityRange,
          prettyItemizeNonEmpty opts "Signers:" "-" (txopts, signers),
          prettyItemizeNonEmpty opts "Inputs:" "-" ((<$ cTxSkel) . uncurry Input <$> Map.toList ins),
          prettyItemizeNonEmpty opts "Reference inputs:" "-" $ prettyCookedOpt opts . (<$ cTxSkel) . ReferenceInput <$> Set.toList insReference,
          prettyItemizeNonEmpty opts "Outputs:" "-" (prettyCookedOpt opts <$> outs),
          prettyItemizeNonEmpty opts "Proposals:" "-" (prettyItemizeNoTitle opts "-" <$> proposals),
          prettyItemizeNonEmpty opts "Withdrawals:" "-" $ view txSkelWithdrawalsListI withdrawals,
          prettyItemizeNonEmpty opts "Certificates:" "-" certificates,
          prettyItemizeNonEmpty opts "Options:" "-" txopts
        ]

instance PrettyCooked TxSkelCertificate where
  prettyCookedOpt opts (TxSkelCertificate owner action) =
    prettyItemize
      opts
      (prettyCookedOpt opts action)
      "-"
      $ prettyCookedList owner

instance PrettyCookedList (User req mode) where
  prettyCookedOptListMaybe opt (UserPubKeyHash (Script.toPubKeyHash -> pkh)) = [Just ("User" <+> prettyHash opt pkh)]
  prettyCookedOptListMaybe opt (UserScript (toVScript -> vScript)) = [Just ("Script" <+> prettyHash opt vScript)]
  prettyCookedOptListMaybe opt (UserRedeemedScript (toVScript -> script) red) =
    Just (prettyHash opt script) : prettyCookedOptListMaybe opt red

instance PrettyCooked (CertificateAction req) where
  prettyCookedOpt _ StakingRegister = "Register staking"
  prettyCookedOpt _ StakingUnRegister = "Unregister staking"
  prettyCookedOpt opt (StakingDelegate deleg) = "Delegate staking to" <+> prettyCookedOpt opt deleg
  prettyCookedOpt opt (StakingRegisterDelegate deleg) = "Register staking and delegate it to" <+> prettyCookedOpt opt deleg
  prettyCookedOpt _ DRepRegister = "Register DRep"
  prettyCookedOpt _ DRepUpdate = "Update DRep"
  prettyCookedOpt _ DRepUnRegister = "Unregister DRep"
  prettyCookedOpt opt (PoolRegister poolVfr) = "Register pool" <+> prettyHash opt poolVfr
  prettyCookedOpt _ (PoolRetire (Ledger.Slot n)) = "Retire pool at slot" <+> PP.pretty n
  prettyCookedOpt opt (CommitteeRegisterHot cred) = "Register hot credential" <+> prettyCookedOpt opt cred
  prettyCookedOpt _ CommitteeResign = "Resign committee"

instance PrettyCooked Api.Delegatee where
  prettyCookedOpt opt (Api.DelegStake pkh) = "Delegate stake to" <+> prettyHash opt pkh
  prettyCookedOpt opt (Api.DelegVote dRep) = "Delegate vote to" <+> prettyCookedOpt opt dRep
  prettyCookedOpt opt (Api.DelegStakeVote pkh dRep) = "Delegate stake to" <+> prettyHash opt pkh <+> "and delegate vote to" <+> prettyCookedOpt opt dRep

instance PrettyCooked Api.DRep where
  prettyCookedOpt _ Api.DRepAlwaysAbstain = "Always abstain"
  prettyCookedOpt _ Api.DRepAlwaysNoConfidence = "Always no confidence"
  prettyCookedOpt opt (Api.DRep (Api.DRepCredential cred)) = prettyCookedOpt opt cred

instance PrettyCooked Withdrawal where
  prettyCookedOpt opts (Withdrawal (UserRedeemedScript (toVScript -> vScript) red) lv) =
    prettyItemize opts (prettyHash opts vScript) "-" $ prettyCookedOptList opts red ++ [prettyCookedOpt opts (Script.toValue lv)]
  prettyCookedOpt opts (Withdrawal (UserPubKeyHash (Script.toPubKeyHash -> pkh)) lv) =
    prettyItemize opts (prettyHash opts pkh) "-" [prettyCookedOpt opts (Script.toValue lv)]

instance PrettyCooked ParameterChange where
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
  prettyCookedOpt opts (MinFeeRefScriptCostPerByte q) = "Min fee per byto of reference script:" <+> prettyCookedOpt opts q

instance PrettyCookedList TxSkelRedeemer where
  prettyCookedOptListMaybe opts (TxSkelRedeemer red mRefScript _) =
    [ Just $ "Redeemer" <+> prettyCookedOpt opts red,
      ("Reference script at:" <+>) . prettyCookedOpt opts <$> mRefScript
    ]

instance PrettyCookedList TxSkelProposal where
  prettyCookedOptListMaybe opts txSkelProposal =
    [ Just $ "Return credential:" <+> prettyCookedOpt opts (view txSkelProposalReturnCredentialL txSkelProposal),
      ("Witnessed governance action:" <+>) . prettyCookedOpt opts <$> preview (txSkelProposalGovActionAT @ReqScript) txSkelProposal,
      ("Other governance action:" <+>) . prettyCookedOpt opts <$> preview (txSkelProposalGovActionAT @ReqNone) txSkelProposal,
      ("Constitution witness:" <+>) . prettyHash opts <$> preview (txSkelProposalMConstitutionAT % _Just % userVScriptL) txSkelProposal
    ]
      ++ maybe [] (prettyCookedOptListMaybe opts) (preview (txSkelProposalMConstitutionAT % _Just % userTxSkelRedeemerL) txSkelProposal)

instance PrettyCooked (TxSkelGovAction a) where
  prettyCookedOpt opts (ParameterChange params) = prettyItemize opts "Parameter changes:" "-" params
  prettyCookedOpt opts (HardForkInitiation (Api.ProtocolVersion major minor)) =
    "Protocol version:" <+> "(" <+> prettyCookedOpt opts major <+> "," <+> prettyCookedOpt opts minor <+> ")"
  prettyCookedOpt opts (TreasuryWithdrawals withdrawals) =
    prettyItemize opts "Withdrawals:" "-" $
      (\(cred, lv) -> prettyCookedOpt opts cred <+> "|" <+> prettyCooked (Script.toValue lv)) <$> Map.toList withdrawals
  prettyCookedOpt _ NoConfidence = "No confidence"
  prettyCookedOpt opts (UpdateCommittee toRemoveCreds toAddCreds quorum) =
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
  prettyCookedOpt opts (NewConstitution (Api.Constitution mScriptHash)) = case mScriptHash of
    Nothing -> "Empty new constitution"
    Just sHash -> "New constitution:" <+> prettyHash opts sHash

-- | Prints a list of pubkeys with a flag next to the balancing wallet
instance PrettyCookedList (TxSkelOpts, [Wallet]) where
  prettyCookedOptList opts (TxSkelOpts {txSkelOptBalancingPolicy = DoNotBalance}, signers) = prettyCookedOptList opts signers
  prettyCookedOptList opts (TxSkelOpts {txSkelOptBalancingPolicy = BalanceWithFirstSigner}, firstSigner : signers) =
    prettyCookedOpt opts firstSigner <+> "[balancing]" : prettyCookedOptList opts signers
  prettyCookedOptList opts (TxSkelOpts {txSkelOptBalancingPolicy = BalanceWith balancingWallet}, signers) =
    (\s -> if s == balancingWallet then prettyCookedOpt opts s <+> "[balancing]" else prettyCookedOpt opts s) <$> signers
  -- The following case should never happen for real transactions
  prettyCookedOptList _ (_, []) = []

-- | Prints a minting specification
--
-- Example:
-- > #abcdef
--     - Redeemer: red
--     - Reference script at: txOutRef
--     - "Foo": 500
--     - "Bar": 1000
instance PrettyCooked Mint where
  prettyCookedOpt opts (Mint (UserRedeemedScript pol red) tks) =
    prettyItemize opts (prettyHash opts (toVScript pol)) "-" $
      prettyCookedOptList opts red ++ ((\(tk, n) -> PP.viaShow tk <> ":" <+> PP.viaShow n) <$> tks)

instance PrettyCookedList TxSkelOut where
  prettyCookedOptList opts output =
    [ prettyCookedOpt opts (view txSkelOutAddressG output),
      prettyCookedOpt opts (view txSkelOutValueL output)
    ]
      ++ catMaybes
        [ prettyCookedOptMaybe opts (output ^. txSkelOutDatumL),
          ("Reference script hash:" <+>) . prettyHash opts <$> preview (txSkelOutReferenceScriptL % txSkelOutReferenceScriptHashAF) output
        ]

instance PrettyCooked TxSkelOut where
  prettyCookedOpt opts output =
    let txSkelOutList = prettyCookedOptList opts output
     in prettyItemize opts ("Pays to" <+> head txSkelOutList) "-" (tail txSkelOutList)

-- | Prints a 'TxSkelOutDatum' when different from 'NoTxSkelOutDatum'
instance PrettyCookedMaybe TxSkelOutDatum where
  prettyCookedOptMaybe _ NoTxSkelOutDatum = Nothing
  prettyCookedOptMaybe opts (SomeTxSkelOutDatum dat Inline) =
    Just $
      "Datum (inline)"
        <+> "("
        <> prettyHash opts (Api.toBuiltinData dat)
        <> "):"
        <+> PP.align (prettyCookedOpt opts dat)
  prettyCookedOptMaybe opts (SomeTxSkelOutDatum dat (Hashed NotResolved)) =
    Just $
      "Datum (hashed, hidden)"
        <+> "("
        <> prettyHash opts (Api.toBuiltinData dat)
        <> "):"
        <+> PP.align (prettyCookedOpt opts dat)
  prettyCookedOptMaybe opts (SomeTxSkelOutDatum dat (Hashed Resolved)) =
    Just $
      "Datum (hashed, visible)"
        <+> "("
        <> prettyHash opts (Api.toBuiltinData dat)
        <> "):"
        <+> PP.align (prettyCookedOpt opts dat)

-- | Pretty-print a list of transaction skeleton options, only printing an
-- option if its value is non-default.
instance PrettyCookedList TxSkelOpts where
  prettyCookedOptListMaybe
    opts
    ( TxSkelOpts
        txSkelOptAutoSlotIncrease
        _
        txSkelOptBalancingPolicy
        txSkelOptFeePolicy
        txSkelOptBalanceOutputPolicy
        txSkelOptBalancingUtxos
        _
        txSkelOptCollateralUtxos
      ) =
      [ prettyIfNot True prettyAutoSlotIncrease txSkelOptAutoSlotIncrease,
        prettyIfNot def prettyBalanceOutputPolicy txSkelOptBalanceOutputPolicy,
        prettyIfNot def prettyBalanceFeePolicy txSkelOptFeePolicy,
        prettyIfNot def prettyBalancingPolicy txSkelOptBalancingPolicy,
        prettyIfNot def prettyBalancingUtxos txSkelOptBalancingUtxos,
        prettyIfNot def prettyCollateralUtxos txSkelOptCollateralUtxos
      ]
      where
        prettyIfNot :: (Eq a) => a -> (a -> DocCooked) -> a -> Maybe DocCooked
        prettyIfNot defaultValue f x
          | x == defaultValue && not (pcOptPrintDefaultTxSkelOpts opts) = Nothing
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

-- | Resolves a "TxOutRef" from a given context, builds a doc cooked for its
-- address and value, and also builds a possibly empty list for its datum and
-- reference script when they exist.
instance PrettyCookedList (Contextualized Api.TxOutRef) where
  prettyCookedOptList opts (Contextualized managedTxOuts txOutRef) =
    maybe [] (prettyCookedOptList opts . fst) (Map.lookup txOutRef managedTxOuts)

data Input = Input
  { inputORef :: Api.TxOutRef,
    inputRed :: TxSkelRedeemer
  }

instance PrettyCooked (Contextualized Input) where
  prettyCookedOpt opts cIn@(Contextualized _ input) =
    case prettyCookedOptList opts (inputORef <$> cIn) of
      (addressDoc : otherDocs) ->
        prettyItemize
          opts
          ( "Spends"
              <+> prettyCookedOpt opts (inputORef input)
              <+> "from"
              <+> addressDoc
          )
          "-"
          (prettyCookedOptList opts (inputRed input) <> otherDocs)
      _ -> "Spends" <+> prettyCookedOpt opts (inputORef input) <+> "(non resolved)"

newtype ReferenceInput = ReferenceInput {unReferenceInput :: Api.TxOutRef}

instance PrettyCooked (Contextualized ReferenceInput) where
  prettyCookedOpt opts cRefIn@(Contextualized _ (ReferenceInput txOutRef)) =
    case prettyCookedOptList opts (unReferenceInput <$> cRefIn) of
      (addressDoc : otherDocs) ->
        prettyItemize
          opts
          ( "References output"
              <+> prettyCookedOpt opts txOutRef
              <+> "from"
              <+> addressDoc
          )
          "-"
          otherDocs
      _ -> "References" <+> prettyCookedOpt opts txOutRef <+> "(non resolved)"
