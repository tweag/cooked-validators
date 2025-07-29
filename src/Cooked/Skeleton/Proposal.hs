{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module exposes the notion of proposal within a
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Proposal
  ( ParameterChange (..),
    TxSkelGovAction (..),
    TxSkelProposal (..),
    txSkelProposalAnchorL,
    txSkelProposalMConstitutionAT,
    txSkelProposalReturnCredentialL,
    txSkelProposalGovActionAT,
    simpleTxSkelProposal,
    autoFillConstitution,
  )
where

import Cooked.Skeleton.Anchor
import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.Scripts
import Data.Kind (Type)
import Data.Map (Map)
import Data.Typeable
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- | These are all the protocol parameters. They are taken from
-- https://github.com/IntersectMBO/cardano-ledger/blob/c4fbc05999866fea7c0cb1b211fd5288f286b95d/eras/conway/impl/cddl-files/conway.cddl#L381-L412
-- and will most likely change in future eras.
data ParameterChange where
  -- | The linear factor for the minimum fee calculation
  FeePerByte :: Integer -> ParameterChange
  -- | The constant factor for the minimum fee calculation
  FeeFixed :: Integer -> ParameterChange
  -- | Maximal block body size
  MaxBlockBodySize :: Integer -> ParameterChange
  -- | Maximal transaction size
  MaxTxSize :: Integer -> ParameterChange
  -- | Maximal block header size
  MaxBlockHeaderSize :: Integer -> ParameterChange
  -- | The amount of a key registration deposit
  KeyDeposit :: Integer -> ParameterChange
  -- | The amount of a pool registration deposit
  PoolDeposit :: Integer -> ParameterChange
  -- | Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled future for.
  PoolRetirementMaxEpoch :: Integer -> ParameterChange
  -- | Desired number of pools
  PoolNumber :: Integer -> ParameterChange
  -- | Pool influence
  PoolInfluence :: Rational -> ParameterChange
  -- | Monetary expansion
  MonetaryExpansion :: Rational -> ParameterChange
  -- | Treasury expansion
  TreasuryCut :: Rational -> ParameterChange
  -- | Minimum Stake Pool Cost
  MinPoolCost :: Integer -> ParameterChange
  -- | Cost in lovelace per byte of UTxO storage
  CoinsPerUTxOByte :: Integer -> ParameterChange
  -- | Cost models for non-native script languages
  CostModels ::
    { cmPlutusV1Costs :: [Integer],
      cmPlutusV2Costs :: [Integer],
      cmPlutusV3Costs :: [Integer]
    } ->
    ParameterChange
  -- | Prices of execution units
  Prices ::
    { pMemoryCost :: Rational,
      pStepCost :: Rational
    } ->
    ParameterChange
  -- | Max total script execution resources units allowed per tx
  MaxTxExUnits ::
    { mteuMemory :: Integer,
      mteuSteps :: Integer
    } ->
    ParameterChange
  -- | Max total script execution resources units allowed per block
  MaxBlockExUnits ::
    { mbeuMemory :: Integer,
      mbeuSteps :: Integer
    } ->
    ParameterChange
  -- | Max size of a Value in an output
  MaxValSize :: Integer -> ParameterChange
  -- | Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  CollateralPercentage :: Integer -> ParameterChange
  -- | Maximum number of collateral inputs allowed in a transaction
  MaxCollateralInputs :: Integer -> ParameterChange
  -- | Thresholds for pool votes
  PoolVotingThresholds ::
    { pvtMotionNoConfidence :: Rational,
      pvtCommitteeNormal :: Rational,
      pvtCommitteeNoConfidence :: Rational,
      pvtHardFork :: Rational,
      pvtSecurityGroup :: Rational
    } ->
    ParameterChange
  -- | Thresholds for DRep votes
  DRepVotingThresholds ::
    { drvtMotionNoConfidence :: Rational,
      drvtCommitteeNormal :: Rational,
      drvtCommitteeNoConfidence :: Rational,
      drvtUpdateConstitution :: Rational,
      drvtHardForkInitialization :: Rational,
      drvtNetworkGroup :: Rational,
      drvtEconomicGroup :: Rational,
      drvtTechnicalGroup :: Rational,
      drvtGovernanceGroup :: Rational,
      drvtTreasuryWithdrawal :: Rational
    } ->
    ParameterChange
  -- | Minimum size of the Constitutional Committee
  CommitteeMinSize :: Integer -> ParameterChange
  -- | The Constitutional Committee Term limit in number of Slots
  CommitteeMaxTermLength :: Integer -> ParameterChange
  -- | Gov action lifetime in number of Epochs
  GovActionLifetime :: Integer -> ParameterChange
  -- | The amount of the Gov Action deposit
  GovActionDeposit :: Integer -> ParameterChange
  -- | The amount of a DRep registration deposit
  DRepRegistrationDeposit :: Integer -> ParameterChange
  -- | The number of Epochs that a DRep can perform no activity without losing
  -- their @Active@ status.
  DRepActivity :: Integer -> ParameterChange
  -- Reference scripts fee for the minimum fee calculation
  MinFeeRefScriptCostPerByte :: Rational -> ParameterChange
  deriving (Show, Eq)

-- | This lists the various possible governance actions. Only two of these
-- action need to be witnessed by the constitution script, while the other do
-- not need any witness.
data TxSkelGovAction :: Maybe UserKind -> Type where
  -- If several parameter changes are of the same kind, only the last
  -- one will take effect
  ParameterChange :: [ParameterChange] -> TxSkelGovAction (Just IsScript)
  TreasuryWithdrawals :: Map Api.Credential Api.Lovelace -> TxSkelGovAction (Just IsScript)
  HardForkInitiation :: Api.ProtocolVersion -> TxSkelGovAction Nothing
  NoConfidence :: TxSkelGovAction Nothing
  UpdateCommittee :: [Api.ColdCommitteeCredential] -> Map Api.ColdCommitteeCredential Integer -> PlutusTx.Rational -> TxSkelGovAction Nothing
  NewConstitution :: Api.Constitution -> TxSkelGovAction Nothing

deriving instance Show (TxSkelGovAction a)

deriving instance Eq (TxSkelGovAction a)

-- | This bundles a governance action into an actual proposal
data TxSkelProposal where
  TxSkelProposal ::
    ( Typeable req,
      Typeable (FromJust req),
      Script.ToCredential cred
    ) =>
    { -- | The credential that should be used for a return account
      txSkelProposalReturnCredential :: cred,
      -- | The proposed action. It can either be a "free" action, which does not
      -- need to be witnessed by the constitution, or a witnessed action. The
      -- latter requires the constitution script, which can either be given
      -- manually, or left for cooekd to be filled out automatically based on
      -- the current official one.
      txSkelProposalGovAction :: TxSkelGovAction req,
      -- | The constitution witness of this proposal
      txSkelProposalConstitution :: Maybe (User (FromJust req) Redemption),
      -- | An optional anchor to be given as additional data. It should
      -- correspond to the URL of a web page
      txSkelProposalAnchor :: TxSkelAnchor
    } ->
    TxSkelProposal

instance Show TxSkelProposal where
  show (TxSkelProposal (Script.toCredential -> cred) action constit anchor) = show [show cred, show action, show constit, show anchor]

instance Eq TxSkelProposal where
  (TxSkelProposal (Script.toCredential -> cred) action constit anchor) == (TxSkelProposal (Script.toCredential -> cred') action' constit' anchor') =
    cred == cred' && cast action == Just action' && cast constit == Just constit' && anchor == anchor'

-- | Focuses on the return credential from a 'TxSkelProposal'
txSkelProposalReturnCredentialL :: Lens' TxSkelProposal Api.Credential
txSkelProposalReturnCredentialL =
  lens
    (\(TxSkelProposal {txSkelProposalReturnCredential}) -> Script.toCredential txSkelProposalReturnCredential)
    (\txSkelProposal cred -> txSkelProposal {txSkelProposalReturnCredential = cred})

-- | Focuses on the optional constitution of a 'TxSkelProposal'
txSkelProposalMConstitutionAT :: AffineTraversal' TxSkelProposal (Maybe (User IsScript Redemption))
txSkelProposalMConstitutionAT =
  atraversal
    (\prop@(TxSkelProposal {txSkelProposalConstitution}) -> maybe (Left prop) Right $ cast txSkelProposalConstitution)
    (\prop@(TxSkelProposal cred action _ anchor) -> maybe prop (\constit -> TxSkelProposal cred action constit anchor) . cast)

-- | Focuses on the governance action of a 'TxSkelProposal'
txSkelProposalGovActionAT :: forall req. (Typeable req) => AffineTraversal' TxSkelProposal (TxSkelGovAction req)
txSkelProposalGovActionAT =
  atraversal
    (\prop@(TxSkelProposal {txSkelProposalGovAction}) -> maybe (Left prop) Right $ cast txSkelProposalGovAction)
    (\prop@(TxSkelProposal @req' cred _ constit anchor) newAction -> maybe prop (\Refl -> TxSkelProposal cred newAction constit anchor) $ eqT @req @req')

-- | A lens to get or set the anchor of a 'TxSkelProposal'
makeLensesFor [("txSkelProposalAnchor", "txSkelProposalAnchorL")] ''TxSkelProposal

-- | Builds a 'TxSkelProposal' from a credential and governance action. This
-- defaults the constitution and anchors to empty values. If the constitution is
-- required by the governance action, it will be automatically filled during
-- transaction generation if an official constitution script exists.
simpleTxSkelProposal :: (Script.ToCredential cred, Typeable req, Typeable (FromJust req)) => cred -> TxSkelGovAction req -> TxSkelProposal
simpleTxSkelProposal cred action = TxSkelProposal cred action Nothing Nothing

-- | Sets the constitution script with an empty redeemer when empty. This will
-- not tamper with an existing constitution script and redeemer.
autoFillConstitution :: (ToVScript script) => script -> TxSkelProposal -> TxSkelProposal
autoFillConstitution constitution = over txSkelProposalMConstitutionAT (maybe (Just $ UserRedeemedScript constitution emptyTxSkelRedeemer) Just)
