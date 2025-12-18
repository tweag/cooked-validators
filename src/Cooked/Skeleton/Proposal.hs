-- | This module exposes the proposals constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities. To issue proposals
-- in a skeleton, the usual way is to invoke @txSkelProposals = [simpleProposal
-- script govAction1, simpleProposal pk govAction2, ... ]@
module Cooked.Skeleton.Proposal
  ( -- * Data types
    ParameterChange (..),
    GovernanceAction (..),
    TxSkelProposal (..),

    -- * Optics
    txSkelProposalAnchorL,
    txSkelProposalMConstitutionAT,
    txSkelProposalReturnCredentialL,
    txSkelProposalGovernanceActionAT,
    txSkelProposalConstitutionAT,

    -- * Smart constructors
    simpleProposal,

    -- * Utilities
    fillConstitution,
  )
where

import Cooked.Skeleton.Anchor
import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.User
import Data.Kind (Type)
import Data.Map (Map)
import Data.Typeable
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- * Proposals for 'Cooked.Skeleton.TxSkel'

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
  -- | Reference scripts fee for the minimum fee calculation
  MinFeeRefScriptCostPerByte :: Rational -> ParameterChange
  deriving (Show, Eq)

-- | This lists the various possible governance actions. Only two of these
-- action need to be witnessed by the constitution script, which are annotated
-- by 'IsScript'.
data GovernanceAction :: UserKind -> Type where
  -- If several parameter changes are of the same kind, only the last
  -- one will take effect
  ParameterChange :: [ParameterChange] -> GovernanceAction IsScript
  TreasuryWithdrawals :: Map Api.Credential Api.Lovelace -> GovernanceAction IsScript
  HardForkInitiation :: Api.ProtocolVersion -> GovernanceAction IsNone
  NoConfidence :: GovernanceAction IsNone
  UpdateCommittee :: [Api.ColdCommitteeCredential] -> Map Api.ColdCommitteeCredential Integer -> PlutusTx.Rational -> GovernanceAction IsNone
  NewConstitution :: Api.Constitution -> GovernanceAction IsNone

deriving instance Show (GovernanceAction a)

deriving instance Eq (GovernanceAction a)

-- | This bundles a governance action into an actual proposal
data TxSkelProposal where
  TxSkelProposal ::
    ( Typeable kind,
      Script.ToCredential cred
    ) =>
    { -- | The credential that should be used for a return account
      txSkelProposalReturnCredential :: cred,
      -- | The proposed action gov action, either witnessed or simple
      txSkelProposalGovernanceAction :: GovernanceAction kind,
      -- | The constitution witness of this proposal, when paired with a
      -- witnessed governance action. Is the governance action is simple,
      -- only 'Nothing' can be provided there.
      txSkelProposalConstitution :: Maybe (User kind Redemption),
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

-- * Optics on 'TxSkelProposal'

-- | Focuses on the return credential from a 'TxSkelProposal'
txSkelProposalReturnCredentialL :: Lens' TxSkelProposal Api.Credential
txSkelProposalReturnCredentialL =
  lens
    (\(TxSkelProposal {txSkelProposalReturnCredential}) -> Script.toCredential txSkelProposalReturnCredential)
    (\txSkelProposal cred -> txSkelProposal {txSkelProposalReturnCredential = cred})

-- | Focuses on the optional constitution of a 'TxSkelProposal'
txSkelProposalMConstitutionAT :: forall kind. (Typeable kind) => AffineTraversal' TxSkelProposal (Maybe (User kind Redemption))
txSkelProposalMConstitutionAT =
  atraversal
    (\prop@(TxSkelProposal {txSkelProposalConstitution}) -> maybe (Left prop) Right $ cast txSkelProposalConstitution)
    (\prop@(TxSkelProposal @kind' cred action _ anchor) constit' -> maybe prop (\Refl -> TxSkelProposal cred action constit' anchor) $ eqT @kind @kind')

-- | Focuses on the constitution of a 'TxSkelProposal'
txSkelProposalConstitutionAT :: AffineTraversal' TxSkelProposal (User IsScript Redemption)
txSkelProposalConstitutionAT = txSkelProposalMConstitutionAT % _Just

-- | Focuses on the governance action of a 'TxSkelProposal'
txSkelProposalGovernanceActionAT :: forall req. (Typeable req) => AffineTraversal' TxSkelProposal (GovernanceAction req)
txSkelProposalGovernanceActionAT =
  atraversal
    (\prop@(TxSkelProposal {txSkelProposalGovernanceAction}) -> maybe (Left prop) Right $ cast txSkelProposalGovernanceAction)
    (\prop@(TxSkelProposal @req' cred _ constit anchor) newAction -> maybe prop (\Refl -> TxSkelProposal cred newAction constit anchor) $ eqT @req @req')

-- | A lens to get or set the anchor of a 'TxSkelProposal'
makeLensesFor [("txSkelProposalAnchor", "txSkelProposalAnchorL")] ''TxSkelProposal

-- * Smart constructors and updators

-- | Builds a 'TxSkelProposal' from a credential and a gov action. Does not set
-- any constitution (when applicable) nor anchor.
simpleProposal :: (Script.ToCredential cred, Typeable kind) => cred -> GovernanceAction kind -> TxSkelProposal
simpleProposal cred action = TxSkelProposal cred action Nothing Nothing

-- | Sets the constitution script with an empty redeemer when empty. This will
-- not tamper with an existing constitution script and redeemer.
fillConstitution :: (ToVScript script, Typeable script) => script -> TxSkelProposal -> TxSkelProposal
fillConstitution constitution =
  over
    (txSkelProposalMConstitutionAT @IsScript)
    (maybe (Just $ UserRedeemedScript constitution emptyTxSkelRedeemer) Just)
