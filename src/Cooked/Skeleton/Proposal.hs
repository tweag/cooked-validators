-- | This module exposes the notion of proposal within a
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Proposal
  ( ParameterChange (..),
    GovAction (..),
    TxSkelProposalGovAction,
    TxSkelProposal (..),
    txSkelProposalReturnCredentialL,
    txSkelProposalGovActionL,
    txSkelProposalWitnessedGovActionAT,
    txSkelProposalConstitutionAT,
    txSkelProposalRedeemerAT,
    txSkelProposalFreeGovActionAT,
    simpleTxSkelProposal,
    witnessedTxSkelProposal,
    autoFillConstitution,
    txSkelProposalConstitutionRedeemerAT,
  )
where

import Cooked.Skeleton.Redeemer as X
import Data.Kind (Type)
import Data.Map (Map)
import Data.Typeable (cast)
import GHC.TypeLits (KnownSymbol, Symbol)
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
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

-- | This lists the various possible governance actions. This type is annotated
-- by a boolean which states whether or not the governance action should be
-- witnessed by the constitution script.
data GovAction :: Symbol -> Type where
  -- If several parameter changes are of the same kind, only the last
  -- one will take effect
  ParameterChange :: [ParameterChange] -> GovAction "Witnessed"
  TreasuryWithdrawals :: Map Api.Credential Api.Lovelace -> GovAction "Witnessed"
  HardForkInitiation :: Api.ProtocolVersion -> GovAction "Free"
  NoConfidence :: GovAction "Free"
  UpdateCommittee :: [Api.ColdCommitteeCredential] -> Map Api.ColdCommitteeCredential Integer -> PlutusTx.Rational -> GovAction "Free"
  NewConstitution :: Api.Constitution -> GovAction "Free"

deriving instance Show (GovAction a)

deriving instance Eq (GovAction a)

type TxSkelProposalGovAction =
  Either
    (GovAction "Free")
    ( GovAction "Witnessed",
      Maybe (Script.Versioned Script.Script, TxSkelRedeemer)
    )

-- | This bundles a governance action into an actual proposal
data TxSkelProposal where
  TxSkelProposal ::
    { -- | The credential that should be used for a return account
      txSkelProposalReturnCredential :: Api.Credential,
      -- | The proposed action. It can either be a "free" action, which does not
      -- need to be witnessed by the constitution, or a witnessed action. The
      -- latter requires the constitution script, which can either be given
      -- manually, or left for cooekd to be filled out automatically based on
      -- the current official one.
      txSkelProposalGovAction :: TxSkelProposalGovAction
    } ->
    TxSkelProposal
  deriving (Show, Eq)

-- | Focuses on the the credential of a 'TxSkelProposal'
makeLensesFor [("txSkelProposalReturnCredential", "txSkelProposalReturnCredentialL")] ''TxSkelProposal

-- | Focuses on the governance action of a 'TxSkelProposal'
makeLensesFor [("txSkelProposalGovAction", "txSkelProposalGovActionL")] ''TxSkelProposal

-- | Focuses on the witnessed gov action from a 'TxSkelProposal'
txSkelProposalWitnessedGovActionAT :: AffineTraversal' TxSkelProposal (GovAction "Free")
txSkelProposalWitnessedGovActionAT = txSkelProposalGovActionL % _Left

-- | Focuses on the pair (constitution script, redeemer) from a 'TxSkelProposal'
txSkelProposalConstitutionRedeemerAT :: AffineTraversal' TxSkelProposal (Script.Versioned Script.Script, TxSkelRedeemer)
txSkelProposalConstitutionRedeemerAT = txSkelProposalGovActionL % _Right % _2 % _Just

-- | Focuses on the constitution script from a 'TxSkelProposal'
txSkelProposalConstitutionAT :: AffineTraversal' TxSkelProposal (Script.Versioned Script.Script)
txSkelProposalConstitutionAT = txSkelProposalConstitutionRedeemerAT % _1

-- | Focuses on the constitution redeemer from a 'TxSkelProposal'
txSkelProposalRedeemerAT :: AffineTraversal' TxSkelProposal TxSkelRedeemer
txSkelProposalRedeemerAT = txSkelProposalConstitutionRedeemerAT % _2

-- | Focuses on the free gov action from a 'TxSkelProposal'
txSkelProposalFreeGovActionAT :: AffineTraversal' TxSkelProposal (GovAction "Witnessed")
txSkelProposalFreeGovActionAT = txSkelProposalGovActionL % _Right % _1

-- | Builds a 'TxSkelProposal' from a credential and governance action of any
-- kind
simpleTxSkelProposal :: forall a b. (KnownSymbol b, Script.ToCredential a) => a -> GovAction b -> TxSkelProposal
simpleTxSkelProposal a govAction =
  TxSkelProposal (Script.toCredential a) $
    case cast @_ @(GovAction "Free") govAction of
      Nothing -> case cast @_ @(GovAction "Witnessed") govAction of
        Nothing -> error "Unreachable case"
        Just witnessedGovAction -> Right (witnessedGovAction, Nothing)
      Just freeGovAction -> Left freeGovAction

-- | Builds a witnessed 'TxSkelProposal' from a credential, witnessed governance
-- action, constitution script and redeemer
witnessedTxSkelProposal :: (Script.ToCredential a) => (a, GovAction "Witnessed", Script.Versioned Script.Script, TxSkelRedeemer) -> TxSkelProposal
witnessedTxSkelProposal (cred, govAction, constitution, red) = TxSkelProposal (Script.toCredential cred) $ Right (govAction, Just (constitution, red))

-- | Sets the constitution script with an empty redeemer when empty. This will
-- not tamper with an existing constitution script and redeemer.
autoFillConstitution :: Maybe (Script.Versioned Script.Script) -> TxSkelProposal -> TxSkelProposal
autoFillConstitution constitution = over (txSkelProposalGovActionL % _Right % _2) $ maybe ((,emptyTxSkelRedeemer) <$> constitution) Just
