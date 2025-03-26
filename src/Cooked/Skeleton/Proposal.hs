module Cooked.Skeleton.Proposal
  ( TxParameterChange (..),
    TxGovAction (..),
    TxSkelProposal (..),
    txSkelProposalAddressL,
    txSkelProposalActionL,
    txSkelProposalWitnessL,
    txSkelProposalAnchorL,
    simpleTxSkelProposal,
    withWitness,
    withAnchor,
  )
where

import Cooked.Skeleton.Redeemer as X
import Data.Map (Map)
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- These are all the protocol parameters. They are taken from
-- https://github.com/IntersectMBO/cardano-ledger/blob/c4fbc05999866fea7c0cb1b211fd5288f286b95d/eras/conway/impl/cddl-files/conway.cddl#L381-L412
-- and will most likely change in future eras.
data TxParameterChange where
  -- | The linear factor for the minimum fee calculation
  FeePerByte :: Integer -> TxParameterChange
  -- | The constant factor for the minimum fee calculation
  FeeFixed :: Integer -> TxParameterChange
  -- | Maximal block body size
  MaxBlockBodySize :: Integer -> TxParameterChange
  -- | Maximal transaction size
  MaxTxSize :: Integer -> TxParameterChange
  -- | Maximal block header size
  MaxBlockHeaderSize :: Integer -> TxParameterChange
  -- | The amount of a key registration deposit
  KeyDeposit :: Integer -> TxParameterChange
  -- | The amount of a pool registration deposit
  PoolDeposit :: Integer -> TxParameterChange
  -- | Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled future for.
  PoolRetirementMaxEpoch :: Integer -> TxParameterChange
  -- | Desired number of pools
  PoolNumber :: Integer -> TxParameterChange
  -- | Pool influence
  PoolInfluence :: Rational -> TxParameterChange
  -- | Monetary expansion
  MonetaryExpansion :: Rational -> TxParameterChange
  -- | Treasury expansion
  TreasuryCut :: Rational -> TxParameterChange
  -- | Minimum Stake Pool Cost
  MinPoolCost :: Integer -> TxParameterChange
  -- | Cost in lovelace per byte of UTxO storage
  CoinsPerUTxOByte :: Integer -> TxParameterChange
  -- | Cost models for non-native script languages
  CostModels ::
    { cmPlutusV1Costs :: [Integer],
      cmPlutusV2Costs :: [Integer],
      cmPlutusV3Costs :: [Integer]
    } ->
    TxParameterChange
  -- | Prices of execution units
  Prices ::
    { pMemoryCost :: Rational,
      pStepCost :: Rational
    } ->
    TxParameterChange
  -- | Max total script execution resources units allowed per tx
  MaxTxExUnits ::
    { mteuMemory :: Integer,
      mteuSteps :: Integer
    } ->
    TxParameterChange
  -- | Max total script execution resources units allowed per block
  MaxBlockExUnits ::
    { mbeuMemory :: Integer,
      mbeuSteps :: Integer
    } ->
    TxParameterChange
  -- | Max size of a Value in an output
  MaxValSize :: Integer -> TxParameterChange
  -- | Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  CollateralPercentage :: Integer -> TxParameterChange
  -- | Maximum number of collateral inputs allowed in a transaction
  MaxCollateralInputs :: Integer -> TxParameterChange
  -- | Thresholds for pool votes
  PoolVotingThresholds ::
    { pvtMotionNoConfidence :: Rational,
      pvtCommitteeNormal :: Rational,
      pvtCommitteeNoConfidence :: Rational,
      pvtHardFork :: Rational,
      pvtSecurityGroup :: Rational
    } ->
    TxParameterChange
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
    TxParameterChange
  -- | Minimum size of the Constitutional Committee
  CommitteeMinSize :: Integer -> TxParameterChange
  -- | The Constitutional Committee Term limit in number of Slots
  CommitteeMaxTermLength :: Integer -> TxParameterChange
  -- | Gov action lifetime in number of Epochs
  GovActionLifetime :: Integer -> TxParameterChange
  -- | The amount of the Gov Action deposit
  GovActionDeposit :: Integer -> TxParameterChange
  -- | The amount of a DRep registration deposit
  DRepRegistrationDeposit :: Integer -> TxParameterChange
  -- | The number of Epochs that a DRep can perform no activity without losing
  -- their @Active@ status.
  DRepActivity :: Integer -> TxParameterChange
  -- Reference scripts fee for the minimum fee calculation
  -- will exist later on MinFeeRefScriptCostPerByte :: Integer -> TxParameterChange
  deriving (Show, Eq)

data TxGovAction where
  -- If several parameter changes are of the same kind, only the last
  -- one will take effect
  TxGovActionParameterChange :: [TxParameterChange] -> TxGovAction
  TxGovActionHardForkInitiation :: Api.ProtocolVersion -> TxGovAction
  TxGovActionTreasuryWithdrawals :: Map Api.Credential Api.Lovelace -> TxGovAction
  TxGovActionNoConfidence :: TxGovAction
  TxGovActionUpdateCommittee :: [Api.ColdCommitteeCredential] -> Map Api.ColdCommitteeCredential Integer -> PlutusTx.Rational -> TxGovAction
  TxGovActionNewConstitution :: Api.Constitution -> TxGovAction
  deriving (Show, Eq)

data TxSkelProposal where
  TxSkelProposal ::
    { -- | Whatever credential will get back the deposit
      txSkelProposalAddress :: Api.Address,
      -- | The proposed action
      txSkelProposalAction :: TxGovAction,
      -- | An optional script (typically the constitution script) to witness the
      -- proposal and validate it. Only parameter changes and treasury
      -- withdrawals can be subject to such a validation and transactions will
      -- not pass validation phase 1 if other actions are given a witness.
      txSkelProposalWitness :: Maybe (Script.Versioned Script.Script, TxSkelRedeemer),
      -- | An optional anchor to be given as additional data. It should
      -- correspond to the URL of a web page
      txSkelProposalAnchor :: Maybe String
    } ->
    TxSkelProposal
  deriving (Show, Eq)

makeLensesFor
  [ ("txSkelProposalAddress", "txSkelProposalAddressL"),
    ("txSkelProposalAction", "txSkelProposalActionL"),
    ("txSkelProposalWitness", "txSkelProposalWitnessL"),
    ("txSkelProposalAnchor", "txSkelProposalAnchorL")
  ]
  ''TxSkelProposal

simpleTxSkelProposal :: (Script.ToAddress a) => a -> TxGovAction -> TxSkelProposal
simpleTxSkelProposal a govAction = TxSkelProposal (Script.toAddress a) govAction Nothing Nothing

withWitness :: (Script.ToVersioned Script.Script a) => TxSkelProposal -> (a, TxSkelRedeemer) -> TxSkelProposal
withWitness prop (s, red) = prop {txSkelProposalWitness = Just (Script.toVersioned s, red)}

withAnchor :: TxSkelProposal -> String -> TxSkelProposal
withAnchor prop url = prop {txSkelProposalAnchor = Just url}
