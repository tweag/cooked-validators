-- | This module exposes the generation of proposal procedures
module Cooked.MockChain.GenerateTx.Proposal (toProposalProcedures) where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as Cardano
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Ledger.Conway.Governance qualified as Conway
import Cardano.Ledger.Conway.PParams qualified as Conway
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Control.Monad.Except (throwError)
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Anchor
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Data.Coerce
import Data.Map qualified as Map
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe
import Data.Maybe.Strict
import Ledger.Tx.CardanoAPI qualified as Ledger
import Lens.Micro qualified as MicroLens
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api

-- | Transorms a `Cooked.Skeleton.Proposal.ParameterChange` into an actual
-- change over a Cardano parameter update
toPParamsUpdate :: ParameterChange -> Conway.PParamsUpdate Emulator.EmulatorEra -> Conway.PParamsUpdate Emulator.EmulatorEra
toPParamsUpdate pChange =
  -- From rational to bounded rational
  let toBR :: (Cardano.BoundedRational r) => Rational -> r
      toBR = fromMaybe minBound . Cardano.boundRational
      -- Helper to set one of the param update with a lens
      setL l = MicroLens.set l . SJust
   in case pChange of
        FeePerByte n -> setL Conway.ppuMinFeeAL $ fromIntegral n
        FeeFixed n -> setL Conway.ppuMinFeeBL $ fromIntegral n
        MaxBlockBodySize n -> setL Conway.ppuMaxBBSizeL $ fromIntegral n
        MaxTxSize n -> setL Conway.ppuMaxTxSizeL $ fromIntegral n
        MaxBlockHeaderSize n -> setL Conway.ppuMaxBHSizeL $ fromIntegral n
        KeyDeposit n -> setL Conway.ppuKeyDepositL $ fromIntegral n
        PoolDeposit n -> setL Conway.ppuPoolDepositL $ fromIntegral n
        PoolRetirementMaxEpoch n -> setL Conway.ppuEMaxL $ Cardano.EpochInterval $ fromIntegral n
        PoolNumber n -> setL Conway.ppuNOptL $ fromIntegral n
        PoolInfluence q -> setL Conway.ppuA0L $ fromMaybe minBound $ Cardano.boundRational q
        MonetaryExpansion q -> setL Conway.ppuRhoL $ fromMaybe minBound $ Cardano.boundRational q
        TreasuryCut q -> setL Conway.ppuTauL $ toBR q
        MinPoolCost n -> setL Conway.ppuMinPoolCostL $ fromIntegral n
        CoinsPerUTxOByte n -> setL Conway.ppuCoinsPerUTxOByteL $ Conway.CoinPerByte $ fromIntegral n
        CostModels _pv1 _pv2 _pv3 -> id -- TODO unsupported for now
        Prices q r -> setL Conway.ppuPricesL $ Cardano.Prices (toBR q) (toBR r)
        MaxTxExUnits n m -> setL Conway.ppuMaxTxExUnitsL $ Cardano.ExUnits (fromIntegral n) (fromIntegral m)
        MaxBlockExUnits n m -> setL Conway.ppuMaxBlockExUnitsL $ Cardano.ExUnits (fromIntegral n) (fromIntegral m)
        MaxValSize n -> setL Conway.ppuMaxValSizeL $ fromIntegral n
        CollateralPercentage n -> setL Conway.ppuCollateralPercentageL $ fromIntegral n
        MaxCollateralInputs n -> setL Conway.ppuMaxCollateralInputsL $ fromIntegral n
        PoolVotingThresholds a b c d e ->
          setL Conway.ppuPoolVotingThresholdsL $
            Conway.PoolVotingThresholds (toBR a) (toBR b) (toBR c) (toBR d) (toBR e)
        DRepVotingThresholds a b c d e f g h i j ->
          setL Conway.ppuDRepVotingThresholdsL $
            Conway.DRepVotingThresholds (toBR a) (toBR b) (toBR c) (toBR d) (toBR e) (toBR f) (toBR g) (toBR h) (toBR i) (toBR j)
        CommitteeMinSize n -> setL Conway.ppuCommitteeMinSizeL $ fromIntegral n
        CommitteeMaxTermLength n -> setL Conway.ppuCommitteeMaxTermLengthL $ Cardano.EpochInterval $ fromIntegral n
        GovActionLifetime n -> setL Conway.ppuGovActionLifetimeL $ Cardano.EpochInterval $ fromIntegral n
        GovActionDeposit n -> setL Conway.ppuGovActionDepositL $ fromIntegral n
        DRepRegistrationDeposit n -> setL Conway.ppuDRepDepositL $ fromIntegral n
        DRepActivity n -> setL Conway.ppuDRepActivityL $ Cardano.EpochInterval $ fromIntegral n
        MinFeeRefScriptCostPerByte q -> setL Conway.ppuMinFeeRefScriptCostPerByteL $ fromMaybe minBound $ Cardano.boundRational q

-- | Translates a given skeleton proposal into a governance action
toGovAction :: (MonadBlockChainBalancing m) => TxSkelGovAction a -> StrictMaybe Conway.ScriptHash -> m (Conway.GovAction Emulator.EmulatorEra)
toGovAction NoConfidence _ = return $ Conway.NoConfidence SNothing
toGovAction UpdateCommittee {} _ = throwError $ MCEUnsupportedFeature "UpdateCommittee"
toGovAction NewConstitution {} _ = throwError $ MCEUnsupportedFeature "TxGovActionNewConstitution"
toGovAction HardForkInitiation {} _ = throwError $ MCEUnsupportedFeature "TxGovActionHardForkInitiation"
toGovAction (ParameterChange changes) sHash =
  return $ Conway.ParameterChange SNothing (foldl (flip toPParamsUpdate) (Conway.PParamsUpdate Cardano.emptyPParamsStrictMaybe) changes) sHash
toGovAction (TreasuryWithdrawals (Map.toList -> withdrawals)) sHash =
  (`Conway.TreasuryWithdrawals` sHash) . Map.fromList <$> mapM (\(cred, Api.Lovelace lv) -> (,Cardano.Coin lv) <$> toRewardAccount cred) withdrawals

-- | Translates a list of skeleton proposals into a proposal procedures
toProposalProcedures ::
  (MonadBlockChainBalancing m) =>
  [TxSkelProposal] ->
  m (Cardano.TxProposalProcedures Cardano.BuildTx Cardano.ConwayEra)
toProposalProcedures props | null props = return Cardano.TxProposalProceduresNone
toProposalProcedures props =
  Cardano.TxProposalProcedures . OMap.fromList
    <$> forM
      props
      ( \(TxSkelProposal (Script.toCredential -> returnCredential) govAction mConstitution (toCardanoAnchor -> anchor)) -> do
          proposalDeposit <- govActionDeposit
          rewardAccount <- toRewardAccount returnCredential
          (Cardano.BuildTxWith -> mConstitutionWitness, mConstitutionHash) <- case mConstitution of
            Just (UserRedeemedScript (toVScript -> script) redeemer) -> do
              scriptWitness <- toScriptWitness script redeemer Cardano.NoScriptDatumForStake
              Cardano.ScriptHash scriptHash <- throwOnToCardanoError "Unable to convert script hash" $ Ledger.toCardanoScriptHash $ Script.toScriptHash script
              return (Just scriptWitness, SJust scriptHash)
            _ -> return (Nothing, SNothing)
          cardanoGovAction <- toGovAction govAction mConstitutionHash
          return (Conway.ProposalProcedure (Cardano.Coin $ coerce proposalDeposit) rewardAccount cardanoGovAction anchor, mConstitutionWitness)
      )
