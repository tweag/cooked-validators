module Cooked.MockChain.GenerateTx.Body
  ( txSkelToTxBody,
    txBodyContentToTxBody,
    txSkelToTxBodyContent,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Conway.PParams qualified as Conway
import Cardano.Ledger.Plutus qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Collateral qualified as Collateral
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Input qualified as Input
import Cooked.MockChain.GenerateTx.Mint qualified as Mint
import Cooked.MockChain.GenerateTx.Output qualified as Output
import Cooked.MockChain.GenerateTx.Proposal qualified as Proposal
import Cooked.MockChain.GenerateTx.Withdrawals qualified as Withdrawals
import Cooked.Skeleton
import Cooked.Wallet
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Address qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api

-- | Generates a body content from a skeleton
txSkelToTxBodyContent ::
  (MonadBlockChainBalancing m) =>
  -- | The skeleton from which the body is created
  TxSkel ->
  -- | The fee to set in the body
  Integer ->
  -- | The collaterals to set in the body
  Maybe (Set Api.TxOutRef, Wallet) ->
  -- | Returns a Cardano body content
  m (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
txSkelToTxBodyContent skel@TxSkel {..} fee mCollaterals | txSkelReferenceInputs <- txSkelReferenceTxOutRefs skel = do
  txIns <- mapM Input.toTxInAndWitness $ Map.toList txSkelIns
  txInsReference <-
    if null txSkelReferenceInputs
      then return Cardano.TxInsReferenceNone
      else
        throwOnToCardanoErrorOrApply
          "txSkelToBodyContent: Unable to translate reference inputs."
          (Cardano.TxInsReference Cardano.BabbageEraOnwardsConway)
          $ mapM Ledger.toCardanoTxIn txSkelReferenceInputs
  (txInsCollateral, txTotalCollateral, txReturnCollateral) <- Collateral.toCollateralTriplet fee mCollaterals
  txOuts <- mapM Output.toCardanoTxOut txSkelOuts
  (txValidityLowerBound, txValidityUpperBound) <-
    throwOnToCardanoError
      "txSkelToBodyContent: Unable to translate transaction validity range."
      $ Ledger.toCardanoValidityRange txSkelValidityRange
  txMintValue <- Mint.toMintValue txSkelMints
  txExtraKeyWits <-
    if null txSkelSigners
      then return Cardano.TxExtraKeyWitnessesNone
      else
        throwOnToCardanoErrorOrApply
          "txSkelToBodyContent: Unable to translate the required signers"
          (Cardano.TxExtraKeyWitnesses Cardano.AlonzoEraOnwardsConway)
          $ mapM (Ledger.toCardanoPaymentKeyHash . Ledger.PaymentPubKeyHash . walletPKHash) txSkelSigners
  txProtocolParams <- Cardano.BuildTxWith . Just . Emulator.ledgerProtocolParameters <$> getParams
  let txFee = Cardano.TxFeeExplicit Cardano.ShelleyBasedEraConway $ Emulator.Coin fee
  txProposalProcedures <-
    Just . Cardano.Featured Cardano.ConwayEraOnwardsConway
      <$> Proposal.toProposalProcedures txSkelProposals (txOptAnchorResolution txSkelOpts)
  txWithdrawals <- Withdrawals.toWithdrawals txSkelWithdrawals
  let txMetadata = Cardano.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = Cardano.TxAuxScriptsNone -- That's what plutus-apps does as well
      txUpdateProposal = Cardano.TxUpdateProposalNone -- That's what plutus-apps does as well
      txCertificates = Cardano.TxCertificatesNone -- That's what plutus-apps does as well
      txScriptValidity = Cardano.TxScriptValidityNone -- That's what plutus-apps does as well
      txVotingProcedures = Nothing
      txCurrentTreasuryValue = Nothing
      txTreasuryDonation = Nothing
  return Cardano.TxBodyContent {..}

txBodyContentToTxBody :: (MonadBlockChainBalancing m) => Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra -> TxSkel -> m (Cardano.TxBody Cardano.ConwayEra)
txBodyContentToTxBody txBodyContent skel = do
  -- We create the associated Shelley TxBody
  txBody@(Cardano.ShelleyTxBody a body c dats e f) <-
    either
      (throwError . MCEGenerationError . TxBodyError "generateTx :")
      return
      (Cardano.createTransactionBody Cardano.ShelleyBasedEraConway txBodyContent)

  -- There is a chance that the body is in need of additional data. This happens
  -- when the set of reference inputs contains hashed datums that will need to
  -- be resolved during phase 2 validation. All that follows until the
  -- definition of "txBody'" aims at doing just that. In the process, we have to
  -- reconstruct the body with the new data and the associated hash. Hopefully,
  -- in the future, cardano-api provides a way to add those data in the body
  -- directly without requiring this method, which somewhat feels like a hack.

  -- We attempt to resolve the reference inputs used by the skeleton
  refIns <- forM (txSkelReferenceTxOutRefs skel) $ \oRef ->
    throwOnMaybe ("txSkelToCardanoTx: Unable to resolve TxOutRef " <> show oRef) =<< txOutByRef oRef
  -- We collect the datum hashes present at these outputs
  let datumHashes = [hash | (Api.TxOut _ _ (Api.OutputDatumHash hash) _) <- refIns]
  -- We resolve those datum hashes from the context
  additionalData <- forM datumHashes $ \dHash ->
    throwOnMaybe ("txSkelToCardanoTx: Unable to resolve datum hash " <> show dHash) =<< datumFromHash dHash
  -- We compute the map from datum hash to datum of these additional required data
  let additionalDataMap = Map.fromList [(Cardano.hashData dat, dat) | Api.Datum (Cardano.Data . Api.toData -> dat) <- additionalData]
  -- We retrieve a needed parameter to process difference plutus languages
  toLangDepViewParam <- Conway.getLanguageView . Cardano.unLedgerProtocolParameters . Emulator.ledgerProtocolParameters <$> getParams
  -- We convert our data map into a 'TxDats'
  let txDats' = Alonzo.TxDats additionalDataMap
      -- We compute the new era, datums and redeemers based on the current dats
      -- in the body and the additional data to include in the body.
      (era, datums, redeemers) = case dats of
        Cardano.TxBodyNoScriptData -> (Cardano.AlonzoEraOnwardsConway, txDats', Alonzo.Redeemers Map.empty)
        Cardano.TxBodyScriptData era' txDats reds -> (era', txDats <> txDats', reds)
      -- We collect the various witnesses in the body
      witnesses = Cardano.collectTxBodyScriptWitnesses Cardano.ShelleyBasedEraConway txBodyContent
      -- We collect their associated languages
      languages = [toCardanoLanguage v | (_, Cardano.AnyScriptWitness (Cardano.PlutusScriptWitness _ v _ _ _ _)) <- witnesses]
      -- We compute the new script integrity hash with the added data
      scriptIntegrityHash =
        Cardano.alonzoEraOnwardsConstraints era $
          Alonzo.hashScriptIntegrity (Set.fromList $ toLangDepViewParam <$> languages) redeemers datums
      -- We wrap all of this in the new body
      body' = body Lens.& Alonzo.scriptIntegrityHashTxBodyL Lens..~ scriptIntegrityHash
      txBody' = Cardano.ShelleyTxBody a body' c (Cardano.TxBodyScriptData era datums redeemers) e f

  return $ if null additionalDataMap then txBody else txBody'
  where
    toCardanoLanguage :: Cardano.PlutusScriptVersion lang -> Cardano.Language
    toCardanoLanguage = \case
      Cardano.PlutusScriptV1 -> Cardano.PlutusV1
      Cardano.PlutusScriptV2 -> Cardano.PlutusV2
      Cardano.PlutusScriptV3 -> Cardano.PlutusV3

txSkelToTxBody :: (MonadBlockChainBalancing m) => TxSkel -> Integer -> Maybe (Set Api.TxOutRef, Wallet) -> m (Cardano.TxBody Cardano.ConwayEra)
txSkelToTxBody skel fee mCollaterals = do
  txBodyContent <- txSkelToTxBodyContent skel fee mCollaterals
  txBodyContentToTxBody txBodyContent skel
