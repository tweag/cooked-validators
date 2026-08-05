-- | This modules exposes entry points to convert a 'TxSkel' into a fully
-- fledged transaction body
module Cooked.MockChain.Automation.GenerateTx.Body
  ( txSkelToTxBody,
    txBodyContentToTxBody,
    txSkelToTxBodyContent,
    txSkelToIndex,
    txSignatoriesAndBodyToCardanoTx,
    txSkelToCardanoTx,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Alonzo.Plutus.Evaluate qualified as Alonzo
import Control.Monad
import Cooked.MockChain.Automation.GenerateTx.Certificate
import Cooked.MockChain.Automation.GenerateTx.Collateral
import Cooked.MockChain.Automation.GenerateTx.Input
import Cooked.MockChain.Automation.GenerateTx.Mint
import Cooked.MockChain.Automation.GenerateTx.Output
import Cooked.MockChain.Automation.GenerateTx.Proposal
import Cooked.MockChain.Automation.GenerateTx.ReferenceInputs
import Cooked.MockChain.Automation.GenerateTx.Withdrawals
import Cooked.MockChain.Automation.GenerateTx.Witness
import Cooked.MockChain.Common
import Cooked.MockChain.Effect.Read
import Cooked.MockChain.Runtime.Error
import Cooked.Skeleton
import Data.Bifunctor (first)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Ledger.Address qualified as P.Ledger
import Ledger.Index qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V1 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail

-- | Generates a body content from a skeleton
txSkelToTxBodyContent ::
  (Members '[MockChainRead, Error MockChainError, Error P.Ledger.ToCardanoError, Fail] effs) =>
  TxSkel ->
  Fee ->
  Maybe Collaterals ->
  Sem effs (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
txSkelToTxBodyContent skel@TxSkel {..} fee mCollaterals = do
  txIns <- mapM toTxInAndWitness $ Map.toList txSkelInputs
  txInsReference <- toInsReference skel
  (txInsCollateral, txTotalCollateral, txReturnCollateral) <- toCollateralTriplet mCollaterals
  txOuts <- mapM toCardanoTxOut txSkelOutputs
  (txValidityLowerBound, txValidityUpperBound) <- fromEither $ P.Ledger.toCardanoValidityRange txSkelValidityRange
  txMintValue <- toMintValue txSkelMints
  txExtraKeyWits <-
    if null txSkelSignatories
      then return Cardano.TxExtraKeyWitnessesNone
      else
        Cardano.TxExtraKeyWitnesses Cardano.AlonzoEraOnwardsConway
          <$> fromEither
            (mapM (P.Ledger.toCardanoPaymentKeyHash . P.Ledger.PaymentPubKeyHash . Script.toPubKeyHash) txSkelSignatories)
  txProtocolParams <- Cardano.BuildTxWith . Just . Cardano.LedgerProtocolParameters <$> getParams
  txProposalProcedures <- Just . Cardano.Featured Cardano.ConwayEraOnwardsConway <$> toProposalProcedures txSkelProposals
  txWithdrawals <- toWithdrawals txSkelWithdrawals
  txCertificates <- toCertificates txSkelCertificates
  let txFee = Cardano.TxFeeExplicit Cardano.ShelleyBasedEraConway $ Cardano.Coin fee
      txMetadata = Cardano.TxMetadataNone
      txAuxScripts = Cardano.TxAuxScriptsNone
      txUpdateProposal = Cardano.TxUpdateProposalNone
      txScriptValidity = Cardano.TxScriptValidityNone
      txVotingProcedures = Nothing
      txCurrentTreasuryValue = Nothing
      txTreasuryDonation = Nothing
  return Cardano.TxBodyContent {..}

-- | Generates a transaction body from a body content
txBodyContentToTxBody ::
  (Member (Error P.Ledger.ToCardanoError) effs) =>
  Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra ->
  Sem effs (Cardano.TxBody Cardano.ConwayEra)
txBodyContentToTxBody =
  fromEither
    . first (P.Ledger.TxBodyError . Cardano.displayError)
    . Cardano.createTransactionBody Cardano.shelleyBasedEra

-- | Generates an index with utxos known to a 'TxSkel'
txSkelToIndex ::
  (Members '[MockChainRead, Error P.Ledger.ToCardanoError] effs) =>
  TxSkel ->
  Maybe Collaterals ->
  Sem effs (Cardano.UTxO Cardano.ConwayEra)
txSkelToIndex txSkel mCollaterals = do
  -- We build the index of UTxOs which are known to this skeleton. This includes
  -- collateral inputs, inputs and reference inputs.
  let collateralIns = maybe [] (Set.toList . fst) mCollaterals
  -- We retrieve all the outputs known to the skeleton
  (knownTxORefs, knownTxOuts) <- unzip . Map.toList <$> lookupUtxos (Set.toList (txSkelKnownTxOutRefs txSkel) <> collateralIns)
  -- We then compute their Cardano counterparts
  txOutL <- forM knownTxOuts toCardanoTxOut
  -- We build the index and handle the possible error
  txInL <- fromEither $ forM knownTxORefs P.Ledger.toCardanoTxIn
  return $ Cardano.UTxO $ Map.fromList $ zip txInL $ Cardano.toCtxUTxOTxOut <$> txOutL

-- | Generates a transaction body from a 'TxSkel' and associated fee and
-- collateral information. This transaction body accounts for the actual
-- execution units of each of the scripts involved in the skeleton.
txSkelToTxBody ::
  (Members '[MockChainRead, Error MockChainError, Error P.Ledger.ToCardanoError, Fail] effs) =>
  TxSkel ->
  Fee ->
  Maybe Collaterals ->
  Sem effs (Cardano.TxBody Cardano.ConwayEra)
txSkelToTxBody txSkel fee mCollaterals = do
  -- We create a first body content and body, without execution units
  txBodyContent' <- txSkelToTxBodyContent txSkel fee mCollaterals
  txBody' <- txBodyContentToTxBody txBodyContent'
  -- We create a full transaction from the body
  let (Cardano.ShelleyTx _ tx) = txSignatoriesAndBodyToCardanoTx (txSkelSignatories txSkel) txBody'
  -- We retrieve the index and parameters to feed to @getTxExUnitsWithLogs@
  index <- txSkelToIndex txSkel mCollaterals
  params <- getParams
  epochInfo <- Cardano.unLedgerEpochInfo . Cardano.toLedgerEpochInfo <$> getEraHistory
  systemStart <- getSystemStart
  -- We compute the execution units associated with the transaction and process
  -- the result by splitting successful cases from errors.
  let exUnitsReport = Alonzo.evalTxExUnits params tx (P.Ledger.fromPlutusIndex index) epochInfo systemStart
      (success, errors) =
        foldl
          ( \(sucs, errs) (purpose, report) -> case report of
              Right exUnits ->
                ( Map.insert (Cardano.toScriptIndex Cardano.AlonzoEraOnwardsConway purpose) (Cardano.fromAlonzoExUnits exUnits) sucs,
                  errs
                )
              Left err ->
                ( success,
                  ( P.Ledger.Phase2,
                    case err of
                      Alonzo.ValidationFailure _ (Api.CekError e) logs _ -> P.Ledger.ScriptFailure (Api.EvaluationError logs ("CekEvaluationFailure: " ++ show e))
                      e -> P.Ledger.CardanoLedgerValidationError $ Text.pack $ show e
                  )
                    : errs
                )
          )
          (Map.empty, [])
          (Map.toList exUnitsReport)
  -- Computing the execution units can result in all phase 2 validation
  -- failures, except for the ones related to the execution units themselves.
  case errors of
    -- No validation failures detected, we assigne the execution units.
    [] -> case Cardano.substituteExecutionUnits success txBodyContent' of
      -- This can only be a @TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap@
      Left err -> throw $ MCEFailure $ "Error while assigning execution units: " <> show err
      -- We now have a body content with proper execution units and can create
      -- the final body from it
      Right txBodyContent -> txBodyContentToTxBody txBodyContent
    -- Some validation failures detected, and they should be handled
    l | not $ txSkelOptDeferPhase2FailuresDuringBalancing $ txSkelOpts txSkel -> throw $ MCEValidationError l
    -- Some validation failures detected, which should be deferred. We ignore
    -- them and return the current body without assigning execution units.
    _ -> return txBody'

-- | Generates a Cardano transaction and signs it
txSignatoriesAndBodyToCardanoTx ::
  [TxSkelSignatory] ->
  Cardano.TxBody Cardano.ConwayEra ->
  Cardano.Tx Cardano.ConwayEra
txSignatoriesAndBodyToCardanoTx signatories txBody = Cardano.Tx txBody $ mapMaybe (toKeyWitness txBody) signatories

-- | Generates a full Cardano transaction from a skeleton, fees and collaterals
txSkelToCardanoTx ::
  (Members '[MockChainRead, Error MockChainError, Error P.Ledger.ToCardanoError, Fail] effs) =>
  TxSkel ->
  Fee ->
  Maybe Collaterals ->
  Sem effs (Cardano.Tx Cardano.ConwayEra)
txSkelToCardanoTx txSkel fee =
  fmap (txSignatoriesAndBodyToCardanoTx (txSkelSignatories txSkel))
    . txSkelToTxBody txSkel fee
