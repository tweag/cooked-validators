-- | This modules exposes entry points to convert a 'TxSkel' into a fully
-- fledged transaction body
module Cooked.Automation.GenerateTx.Body
  ( txSkelToTxBody,
    txBodyContentToTxBody,
    txSkelToTxBodyContent,
    txSkelToIndex,
    txSignatoriesAndBodyToCardanoTx,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Alonzo.Plutus.Evaluate qualified as Alonzo
import Control.Monad
import Cooked.Automation.GenerateTx.Certificate
import Cooked.Automation.GenerateTx.Collateral
import Cooked.Automation.GenerateTx.Input
import Cooked.Automation.GenerateTx.Mint
import Cooked.Automation.GenerateTx.Output
import Cooked.Automation.GenerateTx.Proposal
import Cooked.Automation.GenerateTx.ReferenceInputs
import Cooked.Automation.GenerateTx.Withdrawals
import Cooked.Automation.GenerateTx.Witness
import Cooked.Common
import Cooked.Effect.Read.Chain
import Cooked.Effect.Read.Conf
import Cooked.Runtime.Error
import Cooked.Skeleton
import Data.Bifunctor (first)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Address qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Witherable

-- | Generates a body content from a skeleton
txSkelToTxBodyContent ::
  ( Members
      '[ MockChainReadChain,
         MockChainReadConf,
         Error MockChainError,
         Error P.Ledger.ToCardanoError,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Fee ->
  Maybe Collaterals ->
  Sem effs BodyContent
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
      -- This is filled later on, after computing the execution units
      txScriptValidity = Cardano.TxScriptValidityNone
      txMetadata = Cardano.TxMetadataNone
      txAuxScripts = Cardano.TxAuxScriptsNone
      txUpdateProposal = Cardano.TxUpdateProposalNone
      txVotingProcedures = Nothing
      txCurrentTreasuryValue = Nothing
      txTreasuryDonation = Nothing
  return Cardano.TxBodyContent {..}

-- | Generates a transaction body from a body content
txBodyContentToTxBody ::
  (Member (Error P.Ledger.ToCardanoError) effs) =>
  BodyContent ->
  Sem effs Body
txBodyContentToTxBody =
  fromEither
    . first (P.Ledger.TxBodyError . Cardano.displayError)
    . Cardano.createTransactionBody Cardano.shelleyBasedEra

-- | Generates an index with utxos known to a 'TxSkel'
txSkelToIndex ::
  ( Members
      '[ MockChainReadChain,
         MockChainReadConf,
         Error P.Ledger.ToCardanoError
       ]
      effs
  ) =>
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
  -- We reshape the built index to the right format and return it
  return $ Cardano.UTxO $ Map.fromList $ zip txInL $ Cardano.toCtxUTxOTxOut <$> txOutL

-- | Generates a transaction body from a 'TxSkel' and associated fee and
-- collateral information. This transaction body accounts for the actual
-- execution units of each of the scripts involved in the skeleton. During the
-- computation of these execution units, some validation errors can occur, in
-- which case the body will not account for them, but the error maps will be
-- returned.
txSkelToTxBody ::
  ( Members
      '[ MockChainReadChain,
         MockChainReadConf,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Fee ->
  Maybe Collaterals ->
  Sem effs (Body, ExUnitsFailures)
txSkelToTxBody txSkel fee mCollaterals = do
  -- We create a first body content and body, without execution units
  txBodyContent' <- txSkelToTxBodyContent txSkel fee mCollaterals
  txBody' <- txBodyContentToTxBody txBodyContent'
  -- We create a full transaction from the body
  let (Cardano.ShelleyTx _ tx) = txSignatoriesAndBodyToCardanoTx (txSkelSignatories txSkel) txBody'
  -- We build the index of known utxos
  index <- txSkelToIndex txSkel mCollaterals
  -- We retrieve the parameters
  params <- getParams
  -- We retrieve the @epochInfo@ from the era history
  epochInfo <- Cardano.unLedgerEpochInfo . Cardano.toLedgerEpochInfo <$> getEraHistory
  -- We retrieve the system start
  systemStart <- getSystemStart
  -- We compute the execution units associated with the transaction
  let exUnitsReport = Alonzo.evalTxExUnits params tx (P.Ledger.fromPlutusIndex index) epochInfo systemStart
  -- We transform the keys to Cardano script index
  let cExUnitsReport = Map.mapKeysMonotonic (Cardano.toScriptIndex Cardano.AlonzoEraOnwardsConway) exUnitsReport
  -- We extract the succesful cases from the map
  let executionUnitsMap = mapMaybe (preview (_Right % to Cardano.fromAlonzoExUnits)) cExUnitsReport
  -- We also extract the failures
  let failuresMap = mapMaybe (preview _Left) cExUnitsReport
  -- We attempt to insert the execution units in the body
  let (txBodyContent, scriptValid) =
        Cardano.substituteExecutionUnits executionUnitsMap txBodyContent'
          & either
            -- If this fails, this can only be a
            -- @TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap@ which means that
            -- some scripts failed (@failureMap@ is not empty) in which case we return
            -- the original body, and mark the scripts as invalid.
            (const (txBodyContent', Cardano.ScriptInvalid))
            -- We now have a body content with proper execution units and can create
            -- the final body from it, while marking the scripts as valid.
            (,Cardano.ScriptValid)
  -- We generate the final tx body from the body content and the script validity
  finalTxBody <- txBodyContentToTxBody txBodyContent {Cardano.txScriptValidity = Cardano.TxScriptValidity Cardano.AlonzoEraOnwardsConway scriptValid}
  return (finalTxBody, failuresMap)

-- | Generates a Cardano transaction and signs it
txSignatoriesAndBodyToCardanoTx ::
  [TxSkelSignatory] ->
  Body ->
  Transaction
txSignatoriesAndBodyToCardanoTx signatories txBody = Cardano.Tx txBody $ mapMaybe (toKeyWitness txBody) signatories
