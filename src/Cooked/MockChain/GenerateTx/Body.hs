-- | This modules exposes entry points to convert a 'TxSkel' into a fully
-- fledged transaction body
module Cooked.MockChain.GenerateTx.Body
  ( txSkelToTxBody,
    txBodyContentToTxBody,
    txSkelToTxBodyContent,
    txSkelToIndex,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Internal.Fees qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
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
import Cooked.MockChain.GenerateTx.Witness (toKeyWitness)
import Cooked.Skeleton
import Cooked.Wallet
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Address qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
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
  let txFee = Cardano.TxFeeExplicit Cardano.ShelleyBasedEraConway $ Cardano.Coin fee
  txProposalProcedures <-
    Just . Cardano.Featured Cardano.ConwayEraOnwardsConway
      <$> Proposal.toProposalProcedures txSkelProposals (txOptAnchorResolution txSkelOpts)
  txWithdrawals <- Withdrawals.toWithdrawals txSkelWithdrawals
  let txMetadata = Cardano.TxMetadataNone
      txAuxScripts = Cardano.TxAuxScriptsNone
      txUpdateProposal = Cardano.TxUpdateProposalNone
      txCertificates = Cardano.TxCertificatesNone
      txScriptValidity = Cardano.TxScriptValidityNone
      txVotingProcedures = Nothing
      txCurrentTreasuryValue = Nothing
      txTreasuryDonation = Nothing
  return Cardano.TxBodyContent {..}

-- | Generates a transaction body from a body content
txBodyContentToTxBody :: (MonadBlockChainBalancing m) => Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra -> TxSkel -> m (Cardano.TxBody Cardano.ConwayEra)
txBodyContentToTxBody txBodyContent skel = do
  params <- getParams
  -- We create the associated Shelley TxBody
  txBody@(Cardano.ShelleyTxBody a body c dats e f) <-
    either
      (throwError . MCEToCardanoError "generateTx :")
      return
      (Emulator.createTransactionBody params (Ledger.CardanoBuildTx txBodyContent))

  -- There is a chance that the body is in need of additional data. This happens
  -- when the set of reference inputs contains hashed datums that will need to
  -- be resolved during phase 2 validation. All that follows until the
  -- definition of "txBody'" aims at doing just that. In the process, we have to
  -- reconstruct the body with the new data and the associated hash. Hopefully,
  -- in the future, cardano-api provides a way to add those data in the body
  -- directly without requiring this method, which somewhat feels like a hack.

  -- We gather the datums of the reference input in the skeleton
  refIns <- forM (txSkelReferenceTxOutRefs skel) $ fmap (view txSkelOutDatumL) . unsafeTxOutByRef
  -- We collect the additional data of the hashed datums as a map
  let additionalDataMap =
        Map.fromList [(Cardano.hashData dat, dat) | TxSkelOutSomeDatum (Cardano.Data . Api.toData -> dat) (Hashed _) <- refIns]
  -- -- We retrieve a needed parameter to process difference plutus languages
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

-- | Generates an index with utxos known to a 'TxSkel'
txSkelToIndex :: (MonadBlockChainBalancing m) => TxSkel -> Maybe (Set Api.TxOutRef, Wallet) -> m (Cardano.UTxO Cardano.ConwayEra)
txSkelToIndex txSkel mCollaterals = do
  -- We build the index of UTxOs which are known to this skeleton. This includes
  -- collateral inputs, inputs and reference inputs.
  let collateralIns = case mCollaterals of
        Nothing -> []
        Just (s, _) -> Set.toList s
  -- We retrieve all the outputs known to the skeleton
  (knownTxORefs, knownTxOuts) <- unzip . Map.toList <$> lookupUtxos (txSkelKnownTxOutRefs txSkel <> collateralIns)
  -- We then compute their Cardano counterparts
  txOutL <- forM knownTxOuts Output.toCardanoTxOut
  -- We build the index and handle the possible error
  case do
    txInL <- forM knownTxORefs Ledger.toCardanoTxIn
    return $ Cardano.UTxO $ Map.fromList $ zip txInL $ Cardano.toCtxUTxOTxOut <$> txOutL of
    Left err -> throwError $ MCEToCardanoError "txSkelToIndex:" err
    Right index' -> return index'

-- | Generates a transaction body from a 'TxSkel' and associated fee and
-- collateral information. This transaction body accounts for the actual
-- execution units of each of the scripts involved in the skeleton.
txSkelToTxBody :: (MonadBlockChainBalancing m) => TxSkel -> Integer -> Maybe (Set Api.TxOutRef, Wallet) -> m (Cardano.TxBody Cardano.ConwayEra)
txSkelToTxBody txSkel fee mCollaterals = do
  index <- txSkelToIndex txSkel mCollaterals
  -- We create a first body content and body, without execution units
  txBodyContent' <- txSkelToTxBodyContent txSkel fee mCollaterals
  txBody' <- txBodyContentToTxBody txBodyContent' txSkel
  -- We create a full transaction from the body
  let tx' = Cardano.Tx txBody' (toKeyWitness txBody' <$> txSkelSigners txSkel)
  -- We retrieve the parameters
  params <- getParams
  -- We retrieve the execution units associated with the transaction
  case Emulator.getTxExUnitsWithLogs params (Ledger.fromPlutusIndex index) tx' of
    -- Computing the execution units can result in all kinds of validation
    -- errors except for the ones related to the execution units themselves.
    Left err -> throwError $ uncurry MCEValidationError err
    -- When no error arises, we get an execution unit for each script usage. We
    -- then update the body content to account for those usages.
    Right (Map.toList -> exUnitsList) -> case Cardano.substituteExecutionUnits
      ( Map.fromList $
          fmap
            (\(x, (_, Cardano.ExUnits mem steps)) -> (fromPlutusPurpose x, Cardano.ExecutionUnits steps mem))
            exUnitsList
      )
      txBodyContent' of
      -- This can only be a @TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap@
      Left _ -> throwError $ FailWith "Error while assigning execution units"
      -- We now have a body content with proper execution units and can create
      -- the final body from it
      Right txBody -> txBodyContentToTxBody txBody txSkel
  where
    fromPlutusPurpose (Cardano.ConwaySpending (Cardano.AsIx n)) = Cardano.ScriptWitnessIndexTxIn n
    fromPlutusPurpose (Cardano.ConwayMinting (Cardano.AsIx n)) = Cardano.ScriptWitnessIndexMint n
    fromPlutusPurpose (Cardano.ConwayCertifying (Cardano.AsIx n)) = Cardano.ScriptWitnessIndexCertificate n
    fromPlutusPurpose (Cardano.ConwayRewarding (Cardano.AsIx n)) = Cardano.ScriptWitnessIndexWithdrawal n
    fromPlutusPurpose (Cardano.ConwayVoting (Cardano.AsIx n)) = Cardano.ScriptWitnessIndexVoting n
    fromPlutusPurpose (Cardano.ConwayProposing (Cardano.AsIx n)) = Cardano.ScriptWitnessIndexProposing n
