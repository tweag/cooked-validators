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
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Collateral
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Input
import Cooked.MockChain.GenerateTx.Mint
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.GenerateTx.Proposal
import Cooked.MockChain.GenerateTx.ReferenceInputs
import Cooked.MockChain.GenerateTx.Withdrawals
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Cooked.Wallet
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Address qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Address qualified as Script
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
txSkelToTxBodyContent skel@TxSkel {..} fee mCollaterals = do
  txIns <- mapM toTxInAndWitness $ Map.toList txSkelIns
  txInsReference <- toInsReference skel
  (txInsCollateral, txTotalCollateral, txReturnCollateral) <- toCollateralTriplet fee mCollaterals
  txOuts <- mapM toCardanoTxOut txSkelOuts
  (txValidityLowerBound, txValidityUpperBound) <-
    throwOnToCardanoError
      "txSkelToBodyContent: Unable to translate transaction validity range."
      $ Ledger.toCardanoValidityRange txSkelValidityRange
  txMintValue <- toMintValue txSkelMints
  txExtraKeyWits <-
    if null txSkelSigners
      then return Cardano.TxExtraKeyWitnessesNone
      else
        throwOnToCardanoErrorOrApply
          "txSkelToBodyContent: Unable to translate the required signers"
          (Cardano.TxExtraKeyWitnesses Cardano.AlonzoEraOnwardsConway)
          $ mapM (Ledger.toCardanoPaymentKeyHash . Ledger.PaymentPubKeyHash . Script.toPubKeyHash) txSkelSigners
  txProtocolParams <- Cardano.BuildTxWith . Just . Emulator.ledgerProtocolParameters <$> getParams
  txProposalProcedures <-
    Just . Cardano.Featured Cardano.ConwayEraOnwardsConway
      <$> toProposalProcedures txSkelProposals (txSkelOptAnchorResolution txSkelOpts)
  txWithdrawals <- toWithdrawals txSkelWithdrawals
  let txFee = Cardano.TxFeeExplicit Cardano.ShelleyBasedEraConway $ Cardano.Coin fee
      txMetadata = Cardano.TxMetadataNone
      txAuxScripts = Cardano.TxAuxScriptsNone
      txUpdateProposal = Cardano.TxUpdateProposalNone
      txCertificates = Cardano.TxCertificatesNone
      txScriptValidity = Cardano.TxScriptValidityNone
      txVotingProcedures = Nothing
      txCurrentTreasuryValue = Nothing
      txTreasuryDonation = Nothing
  return Cardano.TxBodyContent {..}

-- | Generates a transaction body from a body content
txBodyContentToTxBody :: (MonadBlockChainBalancing m) => Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra -> m (Cardano.TxBody Cardano.ConwayEra)
txBodyContentToTxBody txBodyContent = do
  params <- getParams
  -- We create the associated Shelley TxBody
  either
    (throwError . MCEToCardanoError "generateTx :")
    return
    (Emulator.createTransactionBody params (Ledger.CardanoBuildTx txBodyContent))

-- | Generates an index with utxos known to a 'TxSkel'
txSkelToIndex :: (MonadBlockChainBalancing m) => TxSkel -> Maybe (Set Api.TxOutRef, Wallet) -> m (Cardano.UTxO Cardano.ConwayEra)
txSkelToIndex txSkel mCollaterals = do
  -- We build the index of UTxOs which are known to this skeleton. This includes
  -- collateral inputs, inputs and reference inputs.
  let collateralIns = case mCollaterals of
        Nothing -> []
        Just (s, _) -> Set.toList s
  -- We retrieve all the outputs known to the skeleton
  (knownTxORefs, knownTxOuts) <- unzip . Map.toList <$> lookupUtxos (Set.toList (txSkelKnownTxOutRefs txSkel) <> collateralIns)
  -- We then compute their Cardano counterparts
  txOutL <- forM knownTxOuts toCardanoTxOut
  -- We build the index and handle the possible error
  either (throwError . MCEToCardanoError "txSkelToIndex:") return $ do
    txInL <- forM knownTxORefs Ledger.toCardanoTxIn
    return $ Cardano.UTxO $ Map.fromList $ zip txInL $ Cardano.toCtxUTxOTxOut <$> txOutL

-- | Generates a transaction body from a 'TxSkel' and associated fee and
-- collateral information. This transaction body accounts for the actual
-- execution units of each of the scripts involved in the skeleton.
txSkelToTxBody :: (MonadBlockChainBalancing m) => TxSkel -> Integer -> Maybe (Set Api.TxOutRef, Wallet) -> m (Cardano.TxBody Cardano.ConwayEra)
txSkelToTxBody txSkel fee mCollaterals = do
  -- We create a first body content and body, without execution units
  txBodyContent' <- txSkelToTxBodyContent txSkel fee mCollaterals
  txBody' <- txBodyContentToTxBody txBodyContent'
  -- We create a full transaction from the body
  let tx' = Cardano.Tx txBody' (toKeyWitness txBody' <$> txSkelSigners txSkel)
  -- We retrieve the index and parameters to feed to @getTxExUnitsWithLogs@
  index <- txSkelToIndex txSkel mCollaterals
  params <- getParams
  -- We retrieve the execution units associated with the transaction
  case Emulator.getTxExUnitsWithLogs params (Ledger.fromPlutusIndex index) tx' of
    -- Computing the execution units can result in all kinds of validation
    -- errors except for the ones related to the execution units themselves.
    Left err -> throwError $ uncurry MCEValidationError err
    -- When no error arises, we get an execution unit for each script usage. We
    -- first have to transform this Ledger map to a cardano API map.
    Right (Map.mapKeysMonotonic (Cardano.toScriptIndex Cardano.AlonzoEraOnwardsConway) . fmap (Cardano.fromAlonzoExUnits . snd) -> exUnits) ->
      -- We can then assign the right execution units to the body content
      case Cardano.substituteExecutionUnits exUnits txBodyContent' of
        -- This can only be a @TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap@
        Left _ -> throwError $ FailWith "Error while assigning execution units"
        -- We now have a body content with proper execution units and can create
        -- the final body from it
        Right txBody -> txBodyContentToTxBody txBody
