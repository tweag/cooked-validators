module Cooked.MockChain.GenerateTx.Body where

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
import Control.Monad.Reader
import Cooked.MockChain.GenerateTx.Collateral qualified as Collateral
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Input qualified as Input
import Cooked.MockChain.GenerateTx.Mint qualified as Mint
import Cooked.MockChain.GenerateTx.Output qualified as Output
import Cooked.MockChain.GenerateTx.Proposal qualified as Proposal
import Cooked.MockChain.GenerateTx.Withdrawals qualified as Withdrawals
import Cooked.Skeleton
import Cooked.Wallet
import Data.Either.Combinators
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Address qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

data TxContext where
  TxContext ::
    { fee :: Integer,
      mCollaterals :: Maybe (Set Api.TxOutRef, Wallet),
      params :: Emulator.Params,
      managedData :: Map Api.DatumHash Api.Datum,
      managedTxOuts :: Map Api.TxOutRef Api.TxOut,
      managedValidators :: Map Script.ValidatorHash (Script.Versioned Script.Validator)
    } ->
    TxContext

type BodyGen a = TxGen TxContext a

instance Transform TxContext Cardano.NetworkId where
  transform = Emulator.pNetworkId . params

instance Transform TxContext (Map Api.TxOutRef Api.TxOut) where
  transform = managedTxOuts

instance Transform TxContext (Emulator.PParams, Map Api.TxOutRef Api.TxOut) where
  transform ctx = (Emulator.pEmulatorPParams $ params ctx, transform ctx)

instance Transform TxContext Input.InputContext where
  transform TxContext {..} = Input.InputContext {..}

instance Transform TxContext Collateral.CollateralContext where
  transform TxContext {..} = Collateral.CollateralContext {..}

instance Transform TxContext Withdrawals.WithdrawalsContext where
  transform TxContext {..} =
    let networkId = Emulator.pNetworkId params
     in Withdrawals.WithdrawalsContext {..}

-- | Generates a body content from a skeleton
txSkelToBodyContent :: TxSkel -> BodyGen (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
txSkelToBodyContent skel@TxSkel {..} | txSkelReferenceInputs <- txSkelReferenceTxOutRefs skel = do
  txIns <- mapM (liftTxGen . Input.toTxInAndWitness) $ Map.toList txSkelIns
  txInsReference <-
    if null txSkelReferenceInputs
      then return Cardano.TxInsReferenceNone
      else
        throwOnToCardanoErrorOrApply
          "txSkelToBodyContent: Unable to translate reference inputs."
          (Cardano.TxInsReference Cardano.BabbageEraOnwardsConway)
          $ mapM Ledger.toCardanoTxIn txSkelReferenceInputs
  (txInsCollateral, txTotalCollateral, txReturnCollateral) <- liftTxGen Collateral.toCollateralTriplet
  txOuts <- mapM (liftTxGen . Output.toCardanoTxOut) txSkelOuts
  (txValidityLowerBound, txValidityUpperBound) <-
    throwOnToCardanoError
      "txSkelToBodyContent: Unable to translate transaction validity range."
      $ Ledger.toCardanoValidityRange txSkelValidityRange
  txMintValue <- liftTxGen $ Mint.toMintValue txSkelMints
  txExtraKeyWits <-
    if null txSkelSigners
      then return Cardano.TxExtraKeyWitnessesNone
      else
        throwOnToCardanoErrorOrApply
          "txSkelToBodyContent: Unable to translate the required signers"
          (Cardano.TxExtraKeyWitnesses Cardano.AlonzoEraOnwardsConway)
          $ mapM (Ledger.toCardanoPaymentKeyHash . Ledger.PaymentPubKeyHash . walletPKHash) txSkelSigners
  txProtocolParams <- asks (Cardano.BuildTxWith . Just . Emulator.ledgerProtocolParameters . params)
  txFee <- asks (Cardano.TxFeeExplicit Cardano.ShelleyBasedEraConway . Emulator.Coin . fee)
  txProposalProcedures <-
    Just . Cardano.Featured Cardano.ConwayEraOnwardsConway
      <$> liftTxGen (Proposal.toProposalProcedures txSkelProposals (txOptAnchorResolution txSkelOpts))
  txWithdrawals <- liftTxGen (Withdrawals.toWithdrawals txSkelWithdrawals)
  let txMetadata = Cardano.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = Cardano.TxAuxScriptsNone -- That's what plutus-apps does as well
      txUpdateProposal = Cardano.TxUpdateProposalNone -- That's what plutus-apps does as well
      txCertificates = Cardano.TxCertificatesNone -- That's what plutus-apps does as well
      txScriptValidity = Cardano.TxScriptValidityNone -- That's what plutus-apps does as well
      txVotingProcedures = Nothing
      txCurrentTreasuryValue = Nothing
      txTreasuryDonation = Nothing
  return Cardano.TxBodyContent {..}

-- | Generates a transaction for a skeleton. We first generate a body and we
-- sign it with the required signers.
txSkelToCardanoTx :: TxSkel -> BodyGen (Cardano.Tx Cardano.ConwayEra)
txSkelToCardanoTx txSkel = do
  -- We begin by creating the body content of the transaction
  txBodyContent <- txSkelToBodyContent txSkel

  -- We create the associated Shelley TxBody
  txBody@(Cardano.ShelleyTxBody a body c dats e f) <-
    lift $ mapLeft (TxBodyError "generateTx :") $ Cardano.createTransactionBody Cardano.ShelleyBasedEraConway txBodyContent

  -- There is a chance that the body is in need of additional data. This happens
  -- when the set of reference inputs contains hashed datums that will need to
  -- be resolved during phase 2 validation. All that follows until the
  -- definition of "txBody'" aims at doing just that. In the process, we have to
  -- reconstruct the body with the new data and the associated hash. Hopefully,
  -- in the future, cardano-api provides a way to add those data in the body
  -- directly without requiring this method, which somewhat feels like a hack.

  -- We retrieve the data available in the context
  mData <- asks managedData
  -- We retrieve the outputs available in the context
  mTxOut <- asks managedTxOuts
  -- We attempt to resolve the reference inputs used by the skeleton
  refIns <- forM (txSkelReferenceTxOutRefs txSkel) $ \oRef ->
    throwOnLookup ("txSkelToCardanoTx: Unable to resolve TxOutRef " <> show oRef) oRef mTxOut
  -- We collect the datum hashes present at these outputs
  let datumHashes = [hash | (Api.TxOut _ _ (Api.OutputDatumHash hash) _) <- refIns]
  -- We resolve those datum hashes from the context
  additionalData <- forM datumHashes $ \dHash ->
    throwOnLookup ("txSkelToCardanoTx: Unable to resolve datum hash " <> show dHash) dHash mData
  -- We compute the map from datum hash to datum of these additional required data
  let additionalDataMap = Map.fromList [(Cardano.hashData dat, dat) | Api.Datum (Cardano.Data . Api.toData -> dat) <- additionalData]
  -- We retrieve a needed parameter to process difference plutus languages
  toLangDepViewParam <- asks (Conway.getLanguageView . Cardano.unLedgerProtocolParameters . Emulator.ledgerProtocolParameters . params)
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

  -- We return the transaction signed by all the required signers. The body is
  -- chosen based on whether or not it required additional data.
  return $
    Ledger.getEmulatorEraTx $
      foldl
        (flip Ledger.addCardanoTxWitness)
        (Ledger.CardanoEmulatorEraTx $ Cardano.Tx (if null additionalDataMap then txBody else txBody') [])
        (Ledger.toWitness . Ledger.PaymentPrivateKey . walletSK <$> txSkelSigners txSkel)
  where
    toCardanoLanguage :: Cardano.PlutusScriptVersion lang -> Cardano.Language
    toCardanoLanguage = \case
      Cardano.PlutusScriptV1 -> Cardano.PlutusV1
      Cardano.PlutusScriptV2 -> Cardano.PlutusV2
      Cardano.PlutusScriptV3 -> Cardano.PlutusV3
