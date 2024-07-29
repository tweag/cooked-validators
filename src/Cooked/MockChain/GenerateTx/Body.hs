module Cooked.MockChain.GenerateTx.Body where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Control.Monad.Reader
import Cooked.MockChain.GenerateTx.Collateral qualified as Collateral
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Input qualified as Input
import Cooked.MockChain.GenerateTx.Mint qualified as Mint
import Cooked.MockChain.GenerateTx.Output qualified as Output
import Cooked.MockChain.GenerateTx.Proposal qualified as Proposal
import Cooked.Skeleton
import Cooked.Wallet
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
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
  let txMetadata = Cardano.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = Cardano.TxAuxScriptsNone -- That's what plutus-apps does as well
      txWithdrawals = Cardano.TxWithdrawalsNone -- That's what plutus-apps does as well
      txCertificates = Cardano.TxCertificatesNone -- That's what plutus-apps does as well
      txUpdateProposal = Cardano.TxUpdateProposalNone -- That's what plutus-apps does as well
      txScriptValidity = Cardano.TxScriptValidityNone -- That's what plutus-apps does as well
      txVotingProcedures = Nothing
  return Cardano.TxBodyContent {..}

-- | Generates a transaction for a skeleton. We first generate a body and we
-- sign it with the required signers.
txSkelToCardanoTx :: TxSkel -> BodyGen (Cardano.Tx Cardano.ConwayEra)
txSkelToCardanoTx txSkel = do
  txBodyContent <- txSkelToBodyContent txSkel
  cardanoTxUnsigned <-
    lift $
      bimap
        (TxBodyError "generateTx: ")
        (`Cardano.Tx` [])
        (Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway txBodyContent)
  foldM
    ( \tx wal ->
        case Ledger.addCardanoTxWitness (Ledger.toWitness $ Ledger.PaymentPrivateKey $ walletSK wal) (Ledger.CardanoTx tx Cardano.ShelleyBasedEraConway) of
          Ledger.CardanoTx tx' Cardano.ShelleyBasedEraConway -> return tx'
          _ -> throwOnString "txSkelToCardanoTx: Wrong output era"
    )
    cardanoTxUnsigned
    (txSkelSigners txSkel)
