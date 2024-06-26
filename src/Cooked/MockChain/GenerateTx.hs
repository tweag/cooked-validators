-- | This module provides primitives to translate elements from our skeleton to
-- actual transaction elements, including the transaction itself. Ideally, this
-- module should only export `generateTx` but we need to make visible a few
-- other primitives that will be used in balancing.
module Cooked.MockChain.GenerateTx
  ( GenerateTxError (..),
    generateBodyContent,
    generateTxOut,
    generateTx,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Control.Monad.Reader
import Cooked.MockChain.GenerateTx.Collateral qualified as Collateral
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Input qualified as Input
import Cooked.MockChain.GenerateTx.Output qualified as Output
import Cooked.MockChain.GenerateTx.Proposal
import Cooked.MockChain.GenerateTx.Witness
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

type TxGenTrans a = TxGen TxContext a

-- | Generates a Cardano `TxOut` from a `TxSkelOut` and the network
-- Id. This runs the generation and is meant to be used outside of the
-- regular transaction generation process.
generateTxOut :: Cardano.NetworkId -> TxSkelOut -> Either GenerateTxError (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
generateTxOut networkId txSkelOut = runReaderT (Output.toCardanoTxOut txSkelOut) networkId

-- * Generation functions

-- | Context in which various parts of transactions will be built
data TxContext where
  TxContext ::
    { -- | fee to apply to body generation
      fee :: Integer,
      -- | collaterals to add to body generation
      collateralIns :: Set Api.TxOutRef,
      -- | wallet to return collaterals to
      returnCollateralWallet :: Maybe Wallet,
      -- | parameters of the emulator
      params :: Emulator.Params,
      -- | datums present in our environment
      managedData :: Map Api.DatumHash Api.Datum,
      -- | txouts present in our environment
      managedTxOuts :: Map Api.TxOutRef Api.TxOut,
      -- | validators present in our environment
      managedValidators :: Map Script.ValidatorHash (Script.Versioned Script.Validator)
    } ->
    TxContext

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

txSkelToBodyContent :: TxSkel -> TxGenTrans (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
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
      "txSkelToBodyContent: Unable to translate transaction validity range"
      $ Ledger.toCardanoValidityRange txSkelValidityRange
  txMintValue <- txSkelMintsToTxMintValue txSkelMints
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
  txProposalProcedures <- Just . Cardano.Featured Cardano.ConwayEraOnwardsConway <$> liftTxGen (toProposalProcedures txSkelProposals)
  let txMetadata = Cardano.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = Cardano.TxAuxScriptsNone -- That's what plutus-apps does as well
      txWithdrawals = Cardano.TxWithdrawalsNone -- That's what plutus-apps does as well
      txCertificates = Cardano.TxCertificatesNone -- That's what plutus-apps does as well
      txUpdateProposal = Cardano.TxUpdateProposalNone -- That's what plutus-apps does as well
      txScriptValidity = Cardano.TxScriptValidityNone -- That's what plutus-apps does as well
      txVotingProcedures = Nothing -- TODO, same as above
  return Cardano.TxBodyContent {..}

generateBodyContent ::
  Integer ->
  Wallet ->
  Set Api.TxOutRef ->
  Emulator.Params ->
  Map Api.DatumHash Api.Datum ->
  Map Api.TxOutRef Api.TxOut ->
  Map Script.ValidatorHash (Script.Versioned Script.Validator) ->
  TxSkel ->
  Either GenerateTxError (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
generateBodyContent fee (Just -> returnCollateralWallet) collateralIns params managedData managedTxOuts managedValidators =
  flip runReaderT TxContext {..} . txSkelToBodyContent

-- Convert the 'TxSkelMints' into a 'TxMintValue'
txSkelMintsToTxMintValue :: TxSkelMints -> TxGenTrans (Cardano.TxMintValue Cardano.BuildTx Cardano.ConwayEra)
txSkelMintsToTxMintValue mints =
  if mints == Map.empty
    then return Cardano.TxMintNone
    else do
      mintVal <-
        throwOnToCardanoError "txSkelMintsToTxMintValue, translating minted value" $ Ledger.toCardanoValue $ txSkelMintsValue mints
      witnessMap <-
        foldM
          ( \acc (policy, redeemer, _tName, _amount) -> do
              policyId <-
                throwOnToCardanoError
                  "txSkelMintsToTxMintValue, calculating the witness map"
                  (Ledger.toCardanoPolicyId (Script.mintingPolicyHash policy))
              mintWitness <- liftTxGen $ toScriptWitness policy redeemer Cardano.NoScriptDatumForMint
              return $ Map.insert policyId mintWitness acc
          )
          Map.empty
          (txSkelMintsToList mints)
      return $ Cardano.TxMintValue Cardano.MaryEraOnwardsConway mintVal (Cardano.BuildTxWith witnessMap)

txSkelToCardanoTx :: TxSkel -> TxGenTrans (Cardano.Tx Cardano.ConwayEra)
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

generateTx ::
  Integer ->
  Wallet ->
  Set Api.TxOutRef ->
  Emulator.Params ->
  Map Script.DatumHash Script.Datum ->
  Map Api.TxOutRef Api.TxOut ->
  Map Script.ValidatorHash (Script.Versioned Script.Validator) ->
  TxSkel ->
  Either GenerateTxError (Cardano.Tx Cardano.ConwayEra)
generateTx fee (Just -> returnCollateralWallet) collateralIns params managedData managedTxOuts managedValidators =
  flip runReaderT TxContext {..} . txSkelToCardanoTx
