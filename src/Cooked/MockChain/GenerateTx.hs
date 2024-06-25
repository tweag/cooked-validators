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
import Cardano.Api.Shelley qualified as Cardano hiding (Testnet)
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Control.Monad.Reader
import Cooked.Conversion
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Input qualified as Input
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.GenerateTx.Proposal
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Cooked.Wallet
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Address qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as PlutusTx

type TxGenTrans a = TxGen TxContext a

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
  (txInsCollateral, txTotalCollateral, txReturnCollateral) <- toCollateralTriplet
  txOuts <- mapM (liftTxGen . toCardanoTxOut) txSkelOuts
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
          "translating the required signers"
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

-- | Computes the collateral triplet from the fees and the collateral inputs in
-- the context. What we call a collateral triplet is composed of:
-- * The set of collateral inputs
-- * The total collateral paid by the transaction in case of phase 2 failure
-- * An output returning excess collateral value when collaterals are used
-- These quantity should satisfy the equation (in terms of their values):
-- collateral inputs = total collateral + return collateral
toCollateralTriplet ::
  TxGenTrans
    ( Cardano.TxInsCollateral Cardano.ConwayEra,
      Cardano.TxTotalCollateral Cardano.ConwayEra,
      Cardano.TxReturnCollateral Cardano.CtxTx Cardano.ConwayEra
    )
toCollateralTriplet = do
  -- Retrieving know outputs
  knownTxOuts <- asks managedTxOuts
  -- Retrieving the outputs to be used as collateral inputs
  collateralInsList <- asks (Set.toList . collateralIns)
  -- We build the collateral inputs from this list
  txInsCollateral <-
    case collateralInsList of
      [] -> return Cardano.TxInsCollateralNone
      l -> throwOnToCardanoError "txOutRefsToTxInCollateral" (Cardano.TxInsCollateral Cardano.AlonzoEraOnwardsConway <$> mapM Ledger.toCardanoTxIn l)
  -- Retrieving the total value in collateral inputs. This fails if one of the
  -- collaterals has been been successfully resolved.
  collateralInsValue <- do
    let collateralInsResolved = mapMaybe (`Map.lookup` knownTxOuts) collateralInsList
    when (length collateralInsResolved /= length collateralInsList) $ throwOnString "toCollateralTriplet: unresolved txOutRefs"
    return $ mconcat (Api.txOutValue <$> collateralInsResolved)
  -- We retrieve the collateral percentage compared to fees. By default, we use
  -- 150% which is the current value in the parameters, although the default
  -- value should never be used here, as the call is supposed to always succeed.
  collateralPercentage <- asks (toInteger . fromMaybe 150 . Cardano.protocolParamCollateralPercent . Emulator.pProtocolParams . params)
  -- The total collateral corresponds to the fees multiplied by the collateral
  -- percentage. We add 1 because the ledger apparently rounds up this value.
  coinTotalCollateral <- asks (Emulator.Coin . (+ 1) . (`div` 100) . (* collateralPercentage) . fee)
  -- We create the total collateral based on the computed value
  let txTotalCollateral = Cardano.TxTotalCollateral Cardano.BabbageEraOnwardsConway coinTotalCollateral
  -- We compute a return collateral value by subtracting the total collateral to
  -- the value in collateral inputs
  let returnCollateralValue = collateralInsValue <> PlutusTx.negate (toValue coinTotalCollateral)
  -- This should never happen, as we always compute the collaterals for the
  -- user, but we guard against having some negative elements in the value in
  -- case we give more freedom to the users in the future
  when (fst (Api.split returnCollateralValue) /= mempty) $ throwOnString "toCollateralTriplet: negative parts in return collateral value"
  -- The return collateral is then computed
  txReturnCollateral <-
    -- If the total collateral equal what the inputs provide, we return `None`
    if returnCollateralValue == mempty
      then return Cardano.TxReturnCollateralNone
      else -- Otherwise, we compute the elements of a new output
      do
        -- The value is a translation of the remaining value
        txReturnCollateralValue <-
          Ledger.toCardanoTxOutValue
            <$> throwOnToCardanoError
              "toCollateralTriplet: cannot build return collateral value"
              (Ledger.toCardanoValue returnCollateralValue)
        -- The address is the one from the return collateral wallet, which is
        -- required to exist here.
        address <- do
          mReturnCollateralWallet <- asks returnCollateralWallet
          case mReturnCollateralWallet of
            Nothing -> throwOnString "toCollateralTriplet: unable to find a return collateral wallet"
            Just returnCollateralWallet -> do
              networkId <- asks (Emulator.pNetworkId . params)
              throwOnToCardanoError "toCollateralTriplet: cannot build return collateral address" $
                Ledger.toCardanoAddressInEra networkId (walletAddress returnCollateralWallet)
        -- The return collateral is built up from those elements
        return $
          Cardano.TxReturnCollateral Cardano.BabbageEraOnwardsConway $
            Cardano.TxOut address txReturnCollateralValue Cardano.TxOutDatumNone Cardano.ReferenceScriptNone
  return (txInsCollateral, txTotalCollateral, txReturnCollateral)

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
