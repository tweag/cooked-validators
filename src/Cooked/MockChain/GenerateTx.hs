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
import Cardano.Ledger.BaseTypes qualified as Cardano
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Ledger.Conway.Governance qualified as Conway
import Cardano.Ledger.Core qualified as Cardano (emptyPParamsStrictMaybe)
import Cardano.Ledger.Plutus.ExUnits qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Cooked.Conversion
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Output
import Cooked.Skeleton
import Cooked.Wallet
import Data.Bifunctor
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as SMap
import Data.Maybe
import Data.Maybe.Strict
import Data.OSet.Strict qualified as OSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.IO.Unsafe
import Ledger.Address qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Lens.Micro qualified as MicroLens
import Network.HTTP.Simple qualified as Network
import Optics.Core
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

txSkelToBodyContent :: TxSkel -> TxGenTrans (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
txSkelToBodyContent TxSkel {..} = do
  txIns <- mapM txSkelInToTxIn $ Map.toList txSkelIns
  txInsReference <- txOutRefsToTxInsReference $ mapMaybe txSkelReferenceScript (Map.elems txSkelIns) ++ Set.toList txSkelInsReference
  (txInsCollateral, txTotalCollateral, txReturnCollateral) <- toCollateralTriplet
  txOuts <- mapM (liftTxGen . toCardanoTxOut) txSkelOuts
  (txValidityLowerBound, txValidityUpperBound) <-
    throwOnToCardanoError "translating the transaction validity range" $ Ledger.toCardanoValidityRange txSkelValidityRange
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
  txProposalProcedures <- Just . Cardano.Featured Cardano.ConwayEraOnwardsConway <$> txSkelProposalsToProposalProcedures txSkelProposals
  let txMetadata = Cardano.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = Cardano.TxAuxScriptsNone -- That's what plutus-apps does as well
      txWithdrawals = Cardano.TxWithdrawalsNone -- That's what plutus-apps does as well
      txCertificates = Cardano.TxCertificatesNone -- That's what plutus-apps does as well
      txUpdateProposal = Cardano.TxUpdateProposalNone -- That's what plutus-apps does as well
      txScriptValidity = Cardano.TxScriptValidityNone -- That's what plutus-apps does as well
      txVotingProcedures = Nothing -- TODO, same as above
  return Cardano.TxBodyContent {..}

-- * Translating proposal procedures

-- | Transorms a `TxParameterChange` into an actual change over a
-- Cardano parameter update
txParameterChangeToPParamsUpdate :: TxParameterChange -> Conway.PParamsUpdate Emulator.EmulatorEra -> Conway.PParamsUpdate Emulator.EmulatorEra
txParameterChangeToPParamsUpdate pChange =
  -- From rational to bounded rational
  let toBR :: (Cardano.BoundedRational r) => Rational -> r
      toBR = fromMaybe minBound . Cardano.boundRational
      -- Helper to set one of the param update with a lenso
      setL l = MicroLens.set l . SJust
   in case pChange of
        -- will exist later on: MinFeeRefScriptCostPerByte n -> setL Conway.ppuMinFeeRefScriptCostPerByteL $ fromIntegral n
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

-- | Translates a given skeleton proposal into a governance action
txSkelProposalToGovAction :: TxSkelProposal -> TxGenTrans (Conway.GovAction Emulator.EmulatorEra)
txSkelProposalToGovAction TxSkelProposal {..} = do
  sHash <- case txSkelProposalWitness of
    Nothing -> return SNothing
    Just (script, _) -> do
      Cardano.ScriptHash sHash <- throwOnToCardanoError "Unable to convert script hash" (Ledger.toCardanoScriptHash (toScriptHash script))
      return $ SJust sHash
  case txSkelProposalAction of
    TxGovActionParameterChange changes ->
      return $
        Conway.ParameterChange
          SNothing -- TODO, should not be Nothing later on
          (foldl (flip txParameterChangeToPParamsUpdate) (Conway.PParamsUpdate Cardano.emptyPParamsStrictMaybe) changes)
          sHash
    TxGovActionHardForkInitiation _ -> throwOnString "TxGovActionHardForkInitiation unsupported"
    TxGovActionTreasuryWithdrawals mapCredentialLovelace -> do
      cardanoMap <- SMap.fromList <$> mapM (\(cred, Api.Lovelace lv) -> (,Emulator.Coin lv) <$> liftTxGen (toRewardAccount cred)) (Map.toList mapCredentialLovelace)
      return $ Conway.TreasuryWithdrawals cardanoMap sHash
    TxGovActionNoConfidence -> return $ Conway.NoConfidence SNothing -- TODO, should not be Nothing later on
    TxGovActionUpdateCommittee {} -> throwOnString "TxGovActionUpdateCommittee unsupported"
    TxGovActionNewConstitution _ -> throwOnString "TxGovActionNewConstitution unsupported"

-- | Translates a skeleton proposal into a proposal procedure
-- alongside a possible witness
txSkelProposalToProposalProcedureAndWitness :: TxSkelProposal -> TxGenTrans (Conway.ProposalProcedure Emulator.EmulatorEra, Maybe (Cardano.ScriptWitness Cardano.WitCtxStake Cardano.ConwayEra))
txSkelProposalToProposalProcedureAndWitness txSkelProposal@TxSkelProposal {..} = do
  minDeposit <- asks (Emulator.unCoin . Lens.view Conway.ppGovActionDepositL . Emulator.emulatorPParams . params)
  cred <- liftTxGen $ toRewardAccount $ toCredential txSkelProposalAddress
  govAction <- txSkelProposalToGovAction txSkelProposal
  let proposalAnchor = do
        anchor <- txSkelProposalAnchor
        anchorUrl <- Cardano.textToUrl (length anchor) (Text.pack anchor)
        let anchorDataHash =
              handle
                (return . throwOnString . (("Error when parsing anchor " ++ show anchor ++ " with error: ") ++) . (show @Network.HttpException))
                ((Network.parseRequest anchor >>= Network.httpBS) <&> return . Cardano.hashAnchorData . Cardano.AnchorData . Network.getResponseBody)
        return $ Cardano.Anchor anchorUrl <$> unsafePerformIO anchorDataHash
  anchor <- fromMaybe (return def) proposalAnchor
  let conwayProposalProcedure = Conway.ProposalProcedure (Emulator.Coin minDeposit) cred govAction anchor
  (conwayProposalProcedure,) <$> case txSkelProposalWitness of
    Nothing -> return Nothing
    Just (script, redeemer) -> Just <$> liftTxGen (toScriptWitness (toScript script) redeemer Cardano.NoScriptDatumForStake)

-- | Translates a list of skeleton proposals into a proposal procedures
txSkelProposalsToProposalProcedures :: [TxSkelProposal] -> TxGenTrans (Cardano.TxProposalProcedures Cardano.BuildTx Cardano.ConwayEra)
txSkelProposalsToProposalProcedures props = do
  (OSet.fromSet -> ppSet, Cardano.BuildTxWith -> ppMap) <- go props
  return $
    if null ppSet
      then Cardano.TxProposalProceduresNone
      else Cardano.TxProposalProcedures ppSet ppMap
  where
    go [] = return (Set.empty, Map.empty)
    go (h : t) = do
      (proposals, mapWitnesses) <- go t
      (proposal, maybeWitness) <- txSkelProposalToProposalProcedureAndWitness h
      let outputMap = case maybeWitness of
            Nothing -> mapWitnesses
            Just newWitness -> Map.insert proposal newWitness mapWitnesses
      return (Set.insert proposal proposals, outputMap)

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

-- Convert a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelIn', into a 'Cardano.TxIn', together with the appropriate witness. If
-- you add reference inputs, don't forget to also update the 'txInsReference'!
txSkelInToTxIn ::
  (Api.TxOutRef, TxSkelRedeemer) ->
  TxGenTrans (Cardano.TxIn, Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra))
txSkelInToTxIn (txOutRef, txSkelRedeemer) = do
  txOut <- asks (Map.lookup txOutRef . managedTxOuts)
  witness <- case toCredential . Api.txOutAddress <$> txOut of
    Just (Api.PubKeyCredential _) -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    Just (Api.ScriptCredential _) -> do
      (_, script, datum) <- resolveScriptOutputOwnerAndDatum txOutRef
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> liftTxGen (toScriptWitness script txSkelRedeemer datum)
    Nothing -> throwOnString "txSkelInToTxIn: unable to resolve input txOutRef"
  throwOnToCardanoErrorOrApply
    "txSkelIntoTxIn, translating TxOutRef"
    (,Cardano.BuildTxWith witness)
    $ Ledger.toCardanoTxIn txOutRef

resolveScriptOutputOwnerAndDatum ::
  Api.TxOutRef ->
  TxGenTrans (Script.ValidatorHash, Script.Versioned Script.Validator, Cardano.ScriptDatum Cardano.WitCtxTxIn)
resolveScriptOutputOwnerAndDatum txOutRef = do
  txOut <- throwOnLookup "txSkelInToTxIn: Unknown txOutRef" txOutRef =<< asks managedTxOuts
  validatorHash <-
    case outputAddress txOut of
      (Api.Address (Api.ScriptCredential (Api.ScriptHash validatorHash)) _) -> return $ Script.ValidatorHash validatorHash
      _ -> throwOnString $ "txSkelInToTxIn: Output is not a script output" <> show txOut
  validator <- throwOnLookup "txSkelInToTxIn: Unknown validator" validatorHash =<< asks managedValidators
  datum <-
    case outputOutputDatum txOut of
      Api.NoOutputDatum -> throwOnString "txSkelInToTxIn: No datum found on script output"
      Api.OutputDatum _ -> return Cardano.InlineScriptDatum
      Api.OutputDatumHash datumHash -> do
        datum <- throwOnLookup "txSkelInToTxIn: Datum hash could not be resolved" datumHash =<< asks managedData
        return $ Cardano.ScriptDatumForTxIn $ Ledger.toCardanoScriptData $ Api.getDatum datum
  return (validatorHash, validator, datum)

-- Convert a list of 'Api.TxOutRef' into a 'Cardano.TxInsReference'
txOutRefsToTxInsReference :: [Api.TxOutRef] -> TxGenTrans (Cardano.TxInsReference Cardano.BuildTx Cardano.ConwayEra)
txOutRefsToTxInsReference =
  throwOnToCardanoErrorOrApply
    "txOutRefsToTxInsReference"
    ( \case
        [] -> Cardano.TxInsReferenceNone
        txIns -> Cardano.TxInsReference Cardano.BabbageEraOnwardsConway txIns
    )
    . mapM Ledger.toCardanoTxIn

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
