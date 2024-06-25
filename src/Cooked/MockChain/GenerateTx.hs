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

import Cardano.Api qualified as Cardanon
import Cardano.Api.Shelley qualified as Cardano hiding (Testnet)
import Cardano.Ledger.Address qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as Cardano
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Ledger.Conway.Governance qualified as Conway
import Cardano.Ledger.Core qualified as Cardano (emptyPParamsStrictMaybe)
import Cardano.Ledger.Credential qualified as Cardano
import Cardano.Ledger.Crypto qualified as Crypto
import Cardano.Ledger.Plutus.ExUnits qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Cooked.Conversion
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

-- * Domain for transaction generation and associated types

data GenerateTxError
  = ToCardanoError String Ledger.ToCardanoError
  | TxBodyError String Cardano.TxBodyError
  | GenerateTxErrorGeneral String
  deriving (Show, Eq)

-- | Context in which various parts of transactions will be built
data Context where
  Context ::
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
    Context

instance Default Context where
  def = Context 0 mempty Nothing def Map.empty Map.empty Map.empty

-- The domain in which transactions are generated.
type TxGen a = ReaderT Context (Either GenerateTxError) a

-- * Helpers to throw errors in 'TxGen'

-- | Looks up a key in a map. Throws a 'GenerateTxErrorGeneral' error with a given
-- message when the key is absent, returns the associated value otherwise.
throwOnLookup :: (Ord k) => String -> k -> Map k a -> TxGen a
throwOnLookup errorMsg key = maybe (throwOnString errorMsg) return . Map.lookup key

-- | Throws a general error from a String
throwOnString :: String -> TxGen a
throwOnString = lift . Left . GenerateTxErrorGeneral

-- | Lifts a 'ToCardanoError' with an associated error message, or apply a
-- function if a value exists
throwOnToCardanoErrorOrApply :: String -> (a -> b) -> Either Ledger.ToCardanoError a -> TxGen b
throwOnToCardanoErrorOrApply errorMsg f = lift . bimap (ToCardanoError errorMsg) f

-- | Lifts a 'ToCardanoError' with an associated error message, or leaves the
-- value unchanged if it exists
throwOnToCardanoError :: String -> Either Ledger.ToCardanoError a -> TxGen a
throwOnToCardanoError = flip throwOnToCardanoErrorOrApply id

-- * Generation functions

txSkelToBodyContent :: TxSkel -> TxGen (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
txSkelToBodyContent TxSkel {..} = do
  txIns <- mapM txSkelInToTxIn $ Map.toList txSkelIns
  txInsReference <- txOutRefsToTxInsReference $ mapMaybe txSkelReferenceScript (Map.elems txSkelIns) ++ Set.toList txSkelInsReference
  (txInsCollateral, txTotalCollateral, txReturnCollateral) <- toCollateralTriplet
  txOuts <- mapM txSkelOutToCardanoTxOut txSkelOuts
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

-- * Translation credentials and witnesses

-- | Translating a given credential to a reward account
generateRewardAccount :: Api.Credential -> TxGen (Cardano.RewardAcnt Crypto.StandardCrypto)
generateRewardAccount cred =
  Cardano.RewardAcnt Cardano.Testnet <$> case cred of
    Api.ScriptCredential scriptHash -> do
      Cardano.ScriptHash cHash <- throwOnToCardanoError "Unable to convert script hash." $ Ledger.toCardanoScriptHash scriptHash
      return $ Cardano.ScriptHashObj cHash
    Api.PubKeyCredential pubkeyHash -> do
      -- TODO: we take the pubkeyHash, maybe we should take the stakehash if any? I am confused about the nature of the stake address..
      Cardano.StakeKeyHash pkHash <- throwOnToCardanoError "Unable to convert private key hash." $ Ledger.toCardanoStakeKeyHash pubkeyHash
      return $ Cardano.KeyHashObj pkHash

generateScriptAndRedeemerData :: Api.SerialisedScript -> TxSkelRedeemer -> TxGen (Cardano.PlutusScriptOrReferenceInput lang, Cardano.HashableScriptData)
generateScriptAndRedeemerData script TxSkelNoRedeemer =
  return (Cardano.PScript $ Cardano.PlutusScriptSerialised script, Ledger.toCardanoScriptData $ Api.toBuiltinData ())
generateScriptAndRedeemerData script (TxSkelRedeemerForScript redeemer) =
  return (Cardano.PScript $ Cardano.PlutusScriptSerialised script, Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)
generateScriptAndRedeemerData script (TxSkelRedeemerForReferencedScript validatorOref redeemer) = do
  refScriptHash <- throwOnLookup "Can't resolve reference script utxo." validatorOref =<< asks (Map.mapMaybe (^. outputReferenceScriptL) . managedTxOuts)
  when (refScriptHash /= toScriptHash script) $ throwOnString "Wrong reference script hash."
  validatorTxIn <- throwOnToCardanoError "Unable to translate reference script utxo." $ Ledger.toCardanoTxIn validatorOref
  scriptHash <- throwOnToCardanoError "Unable to translate script hash of reference script." $ Ledger.toCardanoScriptHash refScriptHash
  return (Cardano.PReferenceScript validatorTxIn (Just scriptHash), Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)

generateScriptWitness :: (ToScript a) => a -> TxSkelRedeemer -> Cardano.ScriptDatum b -> TxGen (Cardano.ScriptWitness b Cardano.ConwayEra)
generateScriptWitness (toScript -> (Script.Versioned (Script.Script script) version)) redeemer datum =
  case version of
    Script.PlutusV1 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 x datum y Ledger.zeroExecutionUnits)
        <$> generateScriptAndRedeemerData script redeemer
    Script.PlutusV2 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 x datum y Ledger.zeroExecutionUnits)
        <$> generateScriptAndRedeemerData script redeemer
    Script.PlutusV3 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 x datum y Ledger.zeroExecutionUnits)
        <$> generateScriptAndRedeemerData script redeemer

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
txSkelProposalToGovAction :: TxSkelProposal -> TxGen (Conway.GovAction Emulator.EmulatorEra)
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
      cardanoMap <- SMap.fromList <$> mapM (\(cred, Api.Lovelace lv) -> (,Emulator.Coin lv) <$> generateRewardAccount cred) (Map.toList mapCredentialLovelace)
      return $ Conway.TreasuryWithdrawals cardanoMap sHash
    TxGovActionNoConfidence -> return $ Conway.NoConfidence SNothing -- TODO, should not be Nothing later on
    TxGovActionUpdateCommittee {} -> throwOnString "TxGovActionUpdateCommittee unsupported"
    TxGovActionNewConstitution _ -> throwOnString "TxGovActionNewConstitution unsupported"

-- | Translates a skeleton proposal into a proposal procedure
-- alongside a possible witness
txSkelProposalToProposalProcedureAndWitness :: TxSkelProposal -> TxGen (Conway.ProposalProcedure Emulator.EmulatorEra, Maybe (Cardano.ScriptWitness Cardano.WitCtxStake Cardano.ConwayEra))
txSkelProposalToProposalProcedureAndWitness txSkelProposal@TxSkelProposal {..} = do
  minDeposit <- asks (Emulator.unCoin . Lens.view Conway.ppGovActionDepositL . Emulator.emulatorPParams . params)
  cred <- generateRewardAccount $ toCredential txSkelProposalAddress
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
    Just (script, redeemer) -> Just <$> generateScriptWitness (toScript script) redeemer Cardano.NoScriptDatumForStake

-- | Translates a list of skeleton proposals into a proposal procedures
txSkelProposalsToProposalProcedures :: [TxSkelProposal] -> TxGen (Cardano.TxProposalProcedures Cardano.BuildTx Cardano.ConwayEra)
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
  flip runReaderT Context {..} . txSkelToBodyContent

-- Convert a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelIn', into a 'Cardano.TxIn', together with the appropriate witness. If
-- you add reference inputs, don't forget to also update the 'txInsReference'!
txSkelInToTxIn ::
  (Api.TxOutRef, TxSkelRedeemer) ->
  TxGen (Cardano.TxIn, Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra))
txSkelInToTxIn (txOutRef, txSkelRedeemer) = do
  txOut <- asks (Map.lookup txOutRef . managedTxOuts)
  witness <- case toCredential . Api.txOutAddress <$> txOut of
    Just (Api.PubKeyCredential _) -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    Just (Api.ScriptCredential _) -> do
      (_, script, datum) <- resolveScriptOutputOwnerAndDatum txOutRef
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> generateScriptWitness script txSkelRedeemer datum
    Nothing -> throwOnString "txSkelInToTxIn: unable to resolve input txOutRef"
  throwOnToCardanoErrorOrApply
    "txSkelIntoTxIn, translating TxOutRef"
    (,Cardano.BuildTxWith witness)
    $ Ledger.toCardanoTxIn txOutRef

resolveScriptOutputOwnerAndDatum ::
  Api.TxOutRef ->
  TxGen (Script.ValidatorHash, Script.Versioned Script.Validator, Cardano.ScriptDatum Cardano.WitCtxTxIn)
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
txOutRefsToTxInsReference :: [Api.TxOutRef] -> TxGen (Cardano.TxInsReference Cardano.BuildTx Cardano.ConwayEra)
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
  TxGen
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
txSkelMintsToTxMintValue :: TxSkelMints -> TxGen (Cardano.TxMintValue Cardano.BuildTx Cardano.ConwayEra)
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
              mintWitness <- generateScriptWitness policy redeemer Cardano.NoScriptDatumForMint
              return $ Map.insert policyId mintWitness acc
          )
          Map.empty
          (txSkelMintsToList mints)
      return $ Cardano.TxMintValue Cardano.MaryEraOnwardsConway mintVal (Cardano.BuildTxWith witnessMap)

-- Convert a 'TxSkelOut' to the corresponding 'Cardano.TxOut'.
txSkelOutToCardanoTxOut :: TxSkelOut -> TxGen (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
txSkelOutToCardanoTxOut (Pays output) = do
  networkId <- asks $ Emulator.pNetworkId . params
  address <- throwOnToCardanoError "txSkelOutToCardanoTxOut: wrong address" $ Ledger.toCardanoAddressInEra networkId (outputAddress output)
  value <- Ledger.toCardanoTxOutValue <$> throwOnToCardanoError "txSkelOutToCardanoTxOut: cannot build value" (Ledger.toCardanoValue $ outputValue output)
  datum <- case output ^. outputDatumL of
    TxSkelOutNoDatum -> return Cardano.TxOutDatumNone
    TxSkelOutDatumHash datum ->
      throwOnToCardanoError "txSkelOutToTxOut: unresolved datum hash" $
        Cardano.TxOutDatumHash Cardano.AlonzoEraOnwardsConway
          <$> Ledger.toCardanoScriptDataHash (Script.datumHash $ Api.Datum $ Api.toBuiltinData datum)
    TxSkelOutDatum datum ->
      return
        $ Cardano.TxOutDatumInTx Cardano.AlonzoEraOnwardsConway
          . Cardano.unsafeHashableScriptData
          . Cardano.fromPlutusData
          . Api.builtinDataToData
          . Api.toBuiltinData
        $ datum
    TxSkelOutInlineDatum datum ->
      return
        $ Cardano.TxOutDatumInline Cardano.BabbageEraOnwardsConway
          . Cardano.unsafeHashableScriptData
          . Cardano.fromPlutusData
          . Api.builtinDataToData
          . Api.toBuiltinData
        $ datum
  let refScript = Ledger.toCardanoReferenceScript (toScript <$> output ^. outputReferenceScriptL)
  return $ Cardano.TxOut address value datum refScript

generateTxOut :: Cardano.NetworkId -> TxSkelOut -> Either GenerateTxError (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
generateTxOut networkId =
  flip runReaderT (def {params = def {Emulator.pNetworkId = networkId}}) . txSkelOutToCardanoTxOut

txSkelToCardanoTx :: TxSkel -> TxGen (Cardano.Tx Cardano.ConwayEra)
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
  flip runReaderT Context {..} . txSkelToCardanoTx
