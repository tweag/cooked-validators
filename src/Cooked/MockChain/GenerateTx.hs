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
import Cardano.Ledger.Conway.PParams qualified as Conway
import Cardano.Ledger.Core qualified as Cardano (emptyPParamsStrictMaybe)
import Cardano.Ledger.Credential qualified as Cardano
import Cardano.Ledger.Plutus.ExUnits qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
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
import Network.Download qualified as Network
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

-- Looks up a key in a map. Throws a 'GenerateTxErrorGeneral' error with a given
-- message when the key is absent, returns the associated value otherwise.
throwOnLookup :: (Ord k) => String -> k -> Map k a -> TxGen a
throwOnLookup errorMsg key = maybe (throwOnString errorMsg) return . Map.lookup key

-- Throws a general error from a String
throwOnString :: String -> TxGen a
throwOnString = lift . Left . GenerateTxErrorGeneral

-- Lifts a 'ToCardanoError' with an associated error message, or apply a
-- function if a value exists
throwOnToCardanoErrorOrApply :: String -> (a -> b) -> Either Ledger.ToCardanoError a -> TxGen b
throwOnToCardanoErrorOrApply errorMsg f = lift . bimap (ToCardanoError errorMsg) f

-- Lifts a 'ToCardanoError' with an associated error message, or leaves the
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
  txProposalProcedures <- txSkelProposalsToProposalProcedures txSkelProposals
  let txMetadata = Cardano.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = Cardano.TxAuxScriptsNone -- That's what plutus-apps does as well
      txWithdrawals = Cardano.TxWithdrawalsNone -- That's what plutus-apps does as well
      txCertificates = Cardano.TxCertificatesNone -- That's what plutus-apps does as well
      txUpdateProposal = Cardano.TxUpdateProposalNone -- That's what plutus-apps does as well
      txScriptValidity = Cardano.TxScriptValidityNone -- That's what plutus-apps does as well
      txVotingProcedures = Nothing -- TODO, same as above
  return Cardano.TxBodyContent {..}

txSkelProposalsToProposalProcedures ::
  [TxSkelProposal] ->
  TxGen
    (Maybe (Cardano.Featured Cardano.ConwayEraOnwards Cardano.ConwayEra (Cardano.TxProposalProcedures Cardano.BuildTx Cardano.ConwayEra)))
txSkelProposalsToProposalProcedures [] = return Nothing -- what would be the difference with
-- return $ Just $ Cardano.Featured Cardano.ConwayEraOnwardsConway Cardano.TxProposalProceduresNone
txSkelProposalsToProposalProcedures props = do
  minDeposit <- asks (Emulator.unCoin . Lens.view Conway.ppGovActionDepositL . Emulator.emulatorPParams . params)
  (ppSet, ppMap) <- fromProposals props minDeposit
  return $ Just $ Cardano.Featured Cardano.ConwayEraOnwardsConway $ Cardano.TxProposalProcedures (OSet.fromSet ppSet) (Cardano.BuildTxWith ppMap)
  where
    fromProposals ::
      [TxSkelProposal] ->
      Integer ->
      TxGen
        ( Set (Conway.ProposalProcedure (Cardano.ShelleyLedgerEra Cardano.ConwayEra)),
          Map (Conway.ProposalProcedure (Cardano.ShelleyLedgerEra Cardano.ConwayEra)) (Cardano.ScriptWitness Cardano.WitCtxStake Cardano.ConwayEra)
        )
    fromProposals [] _ = return (Set.empty, Map.empty)
    fromProposals (h : t) minDeposit = do
      (proposals, mapWitnesses) <- fromProposals t minDeposit
      (proposal, maybeWitness) <- fromProposal h minDeposit
      let outputMap = case maybeWitness of
            Nothing -> mapWitnesses
            Just newWitness -> Map.insert proposal newWitness mapWitnesses
      return (Set.insert proposal proposals, outputMap)

    fromProposal txSkelProposal@TxSkelProposal {..} minDeposit = do
      conwayProposalProcedure <- toConwayProposalProcedure txSkelProposal minDeposit
      (conwayProposalProcedure,) <$> case txSkelProposalWitness of
        Nothing -> return Nothing
        Just (script, redeemer) -> do
          witness <- scriptToScriptWitness (toScript script) redeemer Cardano.NoScriptDatumForStake
          return $ Just witness

    toRewardAccount cred =
      Cardano.RewardAcnt Cardano.Testnet <$> case cred of
        Api.ScriptCredential scriptHash -> do
          Cardano.ScriptHash cHash <- throwOnToCardanoError "unable to convert script hash" $ Ledger.toCardanoScriptHash scriptHash
          return $ Cardano.ScriptHashObj cHash
        Api.PubKeyCredential pubkeyHash -> do
          Cardano.StakeKeyHash pkHash <- throwOnToCardanoError "unable to convert address" $ Ledger.toCardanoStakeKeyHash pubkeyHash
          return $ Cardano.KeyHashObj pkHash

    toConwayProposalProcedure txSkelProposal@TxSkelProposal {..} minDeposit = do
      cred <- toRewardAccount $ toCredential txSkelProposalAddress
      govAction <- toGovAction txSkelProposal
      return $
        Conway.ProposalProcedure @Emulator.EmulatorEra
          (Emulator.Coin (fromMaybe minDeposit txSkelProposalDeposit))
          cred
          govAction
          ( maybe
              def
              ( \s ->
                  maybe
                    def
                    ( \url ->
                        Cardano.Anchor
                          { anchorUrl = url,
                            anchorDataHash = unsafePerformIO $ do
                              Right page <- Network.openURI s
                              return $ Cardano.hashAnchorData $ Cardano.AnchorData page
                          }
                    )
                    (Cardano.textToUrl (length s) (Text.pack s))
              )
              txSkelProposalAnchor
          )

    toGovAction TxSkelProposal {..} = do
      sHash <- case txSkelProposalWitness of
        Nothing -> return SNothing
        Just (script, _) -> do
          Cardano.ScriptHash sHash <- throwOnToCardanoError "" (Ledger.toCardanoScriptHash (toScriptHash script))
          return $ SJust sHash
      case txSkelProposalAction of
        TxGovActionParameterChange changes ->
          return $
            Conway.ParameterChange
              SNothing
              (foldl (flip toParameterChange) (Conway.PParamsUpdate Cardano.emptyPParamsStrictMaybe) changes)
              sHash
        TxGovActionHardForkInitiation protocolVersion -> undefined
        TxGovActionTreasuryWithdrawals mapCredentialLovelace -> do
          cardanoMap <- SMap.fromList <$> mapM (\(cred, Api.Lovelace lv) -> (,Emulator.Coin lv) <$> toRewardAccount cred) (Map.toList mapCredentialLovelace)
          return $ Conway.TreasuryWithdrawals cardanoMap sHash
        TxGovActionNoConfidence -> return $ Conway.NoConfidence SNothing
        TxGovActionUpdateCommittee coldCommitteeCredentialList mapColdCommitteeCredentialInteger rational -> undefined
        TxGovActionNewConstitution constitution -> undefined

    toParameterChange :: TxParameterChange -> Conway.PParamsUpdate Emulator.EmulatorEra -> Conway.PParamsUpdate Emulator.EmulatorEra
    toParameterChange pChange =
      let justFromIntegral :: (Integral a, Num b) => a -> StrictMaybe b
          justFromIntegral = SJust . fromIntegral

          toBoundedRational :: (Cardano.BoundedRational r) => Rational -> r
          toBoundedRational = fromMaybe minBound . Cardano.boundRational
       in case pChange of
            FeePerByte n -> MicroLens.set Conway.ppuMinFeeAL $ justFromIntegral n
            FeeFixed n -> MicroLens.set Conway.ppuMinFeeBL $ justFromIntegral n
            MaxBlockBodySize n -> MicroLens.set Conway.ppuMaxBBSizeL $ justFromIntegral n
            MaxTxSize n -> MicroLens.set Conway.ppuMaxTxSizeL $ justFromIntegral n
            MaxBlockHeaderSize n -> MicroLens.set Conway.ppuMaxBHSizeL $ justFromIntegral n
            KeyDeposit n -> MicroLens.set Conway.ppuKeyDepositL $ justFromIntegral n
            PoolDeposit n -> MicroLens.set Conway.ppuPoolDepositL $ justFromIntegral n
            PoolRetirementMaxEpoch n -> MicroLens.set Conway.ppuEMaxL $ SJust $ Cardano.EpochInterval $ fromIntegral n
            PoolNumber n -> MicroLens.set Conway.ppuNOptL $ justFromIntegral n
            PoolInfluence q -> MicroLens.set Conway.ppuA0L $ SJust $ fromMaybe minBound $ Cardano.boundRational q
            MonetaryExpansion q -> MicroLens.set Conway.ppuRhoL $ SJust $ fromMaybe minBound $ Cardano.boundRational q
            TreasuryCut q -> MicroLens.set Conway.ppuTauL $ SJust $ toBoundedRational q
            ProtocolVersion _ _ -> id -- Non updatable in Conway
            MinPoolCost n -> MicroLens.set Conway.ppuMinPoolCostL $ justFromIntegral n
            CoinsPerUTxOByte n -> MicroLens.set Conway.ppuCoinsPerUTxOByteL $ SJust $ Conway.CoinPerByte $ fromIntegral n
            Prices q r -> MicroLens.set Conway.ppuPricesL $ SJust $ Cardano.Prices (toBoundedRational q) (toBoundedRational r)
            MaxTxExUnits n m -> MicroLens.set Conway.ppuMaxTxExUnitsL $ SJust $ Cardano.ExUnits (fromIntegral n) (fromIntegral m)
            MaxBlockExUnits n m -> MicroLens.set Conway.ppuMaxBlockExUnitsL $ SJust $ Cardano.ExUnits (fromIntegral n) (fromIntegral m)
            MaxValSize n -> MicroLens.set Conway.ppuMaxValSizeL $ justFromIntegral n
            CollateralPercentage n -> MicroLens.set Conway.ppuCollateralPercentageL $ justFromIntegral n
            MaxCollateralInputs n -> MicroLens.set Conway.ppuMaxCollateralInputsL $ justFromIntegral n
            PoolVotingThresholds a b c d e ->
              MicroLens.set Conway.ppuPoolVotingThresholdsL $
                SJust $
                  Conway.PoolVotingThresholds
                    (toBoundedRational a)
                    (toBoundedRational b)
                    (toBoundedRational c)
                    (toBoundedRational d)
                    (toBoundedRational e)
            DRepVotingThresholds a b c d e f g h i j ->
              MicroLens.set Conway.ppuDRepVotingThresholdsL $
                SJust $
                  Conway.DRepVotingThresholds
                    (toBoundedRational a)
                    (toBoundedRational b)
                    (toBoundedRational c)
                    (toBoundedRational d)
                    (toBoundedRational e)
                    (toBoundedRational f)
                    (toBoundedRational g)
                    (toBoundedRational h)
                    (toBoundedRational i)
                    (toBoundedRational j)
            CommitteeMinSize n -> MicroLens.set Conway.ppuCommitteeMinSizeL $ justFromIntegral n
            CommitteeMaxTermLength n -> MicroLens.set Conway.ppuCommitteeMaxTermLengthL $ SJust $ Cardano.EpochInterval $ fromIntegral n
            GovActionLifetime n -> MicroLens.set Conway.ppuGovActionLifetimeL $ SJust $ Cardano.EpochInterval $ fromIntegral n
            GovActionDeposit n -> MicroLens.set Conway.ppuGovActionDepositL $ justFromIntegral n
            DRepRegistrationDeposit n -> MicroLens.set Conway.ppuDRepDepositL $ justFromIntegral n
            DRepActivity n -> MicroLens.set Conway.ppuDRepActivityL $ SJust $ Cardano.EpochInterval $ fromIntegral n

-- will exist later on: MinFeeRefScriptCostPerByte n -> MicroLens.set Conway.ppuMinFeeRefScriptCostPerByteL $ justFromIntegral n

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
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> scriptToScriptWitness script txSkelRedeemer datum
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

plutusScriptBuilder :: Api.SerialisedScript -> TxSkelRedeemer -> TxGen (Cardano.PlutusScriptOrReferenceInput lang, Cardano.HashableScriptData)
plutusScriptBuilder script TxSkelNoRedeemer =
  return (Cardano.PScript $ Cardano.PlutusScriptSerialised script, Ledger.toCardanoScriptData $ Api.toBuiltinData ())
plutusScriptBuilder script (TxSkelRedeemerForScript redeemer) =
  return (Cardano.PScript $ Cardano.PlutusScriptSerialised script, Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)
plutusScriptBuilder script (TxSkelRedeemerForReferencedScript validatorOref redeemer) = do
  refScriptHash <-
    throwOnLookup "txSkelRedeemerToWitness: Can't resolve reference script outref. " validatorOref =<< asks (Map.mapMaybe (^. outputReferenceScriptL) . managedTxOuts)
  when (refScriptHash /= toScriptHash script) $
    throwOnString "txSkelRedeemerToWitness: Wrong reference script hash. Are you using the correct TxOutRef on your TxSkelRedeemerForReferencedScript?"
  validatorTxIn <-
    throwOnToCardanoError "txSkelRedeemerToWitness: translating TxOutRef where the reference script sits" $ Ledger.toCardanoTxIn validatorOref
  scriptHash <-
    throwOnToCardanoError "txSkelRedeemerToWitness: could not convert script hash of referenced script" $ Ledger.toCardanoScriptHash refScriptHash
  return (Cardano.PReferenceScript validatorTxIn (Just scriptHash), Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)

scriptToScriptWitness :: (ToScript a) => a -> TxSkelRedeemer -> Cardano.ScriptDatum b -> TxGen (Cardano.ScriptWitness b Cardano.ConwayEra)
scriptToScriptWitness (toScript -> (Script.Versioned (Script.Script script) version)) redeemer datum =
  case version of
    Script.PlutusV1 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 x datum y Ledger.zeroExecutionUnits)
        <$> plutusScriptBuilder script redeemer
    Script.PlutusV2 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 x datum y Ledger.zeroExecutionUnits)
        <$> plutusScriptBuilder script redeemer
    Script.PlutusV3 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 x datum y Ledger.zeroExecutionUnits)
        <$> plutusScriptBuilder script redeemer

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
              mintWitness <- scriptToScriptWitness policy redeemer Cardano.NoScriptDatumForMint
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
