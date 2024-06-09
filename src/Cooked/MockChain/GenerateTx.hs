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
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
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
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Address qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
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
      returnCollateralWallet :: Wallet,
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
  def = Context 0 mempty (wallet 1) def Map.empty Map.empty Map.empty

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
      then throwOnString "empty txSkelSigners. You must provide at least one signer"
      else
        throwOnToCardanoErrorOrApply
          "translating the required signers"
          (Cardano.TxExtraKeyWitnesses Cardano.AlonzoEraOnwardsConway)
          $ mapM (Ledger.toCardanoPaymentKeyHash . Ledger.PaymentPubKeyHash . walletPKHash) txSkelSigners
  txProtocolParams <- asks (Cardano.BuildTxWith . Just . Emulator.ledgerProtocolParameters . params)
  txFee <- asks (Cardano.TxFeeExplicit Cardano.ShelleyBasedEraConway . Emulator.Coin . fee)
  let txMetadata = Cardano.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = Cardano.TxAuxScriptsNone -- That's what plutus-apps does as well
      txWithdrawals = Cardano.TxWithdrawalsNone -- That's what plutus-apps does as well
      txCertificates = Cardano.TxCertificatesNone -- That's what plutus-apps does as well
      txUpdateProposal = Cardano.TxUpdateProposalNone -- That's what plutus-apps does as well
      txScriptValidity = Cardano.TxScriptValidityNone -- That's what plutus-apps does as well
      txProposalProcedures = Nothing -- TODO, should appear in our skeleton?
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
generateBodyContent fee returnCollateralWallet collateralIns params managedData managedTxOuts managedValidators =
  flip runReaderT Context {..} . txSkelToBodyContent

-- Convert a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelIn', into a 'Cardano.TxIn', together with the appropriate witness. If
-- you add reference inputs, don't forget to also update the 'txInsReference'!
txSkelInToTxIn ::
  (Api.TxOutRef, TxSkelRedeemer) ->
  TxGen (Cardano.TxIn, Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra))
txSkelInToTxIn (txOutRef, txSkelRedeemer) = do
  witness <- txSkelRedeemerToWitness txOutRef txSkelRedeemer
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

txSkelRedeemerToWitness :: Api.TxOutRef -> TxSkelRedeemer -> TxGen (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra)
txSkelRedeemerToWitness _ TxSkelNoRedeemerForPK = return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
txSkelRedeemerToWitness txOutRef (TxSkelRedeemerForReferencedScript validatorOref redeemer) = do
  (Script.ValidatorHash validatorHash, Script.Versioned _ version, datum) <- resolveScriptOutputOwnerAndDatum txOutRef
  Api.ScriptHash scriptHashAtOref <-
    -- In our own MockChainT implementation, this error should never been
    -- thrown, because we collect the 'managedTxOuts' using (eventually)
    -- 'lookupUtxos', which will already fail on un-resolvable 'TxOutRef's.
    throwOnLookup
      "txSkelRedeemerToWitness: Can't resolve reference script outref. This might mean that you either never created or accidentally consumed the UTxO where the reference script is stored"
      validatorOref
      =<< asks (Map.mapMaybe (^. outputReferenceScriptL) . managedTxOuts)
  when (scriptHashAtOref /= validatorHash) $
    throwOnString "txSkelRedeemerToWitness: Wrong reference script hash. Are you using the correct TxOutRef on your TxSkelRedeemerForReferencedScript?"
  validatorTxIn <-
    throwOnToCardanoError "txSkelRedeemerToWitness: translating TxOutRef where the reference script sits" $ Ledger.toCardanoTxIn validatorOref
  scriptHash <-
    throwOnToCardanoError "txSkelRedeemerToWitness: could not convert script hash of referenced script" $ Ledger.toCardanoScriptHash $ Api.ScriptHash validatorHash
  let scriptWitnessBuilder = case version of
        Script.PlutusV1 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 (Cardano.PReferenceScript validatorTxIn (Just scriptHash))
        Script.PlutusV2 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 (Cardano.PReferenceScript validatorTxIn (Just scriptHash))
        Script.PlutusV3 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 (Cardano.PReferenceScript validatorTxIn (Just scriptHash))
  return $
    Cardano.ScriptWitness Cardano.ScriptWitnessForSpending $
      scriptWitnessBuilder
        datum
        (Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)
        Ledger.zeroExecutionUnits -- We can't guess that yet, no?
txSkelRedeemerToWitness txOutRef (TxSkelRedeemerForScript redeemer) = do
  (_validatorHash, Script.Versioned (Script.Validator (Script.Script script)) version, datum) <- resolveScriptOutputOwnerAndDatum txOutRef
  let scriptWitnessBuilder = case version of
        Script.PlutusV1 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
        Script.PlutusV2 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
        Script.PlutusV3 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
  return $
    Cardano.ScriptWitness Cardano.ScriptWitnessForSpending $
      scriptWitnessBuilder
        datum
        (Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)
        Ledger.zeroExecutionUnits -- We can't guess that yet, no?

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

-- Computes the collateral triplet from the fee and the collateral inputs in
-- the context
toCollateralTriplet ::
  TxGen
    ( Cardano.TxInsCollateral Cardano.ConwayEra,
      Cardano.TxTotalCollateral Cardano.ConwayEra,
      Cardano.TxReturnCollateral Cardano.CtxTx Cardano.ConwayEra
    )
toCollateralTriplet = do
  knownTxOuts <- asks managedTxOuts
  returnCollateralWallet <- asks returnCollateralWallet
  networkId <- asks (Emulator.pNetworkId . params)
  collateralInsList <- asks (Set.toList . collateralIns)
  collateralInsValue <-
    let collateralInsResolved = mapMaybe (`Map.lookup` knownTxOuts) collateralInsList
     in if length collateralInsResolved /= length collateralInsList
          then throwOnString "toCollateralTriplet: unresolved txOutRefs"
          else return $ mconcat (Api.txOutValue <$> collateralInsResolved)
  collateralPercentage <- asks (toInteger . fromMaybe 100 . Cardano.protocolParamCollateralPercent . Emulator.pProtocolParams . params)
  coinTotalCollateral <- asks (Emulator.Coin . (+ 1) . (`div` 100) . (* collateralPercentage) . fee)
  txReturnCollateralValue <-
    Ledger.toCardanoTxOutValue
      <$> throwOnToCardanoError
        "toCollateralTriplet: cannot build return collateral value"
        (Ledger.toCardanoValue (collateralInsValue <> PlutusTx.negate (toValue coinTotalCollateral)))
  address <-
    throwOnToCardanoError "toCollateralTriplet: cannot build return collateral address" $
      Ledger.toCardanoAddressInEra networkId (walletAddress returnCollateralWallet)
  let txTotalCollateral = Cardano.TxTotalCollateral Cardano.BabbageEraOnwardsConway coinTotalCollateral
      txReturnCollateral =
        Cardano.TxReturnCollateral Cardano.BabbageEraOnwardsConway $
          Cardano.TxOut address txReturnCollateralValue Cardano.TxOutDatumNone Cardano.ReferenceScriptNone
  txInsCollateral <-
    throwOnToCardanoError
      "txOutRefsToTxInCollateral"
      (toTxInsCollateral <$> mapM Ledger.toCardanoTxIn collateralInsList)
  return (txInsCollateral, txTotalCollateral, txReturnCollateral)
  where
    toTxInsCollateral [] = Cardano.TxInsCollateralNone
    toTxInsCollateral ins = Cardano.TxInsCollateral Cardano.AlonzoEraOnwardsConway ins

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
              mintWitness <- mintingPolicyToMintWitness policy redeemer
              return $ Map.insert policyId mintWitness acc
          )
          Map.empty
          (txSkelMintsToList mints)
      return $ Cardano.TxMintValue Cardano.MaryEraOnwardsConway mintVal (Cardano.BuildTxWith witnessMap)

mintingPolicyToMintWitness :: Script.Versioned Script.MintingPolicy -> MintsRedeemer -> TxGen (Cardano.ScriptWitness Cardano.WitCtxMint Cardano.ConwayEra)
mintingPolicyToMintWitness (Script.Versioned (Script.MintingPolicy (Script.Script script)) version) redeemer = do
  let scriptWitnessBuilder = case version of
        Script.PlutusV1 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
        Script.PlutusV2 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
        Script.PlutusV3 -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
  return $
    scriptWitnessBuilder
      Cardano.NoScriptDatumForMint -- This seems to be the only well-typed option (?)
      ( case redeemer of
          NoMintsRedeemer -> Ledger.toCardanoScriptData $ Api.toBuiltinData ()
          SomeMintsRedeemer red -> Ledger.toCardanoScriptData $ Api.toBuiltinData red
      )
      Ledger.zeroExecutionUnits -- This is what plutus-apps does as well, we can't know this yet, no?

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
generateTx fee returnCollateralWallet collateralIns params managedData managedTxOuts managedValidators =
  flip runReaderT Context {..} . txSkelToCardanoTx
