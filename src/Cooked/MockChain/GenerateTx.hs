{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Cooked.MockChain.GenerateTx
  ( GenerateTxError (..),
    GenTxParams (gtpCollateralIns, gtpFee),
    generateBodyContent,
    generateTxOut,
    generateTx,
  )
where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Node.Emulator.Internal.Node.Params as Emulator
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Cooked.Output
import Cooked.Skeleton
import Cooked.Wallet
import Data.Bifunctor
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (TxOut, txOutValue, validatorHash)
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Pl
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl (fromValue, getLovelace)
import qualified PlutusLedgerApi.V3 as Pl hiding (getLovelace)

-- * Domain for transaction generation and associated types

data GenerateTxError
  = ToCardanoError String Pl.ToCardanoError
  | TxBodyError String C.TxBodyError
  | GenerateTxErrorGeneral String
  deriving (Show, Eq)

-- | The internal (do-not-modify unless you know what you're doing)
-- parameters for 'txSkelToBodyContent'.
data GenTxParams = GenTxParams
  { -- | The collateral UTxOs to use for the transaction.
    --
    -- It is the duty of the caller to choose and set the collateral
    -- UTxOs. 'txSkelToBodyContent' will not do it.
    gtpCollateralIns :: Set Pl.TxOutRef,
    -- | The transaction fee (in Lovelace)
    gtpFee :: Fee
  }

instance Default GenTxParams where
  def = GenTxParams {gtpCollateralIns = mempty, gtpFee = 0}

-- Context in which various parts of transactions will be built This
-- will be used within a reader monad to simplify passing parameters
-- around
data Context where
  Context ::
    { genTxParams :: GenTxParams,
      params :: Emulator.Params,
      managedData :: Map Pl.DatumHash Pl.Datum,
      managedTxOuts :: Map Pl.TxOutRef Pl.TxOut,
      managedValidators :: Map Pl.ValidatorHash (Pl.Versioned Pl.Validator)
    } ->
    Context

instance Default Context where
  def = Context def def Map.empty Map.empty Map.empty

-- The domain in which transactions are generated.
type TxGen a = ReaderT Context (Either GenerateTxError) a

-- * Helpers to throw errors in 'TxGen'

-- Looks up a key in a map. Throws a 'GenerateTxErrorGeneral' error
-- with a given message when the key is absent, returns the associated
-- value otherwise.
throwOnLookup :: (Ord k) => String -> k -> Map k a -> TxGen a
throwOnLookup errorMsg key = maybe (throwOnString errorMsg) return . Map.lookup key

-- Throws a general error from a String
throwOnString :: String -> TxGen a
throwOnString = lift . Left . GenerateTxErrorGeneral

-- Lifts a 'ToCardanoError' with an associated error message, or apply
-- a function if a value exists
throwOnToCardanoErrorOrApply :: String -> (a -> b) -> Either Pl.ToCardanoError a -> TxGen b
throwOnToCardanoErrorOrApply errorMsg f = lift . bimap (ToCardanoError errorMsg) f

-- Lifts a 'ToCardanoError' with an associated error message, or
-- leaves the value unchanged if it exists
throwOnToCardanoError :: String -> Either Pl.ToCardanoError a -> TxGen a
throwOnToCardanoError = flip throwOnToCardanoErrorOrApply id

-- * Generation functions

txSkelToBodyContent :: TxSkel -> TxGen (C.TxBodyContent C.BuildTx C.ConwayEra)
txSkelToBodyContent TxSkel {..} = do
  collateralInsList <- asks (Set.toList . gtpCollateralIns . genTxParams)
  txIns <- mapM txSkelInToTxIn $ Map.toList txSkelIns
  txInsReference <-
    txOutRefsToTxInsReference $
      Maybe.mapMaybe
        ( \case
            TxSkelRedeemerForReferencedScript oref _ -> Just oref
            _ -> Nothing
        )
        (Map.elems txSkelIns)
        ++ Set.toList txSkelInsReference
  txInsCollateral <- txOutRefsToTxSkelInsCollateral collateralInsList
  txOuts <- mapM txSkelOutToCardanoTxOut txSkelOuts
  (txValidityLowerBound, txValidityUpperBound) <-
    lift
      $ left
        (ToCardanoError "translating the transaction validity range")
        . Pl.toCardanoValidityRange
      $ txSkelValidityRange
  txMintValue <- txSkelMintsToTxMintValue txSkelMints
  txExtraKeyWits <-
    if null txSkelSigners
      then throwOnString "empty txSkelSigners. You must provide at least one signer"
      else
        throwOnToCardanoErrorOrApply
          "translating the required signers"
          (C.TxExtraKeyWitnesses C.AlonzoEraOnwardsConway)
          $ mapM (Pl.toCardanoPaymentKeyHash . Pl.PaymentPubKeyHash . walletPKHash) txSkelSigners
  knownTxOuts <- asks managedTxOuts
  txTotalCollateral <-
    C.TxTotalCollateral C.BabbageEraOnwardsConway . Pl.Coin . Pl.getLovelace . Pl.fromValue . mconcat
      <$> mapM
        ( \txOutRef ->
            Pl.txOutValue
              <$> throwOnLookup ("computing the total collateral: Unknown TxOutRef" ++ show txOutRef) txOutRef knownTxOuts
        )
        collateralInsList
  txProtocolParams <- asks (C.BuildTxWith . Just . Emulator.ledgerProtocolParameters . params)
  txFee <- asks (C.TxFeeExplicit C.ShelleyBasedEraConway . Pl.Coin . feeLovelace . gtpFee . genTxParams)
  let txReturnCollateral = C.TxReturnCollateralNone
      txMetadata = C.TxMetadataNone -- That's what plutus-apps does as well
      txAuxScripts = C.TxAuxScriptsNone -- That's what plutus-apps does as well
      txWithdrawals = C.TxWithdrawalsNone -- That's what plutus-apps does as well
      txCertificates = C.TxCertificatesNone -- That's what plutus-apps does as well
      txUpdateProposal = C.TxUpdateProposalNone -- That's what plutus-apps does as well
      txScriptValidity = C.TxScriptValidityNone -- That's what plutus-apps does as well
      txProposalProcedures = Nothing -- TODO, should appear in our skeleton?
      txVotingProcedures = Nothing -- TODO, same as above
  return C.TxBodyContent {..}

generateBodyContent ::
  GenTxParams ->
  Emulator.Params ->
  Map Pl.DatumHash Pl.Datum ->
  Map Pl.TxOutRef Pl.TxOut ->
  Map Pl.ValidatorHash (Pl.Versioned Pl.Validator) ->
  TxSkel ->
  Either GenerateTxError (C.TxBodyContent C.BuildTx C.ConwayEra)
generateBodyContent genTxParams params managedData managedTxOuts managedValidators =
  flip runReaderT Context {..} . txSkelToBodyContent

-- Convert a 'TxSkel' input, which consists of a 'Pl.TxOutRef' and a
-- 'TxSkelIn', into a 'C.TxIn', together with the appropriate witness. If
-- you add reference inputs, don't forget to also update the
-- 'txInsReference'!
txSkelInToTxIn ::
  (Pl.TxOutRef, TxSkelRedeemer) ->
  TxGen (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.ConwayEra))
txSkelInToTxIn (txOutRef, txSkelRedeemer) = do
  witness <- txSkelRedeemerToWitness txOutRef txSkelRedeemer
  throwOnToCardanoErrorOrApply
    "txSkelIntoTxIn, translating TxOutRef"
    (,C.BuildTxWith witness)
    $ Pl.toCardanoTxIn txOutRef

resolveScriptOutputOwnerAndDatum ::
  Pl.TxOutRef ->
  TxGen (Pl.ValidatorHash, Pl.Versioned Pl.Validator, C.ScriptDatum C.WitCtxTxIn)
resolveScriptOutputOwnerAndDatum txOutRef = do
  txOut <- throwOnLookup "txSkelInToTxIn: Unknown txOutRef" txOutRef =<< asks managedTxOuts
  validatorHash <-
    case outputAddress txOut of
      (Pl.Address (Pl.ScriptCredential (Pl.ScriptHash validatorHash)) _) -> return $ Pl.ValidatorHash validatorHash
      _ -> throwOnString "txSkelInToTxIn: Output is not a script output"
  validator <- throwOnLookup "txSkelInToTxIn: Unknown validator" validatorHash =<< asks managedValidators
  datum <-
    case outputOutputDatum txOut of
      Pl.NoOutputDatum -> throwOnString "txSkelInToTxIn: No datum found on script output"
      Pl.OutputDatum _ -> return C.InlineScriptDatum
      Pl.OutputDatumHash datumHash -> do
        datum <- throwOnLookup "txSkelInToTxIn: Datum hash could not be resolved" datumHash =<< asks managedData
        return $ C.ScriptDatumForTxIn $ Pl.toCardanoScriptData $ Pl.getDatum datum
  return (validatorHash, validator, datum)

txSkelRedeemerToWitness :: Pl.TxOutRef -> TxSkelRedeemer -> TxGen (C.Witness C.WitCtxTxIn C.ConwayEra)
txSkelRedeemerToWitness _ TxSkelNoRedeemerForPK = return $ C.KeyWitness C.KeyWitnessForSpending
txSkelRedeemerToWitness txOutRef (TxSkelRedeemerForReferencedScript validatorOref redeemer) = do
  (Pl.ValidatorHash validatorHash, Pl.Versioned _ version, datum) <- resolveScriptOutputOwnerAndDatum txOutRef
  Pl.ScriptHash scriptHashAtOref <-
    -- In our own MockChainT implementation, this error should never
    -- been thrown, because we collect the 'managedTxOuts' using
    -- (eventually) 'lookupUtxos', which will already fail on
    -- un-resolvable 'TxOutRef's.
    throwOnLookup
      "txSkelInToTxIn: Can't resolve reference script outref. This might mean that you either never created or accidentally consumed the UTxO where the reference script is stored"
      validatorOref
      =<< asks (Map.mapMaybe (^. outputReferenceScriptL) . managedTxOuts)
  when (scriptHashAtOref /= validatorHash) $
    throwOnString "txSkelInToTxIn: Wrong reference script hash. Are you using the correct TxOutRef on your TxSkelRedeemerForReferencedScript?"
  validatorTxIn <-
    throwOnToCardanoError "txSkelIntoTxIn: translating TxOutRef where the reference script sits" $ Pl.toCardanoTxIn validatorOref
  scriptHash <-
    throwOnToCardanoError "txSkelInToTxIn: could not convert script hash of referenced script" $ Pl.toCardanoScriptHash $ Pl.ScriptHash validatorHash
  let scriptWitnessBuilder = case version of
        Pl.PlutusV1 -> C.PlutusScriptWitness C.PlutusScriptV1InConway C.PlutusScriptV1 (C.PReferenceScript validatorTxIn (Just scriptHash))
        Pl.PlutusV2 -> C.PlutusScriptWitness C.PlutusScriptV2InConway C.PlutusScriptV2 (C.PReferenceScript validatorTxIn (Just scriptHash))
        Pl.PlutusV3 -> C.PlutusScriptWitness C.PlutusScriptV3InConway C.PlutusScriptV3 (C.PReferenceScript validatorTxIn (Just scriptHash))
  return $
    C.ScriptWitness C.ScriptWitnessForSpending $
      scriptWitnessBuilder
        datum
        (Pl.toCardanoScriptData $ Pl.toBuiltinData redeemer)
        Pl.zeroExecutionUnits -- We can't guess that yet, no?
txSkelRedeemerToWitness txOutRef (TxSkelRedeemerForScript redeemer) = do
  (_validatorHash, Pl.Versioned (Pl.Validator (Pl.Script script)) version, datum) <- resolveScriptOutputOwnerAndDatum txOutRef
  let scriptWitnessBuilder = case version of
        Pl.PlutusV1 -> C.PlutusScriptWitness C.PlutusScriptV1InConway C.PlutusScriptV1 $ C.PScript $ C.PlutusScriptSerialised script
        Pl.PlutusV2 -> C.PlutusScriptWitness C.PlutusScriptV2InConway C.PlutusScriptV2 $ C.PScript $ C.PlutusScriptSerialised script
        Pl.PlutusV3 -> C.PlutusScriptWitness C.PlutusScriptV3InConway C.PlutusScriptV3 $ C.PScript $ C.PlutusScriptSerialised script
  return $
    C.ScriptWitness C.ScriptWitnessForSpending $
      scriptWitnessBuilder
        datum
        (Pl.toCardanoScriptData $ Pl.toBuiltinData redeemer)
        Pl.zeroExecutionUnits -- We can't guess that yet, no?

-- Convert a list of 'Pl.TxOutRef' into a 'C.TxInsReference'
txOutRefsToTxInsReference :: [Pl.TxOutRef] -> TxGen (C.TxInsReference C.BuildTx C.ConwayEra)
txOutRefsToTxInsReference =
  throwOnToCardanoErrorOrApply
    "txOutRefsToTxInsReference"
    ( \case
        [] -> C.TxInsReferenceNone
        txIns -> C.TxInsReference C.BabbageEraOnwardsConway txIns
    )
    . mapM Pl.toCardanoTxIn

-- Convert a list of 'Pl.TxOutRef' into a 'C.TxInsCollateral'
txOutRefsToTxSkelInsCollateral :: [Pl.TxOutRef] -> TxGen (C.TxInsCollateral C.ConwayEra)
txOutRefsToTxSkelInsCollateral =
  throwOnToCardanoError "txOutRefsToTxInCollateral"
    . fmap toTxInsCollateral
    . mapM Pl.toCardanoTxIn
  where
    toTxInsCollateral [] = C.TxInsCollateralNone
    toTxInsCollateral ins = C.TxInsCollateral C.AlonzoEraOnwardsConway ins

-- Convert the 'TxSkelMints' into a 'TxMintValue'
txSkelMintsToTxMintValue :: TxSkelMints -> TxGen (C.TxMintValue C.BuildTx C.ConwayEra)
txSkelMintsToTxMintValue mints =
  if mints == Map.empty
    then return C.TxMintNone
    else do
      mintVal <-
        throwOnToCardanoError "txSkelMintsToTxMintValue, translating minted value" $ Pl.toCardanoValue $ txSkelMintsValue mints
      witnessMap <-
        foldM
          ( \acc (policy, redeemer, _tName, _amount) -> do
              policyId <-
                throwOnToCardanoError
                  "txSkelMintsToTxMintValue, calculating the witness map"
                  (Pl.toCardanoPolicyId (Pl.mintingPolicyHash policy))
              mintWitness <- mintingPolicyToMintWitness policy redeemer
              return $ Map.insert policyId mintWitness acc
          )
          Map.empty
          (txSkelMintsToList mints)
      return $ C.TxMintValue C.MaryEraOnwardsConway mintVal (C.BuildTxWith witnessMap)

mintingPolicyToMintWitness :: Pl.Versioned Pl.MintingPolicy -> MintsRedeemer -> TxGen (C.ScriptWitness C.WitCtxMint C.ConwayEra)
mintingPolicyToMintWitness (Pl.Versioned (Pl.MintingPolicy (Pl.Script script)) version) redeemer = do
  let scriptWitnessBuilder = case version of
        Pl.PlutusV1 -> C.PlutusScriptWitness C.PlutusScriptV1InConway C.PlutusScriptV1 $ C.PScript $ C.PlutusScriptSerialised script
        Pl.PlutusV2 -> C.PlutusScriptWitness C.PlutusScriptV2InConway C.PlutusScriptV2 $ C.PScript $ C.PlutusScriptSerialised script
        Pl.PlutusV3 -> C.PlutusScriptWitness C.PlutusScriptV3InConway C.PlutusScriptV3 $ C.PScript $ C.PlutusScriptSerialised script
  return $
    scriptWitnessBuilder
      C.NoScriptDatumForMint -- This seems to be the only well-typed option (?)
      ( case redeemer of
          NoMintsRedeemer -> Pl.toCardanoScriptData $ Pl.toBuiltinData ()
          SomeMintsRedeemer red -> Pl.toCardanoScriptData $ Pl.toBuiltinData red
      )
      Pl.zeroExecutionUnits -- This is what plutus-apps does as well, we can't know this yet, no?

-- Convert a 'TxSkelOut' to the corresponding 'C.TxOut'.
txSkelOutToCardanoTxOut :: TxSkelOut -> TxGen (C.TxOut C.CtxTx C.ConwayEra)
txSkelOutToCardanoTxOut (Pays output) = do
  networkId <- asks $ Emulator.pNetworkId . params
  address <- throwOnToCardanoError "txSkelOutToCardanoTxOut: wrong address" $ Pl.toCardanoAddressInEra networkId (outputAddress output)
  value <- Pl.toCardanoTxOutValue <$> throwOnToCardanoError "txSkelOutToCardanoTxOut: unresolved value" (Pl.toCardanoValue $ outputValue output)
  datum <- case output ^. outputDatumL of
    TxSkelOutNoDatum -> return C.TxOutDatumNone
    TxSkelOutDatumHash datum ->
      throwOnToCardanoError "txSkelOutToTxOut: unresolved datum hash" $
        C.TxOutDatumHash C.AlonzoEraOnwardsConway
          <$> Pl.toCardanoScriptDataHash (Pl.datumHash $ Pl.Datum $ Pl.toBuiltinData datum)
    TxSkelOutDatum datum ->
      return
        $ C.TxOutDatumInTx C.AlonzoEraOnwardsConway
          . C.unsafeHashableScriptData
          . C.fromPlutusData
          . Pl.builtinDataToData
          . Pl.toBuiltinData
        $ datum
    TxSkelOutInlineDatum datum ->
      return
        $ C.TxOutDatumInline C.BabbageEraOnwardsConway
          . C.unsafeHashableScriptData
          . C.fromPlutusData
          . Pl.builtinDataToData
          . Pl.toBuiltinData
        $ datum
  let refScript = Pl.toCardanoReferenceScript (toScript <$> output ^. outputReferenceScriptL)
  return $ C.TxOut address value datum refScript

generateTxOut :: C.NetworkId -> TxSkelOut -> Either GenerateTxError (C.TxOut C.CtxTx C.ConwayEra)
generateTxOut networkId =
  flip runReaderT (def {params = def {Emulator.pNetworkId = networkId}}) . txSkelOutToCardanoTxOut

txSkelToCardanoTx :: TxSkel -> TxGen (C.Tx C.ConwayEra)
txSkelToCardanoTx txSkel = do
  txBodyContent <- txSkelToBodyContent txSkel
  cardanoTxUnsigned <-
    lift $
      bimap
        (TxBodyError "generateTx: ")
        (flip C.Tx [])
        (C.createAndValidateTransactionBody C.ShelleyBasedEraConway txBodyContent)
  cardanoTxSigned <-
    foldM
      ( \tx wal ->
          case Ledger.addCardanoTxWitness (Pl.toWitness $ Pl.PaymentPrivateKey $ walletSK wal) (Ledger.CardanoTx tx C.ShelleyBasedEraConway) of
            Ledger.CardanoTx tx' C.ShelleyBasedEraConway -> return tx'
            _ -> throwOnString "txSkelToCardanoTx: Wrong output era"
      )
      cardanoTxUnsigned
      (txSkelSigners txSkel)
  return $ applyRawModOnBalancedTx (txOptUnsafeModTx . txSkelOpts $ txSkel) cardanoTxSigned

generateTx ::
  -- | Parameters controlling transaction generation.
  GenTxParams ->
  -- | Some parameters, coming from the 'MockChain'.
  Emulator.Params ->
  -- | All of the currently known data on transaction inputs, also coming from the 'MockChain'.
  Map Pl.DatumHash Pl.Datum ->
  -- | All of the currently known UTxOs which will be used as transaction inputs or referenced, also coming from the 'MockChain'.
  Map Pl.TxOutRef Pl.TxOut ->
  -- | All of the currently known validators which protect transaction inputs, also coming from the 'MockChain'.
  Map Pl.ValidatorHash (Pl.Versioned Pl.Validator) ->
  -- | The transaction skeleton to translate.
  TxSkel ->
  Either GenerateTxError (C.Tx C.ConwayEra)
generateTx genTxParams params managedData managedTxOuts managedValidators =
  flip runReaderT Context {..} . txSkelToCardanoTx
