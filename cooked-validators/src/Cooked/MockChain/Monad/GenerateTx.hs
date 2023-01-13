{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Cooked.MockChain.Monad.GenerateTx where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import Control.Arrow
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Bifunctor
import Data.Default
import qualified Data.List.NonEmpty as NEList
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (TxOut, validatorHash)
import qualified Ledger.Ada as Pl
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Tx.CardanoAPI as Pl
import qualified Plutus.V2.Ledger.Api as Pl

data GenerateTxError
  = ToCardanoError String Pl.ToCardanoError
  | GenerateTxErrorGeneral String
  deriving (Show, Eq)

-- | The internal (do-not-modify unless you know what you're doing) parameters
-- for 'generateTxBodyContent'.
data GenTxParams = GenTxParams
  { -- | The collateral UTxOs to use for the transaction.
    --
    -- It is the duty of the caller to choose and set the collateral UTxOs.
    -- 'generateTxBodyContent' will not do it.
    gtpCollateralIns :: Set Pl.TxOutRef
  }

instance Default GenTxParams where
  def = GenTxParams {gtpCollateralIns = mempty}

generateTxBodyContent ::
  -- | The parameters controlling the transaction generation.
  GenTxParams ->
  -- | Some parameters, coming from the 'MockChain'.
  Pl.Params ->
  -- | All of the currently known data, also coming from the 'MockChain'.
  Map Pl.DatumHash (Pl.Datum, String) ->
  -- | All of the currently known transactions outputs, also coming from the 'MockChain'.
  Map Pl.TxOutRef Pl.TxOut ->
  -- | All of the currently known transactions outputs, also coming from the 'MockChain'.
  Map Pl.ValidatorHash (Pl.Versioned Pl.Validator) ->
  -- | The transaction skeleton to translate.
  TxSkel ->
  Either
    GenerateTxError
    (C.TxBodyContent C.BuildTx C.BabbageEra)
generateTxBodyContent GenTxParams {..} theParams managedData managedTxOuts managedValidators skel = do
  txIns <- mapM txSkelInToTxIn $ Map.toList (txSkelIns skel)
  txInsCollateral <- spOutsToTxInsCollateral $ Set.toList gtpCollateralIns
  txOuts <- mapM txSkelOutToCardanoTxOut $ txSkelOuts skel
  txValidityRange <-
    left
      (ToCardanoError "translating the transaction validity range")
      . Pl.toCardanoValidityRange
      . Pl.posixTimeRangeToContainedSlotRange (Pl.pSlotConfig theParams)
      $ txSkelValidityRange skel
  txMintValue <- txSkelMintsToTxMintValue $ txSkelMints skel
  txExtraKeyWits <-
    bimap
      (ToCardanoError "translating the required signers")
      (C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra)
      $ mapM
        (Pl.toCardanoPaymentKeyHash . Pl.PaymentPubKeyHash . walletPKHash)
        (NEList.toList $ txSkelSigners skel)
  txTotalCollateral <-
    right
      ( C.TxTotalCollateral (Maybe.fromJust (C.totalAndReturnCollateralSupportedInEra C.BabbageEra))
          . C.Lovelace
          . Pl.getLovelace
          . Pl.fromValue
          . mconcat
      )
      ( mapM
          ( \txOutRef -> do
              Pl.TxOut _ outValue _ _ <-
                throwOnNothing
                  (GenerateTxErrorGeneral $ "computing the total collateral: Unknown TxOutRef" ++ show txOutRef)
                  (Map.lookup txOutRef managedTxOuts)
              Right outValue
          )
          $ Set.toList gtpCollateralIns
      )
  Right $
    C.TxBodyContent
      { C.txIns = txIns,
        C.txInsCollateral = txInsCollateral,
        -- We don't yet support reference inputs. If you add this
        -- functionality, remember that both the 'txIns' and 'txInsReference'
        -- fields have to change!
        C.txInsReference = C.TxInsReferenceNone,
        C.txOuts = txOuts,
        C.txTotalCollateral = txTotalCollateral,
        -- WARN For now we are not dealing with return collateral
        C.txReturnCollateral = C.TxReturnCollateralNone,
        C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra . C.Lovelace $ txSkelFee skel,
        C.txValidityRange = txValidityRange,
        C.txMetadata = C.TxMetadataNone, -- That's what plutus-apps does as well
        C.txAuxScripts = C.TxAuxScriptsNone, -- That's what plutus-apps does as well
        C.txExtraKeyWits = txExtraKeyWits,
        C.txProtocolParams = C.BuildTxWith . Just . Pl.pProtocolParams $ theParams, -- That's what plutus-apps does as well
        C.txWithdrawals = C.TxWithdrawalsNone, -- That's what plutus-apps does as well
        C.txCertificates = C.TxCertificatesNone, -- That's what plutus-apps does as well
        C.txUpdateProposal = C.TxUpdateProposalNone, -- That's what plutus-apps does as well
        C.txMintValue = txMintValue,
        C.txScriptValidity = C.TxScriptValidityNone -- That's what plutus-apps does as well
      }
  where
    -- Helper function to throw errors.
    throwOnNothing :: e -> Maybe a -> Either e a
    throwOnNothing err = maybe (Left err) Right

    -- Convert a 'TxSkel' input, which consists of a 'SpendableOut' and a
    -- 'TxSkelIn', into a 'C.TxIn', together with the appropriate witness. If
    -- you add reference inputs, don't forget to also update the
    -- 'txInsReference'!
    txSkelInToTxIn ::
      (Pl.TxOutRef, TxSkelRedeemer) ->
      Either
        GenerateTxError
        ( C.TxIn,
          C.BuildTxWith
            C.BuildTx
            (C.Witness C.WitCtxTxIn C.BabbageEra)
        )
    txSkelInToTxIn (txOutRef, TxSkelNoRedeemerForPK) =
      bimap
        (ToCardanoError "txSkelIntoTxIn, translating 'SpendsPK' outRef")
        (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)
        $ Pl.toCardanoTxIn txOutRef
    txSkelInToTxIn (txOutRef, redeemer) = do
      witness <- mkWitness
      bimap
        (ToCardanoError "txSkelIntoTxIn, translating 'SpendsScript' outRef")
        (,C.BuildTxWith witness)
        $ Pl.toCardanoTxIn txOutRef
      where
        mkWitness :: Either GenerateTxError (C.Witness C.WitCtxTxIn C.BabbageEra)
        mkWitness = do
          txOut <-
            throwOnNothing
              (GenerateTxErrorGeneral "txSkelIntoTxIn: Unknown txOutRef")
              (Map.lookup txOutRef managedTxOuts)
          validatorHash <-
            case txOut of
              Pl.TxOut (Pl.Address (Pl.ScriptCredential validatorHash) _) _ _ _ -> Right validatorHash
              _ -> Left (GenerateTxErrorGeneral "txSkelIntoTxIn: tx output is not a script output")
          validator <-
            throwOnNothing
              (GenerateTxErrorGeneral "txSkelIntoTxIn: Unknown validator")
              (Map.lookup validatorHash managedValidators)
          scriptWitnessBuilder <-
            case validator of
              Pl.Versioned (Pl.Validator script) Pl.PlutusV1 ->
                bimap
                  (ToCardanoError "txSkelIntoTxIn, translating to Cardano API PlutusV1 script")
                  (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
              Pl.Versioned (Pl.Validator script) Pl.PlutusV2 ->
                bimap
                  (ToCardanoError "txSkelIntoTxIn, translating to Cardano API PlutusV2 script")
                  (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
          let (Pl.TxOut _ _ outputDatum _) = txOut
          datum <-
            case outputDatum of
              Pl.NoOutputDatum -> Left (GenerateTxErrorGeneral "txSkelIntoTxIn: No datum found on script output")
              Pl.OutputDatum _datum -> Right C.InlineScriptDatum
              Pl.OutputDatumHash datumHash ->
                throwOnNothing
                  (GenerateTxErrorGeneral "txSkelIntoTxIn: Datum hash could not be resolved")
                  (C.ScriptDatumForTxIn . Pl.toCardanoScriptData . Pl.getDatum . fst <$> managedData Map.!? datumHash)
          return $
            C.ScriptWitness C.ScriptWitnessForSpending $
              scriptWitnessBuilder
                datum
                ( Pl.toCardanoScriptData $ case redeemer of
                    TxSkelNoRedeemerForScript -> Pl.toBuiltinData Pl.unitRedeemer
                    TxSkelRedeemerForScript red -> Pl.toBuiltinData red
                    TxSkelNoRedeemerForPK -> error "'TxSkelNoRedeemerForPK' on script input. This cannot happen, as we excluded it with an earlier pattern match"
                )
                Pl.zeroExecutionUnits -- We can't guess that yet, no?

    -- Convert a list of 'SpendableOut' into a 'C.TxInsCollateral'
    spOutsToTxInsCollateral :: [Pl.TxOutRef] -> Either GenerateTxError (C.TxInsCollateral C.BabbageEra)
    spOutsToTxInsCollateral =
      left (ToCardanoError "spOutsToTxInCollateral")
        . Pl.toCardanoTxInsCollateral
        . (toPKTxInput <$>)
      where
        toPKTxInput :: Pl.TxOutRef -> Pl.TxInput
        toPKTxInput txOutRef = Pl.TxInput txOutRef Pl.TxConsumePublicKeyAddress

    txSkelOutToCardanoTxOut :: TxSkelOut -> Either GenerateTxError (C.TxOut C.CtxTx C.BabbageEra)
    txSkelOutToCardanoTxOut txSkelOut =
      left
        (ToCardanoError "txSkelOutToTxOut, translating 'Pays'")
        ( Pl.toCardanoTxOut
            (Pl.pNetworkId theParams)
            ( \case
                Pl.NoOutputDatum -> Right Pl.toCardanoTxOutNoDatum
                Pl.OutputDatumHash hash ->
                  case Map.lookup hash (txSkelOutputData skel) of
                    Nothing -> Pl.toCardanoTxOutDatumHash hash
                    Just (datum, _) -> Right $ Pl.toCardanoTxOutDatumInTx datum
                Pl.OutputDatum datum -> Right $ Pl.toCardanoTxOutDatumInline datum
            )
            $ txSkelOutToTxOut txSkelOut
        )

    -- Convert the 'TxSkelMints' into a 'TxMintValue'
    txSkelMintsToTxMintValue :: TxSkelMints -> Either GenerateTxError (C.TxMintValue C.BuildTx C.BabbageEra)
    txSkelMintsToTxMintValue mints =
      if mints == Map.empty
        then Right C.TxMintNone
        else C.TxMintValue C.MultiAssetInBabbageEra <$> mintVal <*> (C.BuildTxWith <$> witnessMap)
      where
        mintVal :: Either GenerateTxError C.Value
        mintVal =
          left (ToCardanoError "txSkelMintsToTxMintValue, translating minted value")
            . Pl.toCardanoValue
            . txSkelMintsValue
            $ mints

        witnessMap :: Either GenerateTxError (Map C.PolicyId (C.ScriptWitness C.WitCtxMint C.BabbageEra))
        witnessMap =
          right mconcat $
            mapM
              ( \(policy, redeemer, _tName, _amount) ->
                  Map.singleton
                    <$> left
                      (ToCardanoError "txSkelMintsToTxMintValue, calculating the witness map")
                      (Pl.toCardanoPolicyId (Pl.mintingPolicyHash policy))
                      <*> mkMintWitness policy redeemer
              )
              $ txSkelMintsToList mints

        mkMintWitness ::
          Pl.Versioned Pl.MintingPolicy ->
          MintsRedeemer ->
          Either
            GenerateTxError
            (C.ScriptWitness C.WitCtxMint C.BabbageEra)
        mkMintWitness (Pl.Versioned (Pl.MintingPolicy script) version) redeemer = do
          scriptWitnessBuilder <-
            case version of
              Pl.PlutusV1 ->
                bimap
                  (ToCardanoError "txSkelMintsToTxMintValue, translating to Cardano API PlutusV1 script")
                  (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
              Pl.PlutusV2 ->
                bimap
                  (ToCardanoError "txSkelMintsToTxMintValue, translating to Cardano API PlutusV2 script")
                  (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
          return $
            scriptWitnessBuilder
              C.NoScriptDatumForMint -- This seems to be the only well-typed option (?)
              ( case redeemer of
                  NoMintsRedeemer -> Pl.toCardanoScriptData $ Pl.toBuiltinData () -- This is also how plutus-apps is doing it: Using no redeemer means using '()' on-chain
                  SomeMintsRedeemer red -> Pl.toCardanoScriptData $ Pl.toBuiltinData red
              )
              Pl.zeroExecutionUnits -- This is what plutus-apps does as well, we can't know this yet, no?
