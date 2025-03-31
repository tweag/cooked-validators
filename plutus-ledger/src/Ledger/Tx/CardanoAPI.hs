{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Interface to the transaction types from 'cardano-api'
module Ledger.Tx.CardanoAPI
  ( module Ledger.Tx.CardanoAPI.Internal,
    CardanoBuildTx (..),
    CardanoTx (..),
    fromCardanoTxInsCollateral,
    fromCardanoTotalCollateral,
    fromCardanoReturnCollateral,
    toCardanoTotalCollateral,
    toCardanoReturnCollateral,
    toCardanoDatumWitness,
    toCardanoTxInReferenceWitnessHeader,
    toCardanoTxInScriptWitnessHeader,
    toCardanoMintWitness,
    ToCardanoError (..),
    FromCardanoError (..),
    getRequiredSigners,

    -- * Conversion from Plutus types
    toPlutusIndex,
    fromPlutusIndex,
    fromPlutusTxOut,
    fromPlutusTxOutRef,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.BaseTypes (mkTxIxPartial)
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (ConwayTxBody, ctbReqSignerHashes))
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Data.Bifunctor (bimap)
import Data.Map qualified as Map
import Ledger.Address qualified as P
import Ledger.Index.Internal qualified as P
import Ledger.Scripts qualified as P
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal qualified as P
import Plutus.Script.Utils.Scripts qualified as PV1
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3 qualified as PV3

toCardanoMintWitness ::
  PV1.Redeemer ->
  Maybe (P.Versioned PV3.TxOutRef) ->
  Maybe (P.Versioned PV1.MintingPolicy) ->
  Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.ConwayEra)
toCardanoMintWitness _ Nothing Nothing = Left MissingMintingPolicy
toCardanoMintWitness redeemer (Just ref) _ =
  toCardanoScriptWitness C.NoScriptDatumForMint redeemer (Right ref)
toCardanoMintWitness redeemer _ (Just script) =
  toCardanoScriptWitness C.NoScriptDatumForMint redeemer (Left (fmap P.getMintingPolicy script))

toCardanoScriptWitness ::
  (PV1.ToData a) =>
  C.ScriptDatum witctx ->
  a ->
  Either (P.Versioned PV1.Script) (P.Versioned PV3.TxOutRef) ->
  Either ToCardanoError (C.ScriptWitness witctx C.ConwayEra)
toCardanoScriptWitness datum redeemer scriptOrRef =
  ( case scriptOrRef of
      Left script -> pure $ toCardanoTxInScriptWitnessHeader script
      Right ref -> toCardanoTxInReferenceWitnessHeader ref
  )
    <*> pure datum
    <*> pure (C.unsafeHashableScriptData $ C.fromPlutusData $ PV1.toData redeemer)
    <*> pure zeroExecutionUnits

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> [C.TxIn]
fromCardanoTxInsCollateral C.TxInsCollateralNone = []
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = txIns

toCardanoDatumWitness :: Maybe PV1.Datum -> C.ScriptDatum C.WitCtxTxIn
toCardanoDatumWitness = maybe C.InlineScriptDatum (C.ScriptDatumForTxIn . Just . toCardanoScriptData . PV1.getDatum)

type WitnessHeader witctx =
  C.ScriptDatum witctx -> C.ScriptRedeemer -> C.ExecutionUnits -> C.ScriptWitness witctx C.ConwayEra

toCardanoTxInReferenceWitnessHeader ::
  P.Versioned PV3.TxOutRef -> Either ToCardanoError (WitnessHeader witctx)
toCardanoTxInReferenceWitnessHeader (P.Versioned ref lang) = do
  txIn <- toCardanoTxIn ref
  pure $ case lang of
    P.PlutusV1 ->
      C.PlutusScriptWitness C.PlutusScriptV1InConway C.PlutusScriptV1 $ C.PReferenceScript txIn
    P.PlutusV2 ->
      C.PlutusScriptWitness C.PlutusScriptV2InConway C.PlutusScriptV2 $ C.PReferenceScript txIn
    P.PlutusV3 ->
      C.PlutusScriptWitness C.PlutusScriptV3InConway C.PlutusScriptV3 $ C.PReferenceScript txIn

toCardanoTxInScriptWitnessHeader :: P.Versioned PV1.Script -> WitnessHeader witctx
toCardanoTxInScriptWitnessHeader script =
  case toCardanoScriptInEra script of
    C.ScriptInEra _ (C.SimpleScript _) -> error "toCardanoTxInScriptWitnessHeader: impossible simple script"
    C.ScriptInEra era (C.PlutusScript v s) ->
      C.PlutusScriptWitness era v (C.PScript s)

fromCardanoTotalCollateral :: C.TxTotalCollateral C.ConwayEra -> Maybe C.Ledger.Coin
fromCardanoTotalCollateral C.TxTotalCollateralNone = Nothing
fromCardanoTotalCollateral (C.TxTotalCollateral _ lv) = Just lv

toCardanoTotalCollateral :: Maybe C.Ledger.Coin -> C.TxTotalCollateral C.ConwayEra
toCardanoTotalCollateral =
  maybe
    C.TxTotalCollateralNone
    (C.TxTotalCollateral C.BabbageEraOnwardsConway)

fromCardanoReturnCollateral :: C.TxReturnCollateral C.CtxTx C.ConwayEra -> Maybe P.TxOut
fromCardanoReturnCollateral C.TxReturnCollateralNone = Nothing
fromCardanoReturnCollateral (C.TxReturnCollateral _ txOut) = Just $ P.TxOut txOut

toCardanoReturnCollateral :: Maybe P.TxOut -> C.TxReturnCollateral C.CtxTx C.ConwayEra
toCardanoReturnCollateral =
  maybe
    C.TxReturnCollateralNone
    (C.TxReturnCollateral C.BabbageEraOnwardsConway . P.getTxOut)

getRequiredSigners :: C.Tx C.ConwayEra -> [P.PaymentPubKeyHash]
getRequiredSigners (C.ShelleyTx _ (AlonzoTx ConwayTxBody {ctbReqSignerHashes = rsq} _ _ _)) =
  foldMap
    (pure . P.PaymentPubKeyHash . P.toPlutusPubKeyHash . C.PaymentKeyHash . C.Ledger.coerceKeyRole)
    rsq

toPlutusIndex ::
  C.Ledger.UTxO EmulatorEra ->
  P.UtxoIndex
toPlutusIndex (C.Ledger.UTxO utxo) =
  C.UTxO
    . Map.fromList
    . map (bimap C.fromShelleyTxIn (C.fromShelleyTxOut C.ShelleyBasedEraConway))
    . Map.toList
    $ utxo

fromPlutusIndex :: P.UtxoIndex -> C.Ledger.UTxO Conway.ConwayEra
fromPlutusIndex = C.toLedgerUTxO C.ShelleyBasedEraConway

fromPlutusTxOutRef :: PV3.TxOutRef -> Either ToCardanoError C.Ledger.TxIn
fromPlutusTxOutRef (PV3.TxOutRef txId i) = C.Ledger.TxIn <$> fromPlutusTxId txId <*> pure (mkTxIxPartial i)

fromPlutusTxId :: PV3.TxId -> Either ToCardanoError C.Ledger.TxId
fromPlutusTxId = fmap C.toShelleyTxId . toCardanoTxId

fromPlutusTxOut :: P.TxOut -> Ledger.TxOut Conway.ConwayEra
fromPlutusTxOut = C.toShelleyTxOut C.ShelleyBasedEraConway . P.toCtxUTxOTxOut
