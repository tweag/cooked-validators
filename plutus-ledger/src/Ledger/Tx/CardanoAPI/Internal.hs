{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Interface to the transaction types from 'cardano-api'
module Ledger.Tx.CardanoAPI.Internal
  ( CardanoBuildTx (..),
    CardanoTx (..),
    getEmulatorEraTx,
    pattern CardanoEmulatorEraTx,
    txOutRefs,
    unspentOutputsTx,
    fromCardanoTxId,
    fromCardanoTxIn,
    fromCardanoTxOutToPV1TxInfoTxOut,
    fromCardanoTxOutToPV1TxInfoTxOut',
    fromCardanoTxOutToPV2TxInfoTxOut,
    fromCardanoTxOutToPV2TxInfoTxOut',
    fromCardanoTxOutDatumHash,
    fromCardanoTxOutDatumHash',
    fromCardanoTxOutDatum,
    fromCardanoTxOutValue,
    fromCardanoAddressInEra,
    fromCardanoAddress,
    fromCardanoAssetId,
    fromCardanoAssetName,
    fromCardanoMintValue,
    fromCardanoValue,
    fromCardanoPolicyId,
    fromCardanoFee,
    fromCardanoValidityRange,
    fromCardanoScriptInEra,
    fromCardanoPaymentKeyHash,
    fromCardanoScriptData,
    fromCardanoPlutusScript,
    fromCardanoScriptInAnyLang,
    fromCardanoReferenceScript,
    fromCardanoLovelace,
    fromCardanoSlotNo,
    fromTxScriptValidity,
    toTxScriptValidity,
    scriptDataFromCardanoTxBody,
    plutusScriptsFromTxBody,
    createTransactionBody,
    toCardanoTxIn,
    toCardanoTxOut,
    toCardanoTxOutDatum,
    toCardanoTxOutDatumHash,
    toCardanoTxOutDatumHashFromDatum,
    toCardanoTxOutDatumInline,
    toCardanoTxOutNoDatum,
    toCardanoTxOutValue,
    toCardanoAddressInEra,
    toCardanoAssetId,
    toCardanoAssetName,
    toCardanoPolicyId,
    toCardanoValue,
    toCardanoLovelace,
    toCardanoFee,
    adaToCardanoValue,
    toCardanoValidityRange,
    toCardanoScriptInEra,
    toCardanoPaymentKeyHash,
    toCardanoScriptData,
    toCardanoScriptDataHash,
    toCardanoScriptHash,
    toCardanoStakeKeyHash,
    toCardanoScriptInAnyLang,
    toCardanoReferenceScript,
    toCardanoTxId,
    ToCardanoError (..),
    FromCardanoError (..),
    deserialiseFromRawBytes,
    zeroExecutionUnits,
    tag,
    withIsCardanoEra,
    EmulatorEra,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Internal.Error qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.Tracer (ToObject)
import Cardano.Chain.Common (addrToBase58)
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Scripts qualified as Conway
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Lens ((<&>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseFail, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Data (Proxy (Proxy))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import GHC.Exts
import GHC.Generics (Generic)
import Ledger.Address qualified as P
import Ledger.Scripts qualified as P
import Ledger.Slot qualified as P
import Plutus.Script.Utils.Scripts (scriptCurrencySymbol, toMintingPolicyHash)
import Plutus.Script.Utils.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.Script.Utils.Value qualified as Value
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Credential qualified as Credential
import PlutusLedgerApi.V1.Tx qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty), colon, viaShow, (<+>))

type EmulatorEra = ConwayEra StandardCrypto

newtype CardanoBuildTx = CardanoBuildTx {getCardanoBuildTx :: C.TxBodyContent C.BuildTx C.ConwayEra}
  deriving (Show, Eq, Generic)

-- | Cardano tx from any era.
data CardanoTx where
  CardanoTx :: C.Tx era -> C.ShelleyBasedEra era -> CardanoTx

getEmulatorEraTx :: CardanoTx -> C.Tx C.ConwayEra
getEmulatorEraTx (CardanoTx tx C.ShelleyBasedEraConway) = tx
getEmulatorEraTx _ = error "getEmulatorEraTx: Expected a Conway tx"

pattern CardanoEmulatorEraTx :: C.Tx C.ConwayEra -> CardanoTx
pattern CardanoEmulatorEraTx tx <- (getEmulatorEraTx -> tx)
  where
    CardanoEmulatorEraTx tx = CardanoTx tx C.shelleyBasedEra

{-# COMPLETE CardanoEmulatorEraTx #-}

instance Eq CardanoTx where
  (CardanoTx tx1 C.ShelleyBasedEraShelley) == (CardanoTx tx2 C.ShelleyBasedEraShelley) = tx1 == tx2
  (CardanoTx tx1 C.ShelleyBasedEraAllegra) == (CardanoTx tx2 C.ShelleyBasedEraAllegra) = tx1 == tx2
  (CardanoTx tx1 C.ShelleyBasedEraMary) == (CardanoTx tx2 C.ShelleyBasedEraMary) = tx1 == tx2
  (CardanoTx tx1 C.ShelleyBasedEraAlonzo) == (CardanoTx tx2 C.ShelleyBasedEraAlonzo) = tx1 == tx2
  (CardanoTx tx1 C.ShelleyBasedEraBabbage) == (CardanoTx tx2 C.ShelleyBasedEraBabbage) = tx1 == tx2
  (CardanoTx tx1 C.ShelleyBasedEraConway) == (CardanoTx tx2 C.ShelleyBasedEraConway) = tx1 == tx2
  _ == _ = False

deriving instance Show CardanoTx

instance ToJSON CardanoTx where
  toJSON (CardanoTx tx sbe) =
    C.shelleyBasedEraConstraints sbe $
      object
        [ "tx" .= C.serialiseToTextEnvelope Nothing tx,
          "shelleyBasedEra" .= sbe
        ]

-- | Converting 'CardanoTx' to JSON.
--
-- If the "tx" field is from an unknown era, the JSON parser will print an
-- error at runtime while parsing.
instance FromJSON CardanoTx where
  parseJSON = parseSomeCardanoTx

-- | Run code that needs an `IsCardanoEra` constraint while you only have an `EraInMode` value.
withIsCardanoEra :: C.CardanoEra era -> ((C.IsCardanoEra era) => r) -> r
withIsCardanoEra = C.cardanoEraConstraints

parseSomeCardanoTx ::
  Aeson.Value ->
  Parser CardanoTx
parseSomeCardanoTx (Aeson.Object v) = do
  C.AnyShelleyBasedEra sbe <- v .: "shelleyBasedEra"
  envelope :: C.TextEnvelope <- v .: "tx"
  tx <-
    C.shelleyBasedEraConstraints sbe
      $ either
        (const $ parseFail "Failed to parse 'tx' field from CardanoTx")
        pure
      $ C.deserialiseFromTextEnvelope (C.AsTx (C.proxyToAsType Proxy)) envelope
  pure $ CardanoTx tx sbe
parseSomeCardanoTx invalid =
  prependFailure
    "parsing CardanoTx failed, "
    (typeMismatch "Object" invalid)

txOutRefs :: CardanoTx -> [(PV1.TxOut, PV3.TxOutRef)]
txOutRefs (CardanoTx tx _) =
  mkOut <$> zip [0 ..] plutusTxOuts
  where
    body = C.getTxBody tx
    mkOut (i, o) = (o, PV3.TxOutRef (fromCardanoTxId $ C.getTxId body) i)
    plutusTxOuts = fromCardanoTxOutToPV1TxInfoTxOut <$> C.txOuts (C.getTxBodyContent body)

unspentOutputsTx :: CardanoTx -> Map PV3.TxOutRef PV1.TxOut
unspentOutputsTx tx = Map.fromList $ swap <$> txOutRefs tx

-- | Given a 'C.TxScriptValidity era', if the @era@ supports scripts, return a
-- @True@ or @False@ depending on script validity. If the @era@ does not support
-- scripts, always return @True@.
fromTxScriptValidity :: C.TxScriptValidity era -> Bool
fromTxScriptValidity (C.TxScriptValidity _ C.ScriptValid) = True
fromTxScriptValidity (C.TxScriptValidity _ C.ScriptInvalid) = False
fromTxScriptValidity C.TxScriptValidityNone = True

toTxScriptValidity :: C.ShelleyBasedEra era -> Bool -> C.TxScriptValidity era
toTxScriptValidity C.ShelleyBasedEraAlonzo True = C.TxScriptValidity C.AlonzoEraOnwardsAlonzo C.ScriptValid
toTxScriptValidity C.ShelleyBasedEraAlonzo False = C.TxScriptValidity C.AlonzoEraOnwardsAlonzo C.ScriptInvalid
toTxScriptValidity C.ShelleyBasedEraBabbage True = C.TxScriptValidity C.AlonzoEraOnwardsBabbage C.ScriptValid
toTxScriptValidity C.ShelleyBasedEraBabbage False = C.TxScriptValidity C.AlonzoEraOnwardsBabbage C.ScriptInvalid
toTxScriptValidity C.ShelleyBasedEraConway True = C.TxScriptValidity C.AlonzoEraOnwardsConway C.ScriptValid
toTxScriptValidity C.ShelleyBasedEraConway False = C.TxScriptValidity C.AlonzoEraOnwardsConway C.ScriptInvalid
toTxScriptValidity _ _ = C.TxScriptValidityNone

withShelleyBasedEraConstraintsForLedger ::
  C.ShelleyBasedEra era -> ((Ledger.Era (C.ShelleyLedgerEra era)) => r) -> r
withShelleyBasedEraConstraintsForLedger = \case
  C.ShelleyBasedEraShelley -> id
  C.ShelleyBasedEraAllegra -> id
  C.ShelleyBasedEraMary -> id
  C.ShelleyBasedEraAlonzo -> id
  C.ShelleyBasedEraBabbage -> id
  C.ShelleyBasedEraConway -> id

-- | Given a 'C.TxBody from a 'C.Tx era', return the datums and redeemers along
-- with their hashes.
scriptDataFromCardanoTxBody ::
  C.TxBody C.ConwayEra ->
  (Map P.DatumHash P.Datum, PV1.Redeemers)
-- scriptDataFromCardanoTxBody C.ByronTxBody{} = (mempty, mempty)
scriptDataFromCardanoTxBody (C.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) =
  (mempty, mempty)
scriptDataFromCardanoTxBody
  (C.ShelleyTxBody shelleyBasedEra _ _ (C.TxBodyScriptData _ (Alonzo.TxDats' dats) reds') _ _) =
    withShelleyBasedEraConstraintsForLedger shelleyBasedEra $ case reds' of
      (Alonzo.Redeemers reds) ->
        let datums =
              Map.fromList ((\d -> (P.datumHash d, d)) . P.Datum . fromCardanoScriptData . C.fromAlonzoData <$> Map.elems dats)
            redeemers =
              Map.fromList
                $ map
                  ( \(ptr, rdmr) ->
                      (redeemerPtrFromCardanoRdmrPtr ptr, P.Redeemer $ fromCardanoScriptData $ C.fromAlonzoData $ fst rdmr)
                  )
                $ Map.toList reds
         in (datums, redeemers)

redeemerPtrFromCardanoRdmrPtr :: Alonzo.PlutusPurpose Alonzo.AsIx EmulatorEra -> PV1.RedeemerPtr
redeemerPtrFromCardanoRdmrPtr (Conway.ConwaySpending (Alonzo.AsIx ix)) = PV1.RedeemerPtr PV1.Spend (toInteger ix)
redeemerPtrFromCardanoRdmrPtr (Conway.ConwayMinting (Alonzo.AsIx ix)) = PV1.RedeemerPtr PV1.Mint (toInteger ix)
redeemerPtrFromCardanoRdmrPtr (Conway.ConwayCertifying (Alonzo.AsIx ix)) = PV1.RedeemerPtr PV1.Cert (toInteger ix)
redeemerPtrFromCardanoRdmrPtr (Conway.ConwayRewarding (Alonzo.AsIx ix)) = PV1.RedeemerPtr PV1.Reward (toInteger ix)
redeemerPtrFromCardanoRdmrPtr (Conway.ConwayVoting (Alonzo.AsIx ix)) = PV1.RedeemerPtr PV1.Reward (toInteger ix)
redeemerPtrFromCardanoRdmrPtr (Conway.ConwayProposing (Alonzo.AsIx ix)) = PV1.RedeemerPtr PV1.Reward (toInteger ix)

-- | Extract plutus scripts from a Cardano API tx body.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
plutusScriptsFromTxBody :: C.TxBody era -> Map P.ScriptHash (P.Versioned P.Script)
-- plutusScriptsFromTxBody C.ByronTxBody{} = mempty
plutusScriptsFromTxBody (C.ShelleyTxBody shelleyBasedEra _ scripts _ _ _) =
  Map.fromList $
    mapMaybe (fmap (\s -> (Scripts.toScriptHash s, s)) . fromLedgerScript shelleyBasedEra) scripts

--

-- | Convert a script from a Cardano api in shelley based era to a Plutus script along with it's hash.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
fromLedgerScript ::
  C.ShelleyBasedEra era ->
  Ledger.Script (C.ShelleyLedgerEra era) ->
  Maybe (P.Versioned P.Script)
fromLedgerScript e s = fromCardanoScriptInEra $ C.fromShelleyBasedScript e s

createTransactionBody ::
  CardanoBuildTx ->
  Either ToCardanoError (C.TxBody C.ConwayEra)
createTransactionBody (CardanoBuildTx txBodyContent) =
  first (TxBodyError . C.displayError) $
    C.createTransactionBody C.ShelleyBasedEraConway txBodyContent

fromCardanoTxIn :: C.TxIn -> PV3.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV3.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

toCardanoTxIn :: PV3.TxOutRef -> Either ToCardanoError C.TxIn
toCardanoTxIn (PV3.TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (C.TxIx (fromInteger txIx))

fromCardanoTxId :: C.TxId -> PV3.TxId
fromCardanoTxId txId = PV3.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

toCardanoTxId :: PV3.TxId -> Either ToCardanoError C.TxId
toCardanoTxId (PV3.TxId bs) =
  tag "toCardanoTxId" $
    deserialiseFromRawBytes C.AsTxId $
      PlutusTx.fromBuiltin bs

fromCardanoTxOutToPV1TxInfoTxOut :: C.TxOut C.CtxTx era -> PV1.TxOut
fromCardanoTxOutToPV1TxInfoTxOut (C.TxOut addr value datumHash _) =
  PV1.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatumHash datumHash)

fromCardanoTxOutToPV1TxInfoTxOut' :: C.TxOut C.CtxUTxO era -> PV1.TxOut
fromCardanoTxOutToPV1TxInfoTxOut' (C.TxOut addr value datumHash _) =
  PV1.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatumHash' datumHash)

fromCardanoTxOutToPV2TxInfoTxOut :: C.TxOut C.CtxTx era -> PV2.TxOut
fromCardanoTxOutToPV2TxInfoTxOut (C.TxOut addr value datum refScript) =
  PV2.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatum datum)
    (refScriptToScriptHash refScript)

fromCardanoTxOutToPV2TxInfoTxOut' :: C.TxOut C.CtxUTxO era -> PV2.TxOut
fromCardanoTxOutToPV2TxInfoTxOut' (C.TxOut addr value datum refScript) =
  PV2.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatum' datum)
    (refScriptToScriptHash refScript)

refScriptToScriptHash :: C.ReferenceScript era -> Maybe PV2.ScriptHash
refScriptToScriptHash C.ReferenceScriptNone = Nothing
refScriptToScriptHash (C.ReferenceScript _ (C.ScriptInAnyLang _ s)) =
  let (P.ScriptHash h) = fromCardanoScriptHash $ C.hashScript s
   in Just $ PV2.ScriptHash h

toCardanoTxOut ::
  C.NetworkId ->
  PV2.TxOut ->
  Either ToCardanoError (C.TxOut C.CtxTx C.ConwayEra)
toCardanoTxOut networkId (PV2.TxOut addr value datum _rsHash) =
  C.TxOut
    <$> toCardanoAddressInEra networkId addr
    <*> (toCardanoTxOutValue <$> toCardanoValue value)
    <*> toCardanoTxOutDatum datum
    <*> pure C.ReferenceScriptNone -- Not possible from just a hash

{-# DEPRECATED
  fromCardanoAddressInEra
  "we now use Cardano address internally, if you need a plutus address use 'Ledger.Address.toPlutusAddress' "
  #-}
fromCardanoAddressInEra :: C.AddressInEra era -> P.Address
fromCardanoAddressInEra = P.toPlutusAddress

{-# DEPRECATED fromCardanoAddress "Shouldn't be used as we use Cardano address internally now" #-}
fromCardanoAddress :: C.Address addrtype -> P.Address
fromCardanoAddress (C.ByronAddress address) =
  P.Address plutusCredential Nothing
  where
    plutusCredential :: Credential.Credential
    plutusCredential =
      Credential.PubKeyCredential $
        PV1.PubKeyHash $
          PlutusTx.toBuiltin $
            addrToBase58 address
fromCardanoAddress (C.ShelleyAddress _ paymentCredential stakeAddressReference) =
  P.Address (fromCardanoPaymentCredential (C.fromShelleyPaymentCredential paymentCredential)) $
    fromCardanoStakeAddressReference (C.fromShelleyStakeReference stakeAddressReference)

toCardanoAddressInEra ::
  C.NetworkId -> P.Address -> Either ToCardanoError (C.AddressInEra C.ConwayEra)
toCardanoAddressInEra networkId (P.Address addressCredential addressStakingCredential) =
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraConway)
    <$> ( C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential
        )

{-# DEPRECATED fromCardanoPaymentCredential "Shouldn't be used as we use Cardano address internally now" #-}
fromCardanoPaymentCredential :: C.PaymentCredential -> Credential.Credential
fromCardanoPaymentCredential (C.PaymentCredentialByKey paymentKeyHash) = Credential.PubKeyCredential (fromCardanoPaymentKeyHash paymentKeyHash)
fromCardanoPaymentCredential (C.PaymentCredentialByScript scriptHash) = Credential.ScriptCredential (fromCardanoScriptHash scriptHash)

toCardanoPaymentCredential :: Credential.Credential -> Either ToCardanoError C.PaymentCredential
toCardanoPaymentCredential (Credential.PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash (P.PaymentPubKeyHash pubKeyHash)
toCardanoPaymentCredential (Credential.ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

{-# DEPRECATED fromCardanoPaymentKeyHash "Shouldn't be used as we use Cardano address internally now" #-}
fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

toCardanoPaymentKeyHash :: P.PaymentPubKeyHash -> Either ToCardanoError (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (P.PaymentPubKeyHash (PV1.PubKeyHash bs)) =
  let bsx = PlutusTx.fromBuiltin bs
      tg = "toCardanoPaymentKeyHash (" <> show (BS.length bsx) <> " bytes)"
   in tag tg $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

{-# DEPRECATED fromCardanoScriptHash "Shouldn't be used as we use Cardano address internally now" #-}
fromCardanoScriptHash :: C.ScriptHash -> P.ScriptHash
fromCardanoScriptHash scriptHash = P.ScriptHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes scriptHash

toCardanoScriptHash :: P.ScriptHash -> Either ToCardanoError C.ScriptHash
toCardanoScriptHash (P.ScriptHash bs) = tag "toCardanoScriptHash" $ deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin bs

{-# DEPRECATED fromCardanoStakeAddressReference "Shouldn't be used as we use Cardano address internally now" #-}
fromCardanoStakeAddressReference :: C.StakeAddressReference -> Maybe Credential.StakingCredential
fromCardanoStakeAddressReference C.NoStakeAddress = Nothing
fromCardanoStakeAddressReference (C.StakeAddressByValue stakeCredential) =
  Just (Credential.StakingHash $ fromCardanoStakeCredential stakeCredential)
fromCardanoStakeAddressReference C.StakeAddressByPointer {} = Nothing

toCardanoStakeAddressReference ::
  Maybe Credential.StakingCredential -> Either ToCardanoError C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (Credential.StakingHash credential)) =
  C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just Credential.StakingPtr {}) = Left StakingPointersNotSupported

{-# DEPRECATED fromCardanoStakeCredential "Shouldn't be used as we use Cardano address internally now" #-}
fromCardanoStakeCredential :: C.StakeCredential -> Credential.Credential
fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) = Credential.PubKeyCredential (fromCardanoStakeKeyHash stakeKeyHash)
fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = Credential.ScriptCredential (fromCardanoScriptHash scriptHash)

toCardanoStakeCredential :: Credential.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakeCredential (Credential.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (Credential.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoStakeKeyHash :: C.Hash C.StakeKey -> PV1.PubKeyHash
fromCardanoStakeKeyHash stakeKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes stakeKeyHash

toCardanoStakeKeyHash :: PV1.PubKeyHash -> Either ToCardanoError (C.Hash C.StakeKey)
toCardanoStakeKeyHash (PV1.PubKeyHash bs) =
  tag "toCardanoStakeKeyHash" $
    deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (PlutusTx.fromBuiltin bs)

fromCardanoTxOutValue :: C.TxOutValue era -> C.Value
fromCardanoTxOutValue = C.txOutValueToValue

toCardanoTxOutValue :: C.Value -> C.TxOutValue C.ConwayEra
toCardanoTxOutValue = C.TxOutValueShelleyBased C.shelleyBasedEra . C.toMaryValue

fromCardanoTxOutDatumHash :: C.TxOutDatum C.CtxTx era -> Maybe P.DatumHash
fromCardanoTxOutDatumHash C.TxOutDatumNone = Nothing
fromCardanoTxOutDatumHash (C.TxOutDatumHash _ h) =
  Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash (C.TxOutDatumInline _ d) =
  Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d))
fromCardanoTxOutDatumHash (C.TxOutSupplementalDatum _ d) =
  Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d))

fromCardanoTxOutDatumHash' :: C.TxOutDatum C.CtxUTxO era -> Maybe P.DatumHash
fromCardanoTxOutDatumHash' C.TxOutDatumNone = Nothing
fromCardanoTxOutDatumHash' (C.TxOutDatumHash _ h) =
  Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash' (C.TxOutDatumInline _ d) =
  Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d))

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> PV2.OutputDatum
fromCardanoTxOutDatum C.TxOutDatumNone =
  PV2.NoOutputDatum
fromCardanoTxOutDatum (C.TxOutDatumHash _ h) =
  PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
  PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d
fromCardanoTxOutDatum (C.TxOutSupplementalDatum _ d) =
  PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

fromCardanoTxOutDatum' :: C.TxOutDatum C.CtxUTxO era -> PV2.OutputDatum
fromCardanoTxOutDatum' C.TxOutDatumNone =
  PV2.NoOutputDatum
fromCardanoTxOutDatum' (C.TxOutDatumHash _ h) =
  PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum' (C.TxOutDatumInline _ d) =
  PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

toCardanoTxOutNoDatum :: C.TxOutDatum C.CtxTx C.ConwayEra
toCardanoTxOutNoDatum = C.TxOutDatumNone

toCardanoTxOutDatumInline :: PV2.Datum -> C.TxOutDatum C.CtxTx C.ConwayEra
toCardanoTxOutDatumInline =
  C.TxOutDatumInline C.BabbageEraOnwardsConway
    . C.unsafeHashableScriptData
    . C.fromPlutusData
    . PV2.builtinDataToData
    . PV2.getDatum

toCardanoTxOutDatumHashFromDatum :: PV2.Datum -> C.TxOutDatum ctx C.ConwayEra
toCardanoTxOutDatumHashFromDatum =
  C.TxOutDatumHash C.AlonzoEraOnwardsConway
    . C.hashScriptDataBytes
    . C.unsafeHashableScriptData
    . C.fromPlutusData
    . PV2.builtinDataToData
    . PV2.getDatum

toCardanoTxOutDatumHash :: P.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.ConwayEra)
toCardanoTxOutDatumHash datumHash = C.TxOutDatumHash C.AlonzoEraOnwardsConway <$> toCardanoScriptDataHash datumHash

toCardanoTxOutDatum :: PV2.OutputDatum -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.ConwayEra)
toCardanoTxOutDatum PV2.NoOutputDatum = pure toCardanoTxOutNoDatum
toCardanoTxOutDatum (PV2.OutputDatum d) = pure $ toCardanoTxOutDatumInline d
toCardanoTxOutDatum (PV2.OutputDatumHash dh) = toCardanoTxOutDatumHash dh

toCardanoScriptDataHash :: P.DatumHash -> Either ToCardanoError (C.Hash C.ScriptData)
toCardanoScriptDataHash (P.DatumHash bs) =
  tag
    "toCardanoTxOutDatumHash"
    (deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs))

{-# DEPRECATED fromCardanoMintValue "Use 'txMintValueToValue' from cardano-api instead." #-}
fromCardanoMintValue :: C.TxMintValue build era -> C.Value
fromCardanoMintValue = C.txMintValueToValue

adaToCardanoValue :: Value.Lovelace -> C.Value
adaToCardanoValue (Value.Lovelace n) = fromList [(C.AdaAssetId, C.Quantity n)]

fromCardanoValue :: C.Value -> Value.Value
fromCardanoValue = foldMap fromSingleton . toList
  where
    fromSingleton (fromCardanoAssetId -> assetClass, C.Quantity quantity) =
      Value.assetClassValue assetClass quantity

toCardanoValue :: Value.Value -> Either ToCardanoError C.Value
toCardanoValue =
  fmap fromList . traverse toSingleton . Value.flattenValue
  where
    toSingleton (cs, tn, q) =
      toCardanoAssetId (Value.assetClass cs tn) <&> (,C.Quantity q)

fromCardanoPolicyId :: C.PolicyId -> P.MintingPolicyHash
fromCardanoPolicyId (C.PolicyId scriptHash) = P.MintingPolicyHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

toCardanoPolicyId :: P.MintingPolicyHash -> Either ToCardanoError C.PolicyId
toCardanoPolicyId (P.MintingPolicyHash bs) =
  tag "toCardanoPolicyId" $
    tag
      (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes")
      (deserialiseFromRawBytes C.AsPolicyId (PlutusTx.fromBuiltin bs))

fromCardanoAssetName :: C.AssetName -> Value.TokenName
fromCardanoAssetName (C.AssetName bs) = Value.TokenName $ PlutusTx.toBuiltin bs

toCardanoAssetName :: Value.TokenName -> Either ToCardanoError C.AssetName
toCardanoAssetName (Value.TokenName bs) =
  tag "toCardanoAssetName" $
    tag
      (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes")
      (deserialiseFromRawBytes C.AsAssetName (PlutusTx.fromBuiltin bs))

fromCardanoAssetId :: C.AssetId -> Value.AssetClass
fromCardanoAssetId C.AdaAssetId = Value.assetClass Value.adaSymbol Value.adaToken
fromCardanoAssetId (C.AssetId policyId assetName) =
  Value.assetClass
    (scriptCurrencySymbol . fromCardanoPolicyId $ policyId)
    (fromCardanoAssetName assetName)

toCardanoAssetId :: Value.AssetClass -> Either ToCardanoError C.AssetId
toCardanoAssetId (Value.AssetClass (currencySymbol, tokenName))
  | currencySymbol == Value.adaSymbol && tokenName == Value.adaToken =
      pure C.AdaAssetId
  | otherwise =
      C.AssetId
        <$> toCardanoPolicyId (toMintingPolicyHash currencySymbol)
        <*> toCardanoAssetName tokenName

fromCardanoFee :: C.TxFee era -> Coin
fromCardanoFee (C.TxFeeExplicit _ lovelace) = lovelace

toCardanoFee :: Coin -> C.TxFee C.ConwayEra
toCardanoFee = C.TxFeeExplicit C.shelleyBasedEra

fromCardanoLovelace :: Coin -> PV1.Value
fromCardanoLovelace (Coin lovelace) = Value.lovelace lovelace

toCardanoLovelace :: PV1.Value -> Either ToCardanoError Coin
toCardanoLovelace value =
  if value == Value.lovelace lovelace
    then pure . C.quantityToLovelace . C.Quantity $ lovelace
    else Left ValueNotPureAda
  where
    Value.Lovelace lovelace = Value.lovelaceValueOf value

fromCardanoValidityRange :: C.TxValidityLowerBound era -> C.TxValidityUpperBound era -> P.SlotRange
fromCardanoValidityRange l u = PV1.Interval (fromCardanoValidityLowerBound l) (fromCardanoValidityUpperBound u)

toCardanoValidityRange ::
  P.SlotRange ->
  Either ToCardanoError (C.TxValidityLowerBound C.ConwayEra, C.TxValidityUpperBound C.ConwayEra)
toCardanoValidityRange (PV1.Interval l u) = (,) <$> toCardanoValidityLowerBound l <*> toCardanoValidityUpperBound u

fromCardanoValidityLowerBound :: C.TxValidityLowerBound era -> PV1.LowerBound P.Slot
fromCardanoValidityLowerBound C.TxValidityNoLowerBound = PV1.LowerBound PV1.NegInf True
fromCardanoValidityLowerBound (C.TxValidityLowerBound _ slotNo) = PV1.LowerBound (PV1.Finite $ fromCardanoSlotNo slotNo) True

toCardanoValidityLowerBound ::
  PV1.LowerBound P.Slot -> Either ToCardanoError (C.TxValidityLowerBound C.ConwayEra)
toCardanoValidityLowerBound (PV1.LowerBound PV1.NegInf _) = pure C.TxValidityNoLowerBound
toCardanoValidityLowerBound (PV1.LowerBound (PV1.Finite slotNo) closed) =
  pure
    . C.TxValidityLowerBound C.AllegraEraOnwardsConway
    . toCardanoSlotNo
    $ if slotNo < 0 then 0 else if closed then slotNo else slotNo + 1
toCardanoValidityLowerBound (PV1.LowerBound PV1.PosInf _) = Left InvalidValidityRange

fromCardanoValidityUpperBound :: C.TxValidityUpperBound era -> PV1.UpperBound P.Slot
fromCardanoValidityUpperBound (C.TxValidityUpperBound _ Nothing) = PV1.UpperBound PV1.PosInf True
fromCardanoValidityUpperBound (C.TxValidityUpperBound _ (Just slotNo)) = PV1.UpperBound (PV1.Finite $ fromCardanoSlotNo slotNo) False

toCardanoValidityUpperBound ::
  PV1.UpperBound P.Slot -> Either ToCardanoError (C.TxValidityUpperBound C.ConwayEra)
toCardanoValidityUpperBound (PV1.UpperBound PV1.PosInf _) = pure $ C.TxValidityUpperBound C.shelleyBasedEra Nothing
toCardanoValidityUpperBound (PV1.UpperBound (PV1.Finite slotNo) closed) =
  pure . C.TxValidityUpperBound C.shelleyBasedEra . Just . toCardanoSlotNo $
    if closed then slotNo + 1 else slotNo
toCardanoValidityUpperBound (PV1.UpperBound PV1.NegInf _) = Left InvalidValidityRange

fromCardanoSlotNo :: C.SlotNo -> P.Slot
fromCardanoSlotNo (C.SlotNo w64) = P.Slot (toInteger w64)

toCardanoSlotNo :: P.Slot -> C.SlotNo
toCardanoSlotNo (P.Slot i) = C.SlotNo (fromInteger i)

fromCardanoScriptData :: C.HashableScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData . C.getScriptData

toCardanoScriptData :: PV1.BuiltinData -> C.HashableScriptData
toCardanoScriptData = C.unsafeHashableScriptData . C.fromPlutusData . PV1.builtinDataToData

fromCardanoScriptInEra :: C.ScriptInEra era -> Maybe (P.Versioned P.Script)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InAlonzo (C.PlutusScript C.PlutusScriptV1 script)) =
  Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InBabbage (C.PlutusScript C.PlutusScriptV1 script)) =
  Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV2InBabbage (C.PlutusScript C.PlutusScriptV2 script)) =
  Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV2)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InConway (C.PlutusScript C.PlutusScriptV1 script)) =
  Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV2InConway (C.PlutusScript C.PlutusScriptV2 script)) =
  Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV2)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV3InConway (C.PlutusScript C.PlutusScriptV3 script)) =
  Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV3)
fromCardanoScriptInEra (C.ScriptInEra _ C.SimpleScript {}) = Nothing

toCardanoScriptInEra :: P.Versioned P.Script -> C.ScriptInEra C.ConwayEra
toCardanoScriptInEra (P.Versioned (P.Script s) P.PlutusV1) =
  C.ScriptInEra C.PlutusScriptV1InConway . C.PlutusScript C.PlutusScriptV1 $
    C.PlutusScriptSerialised s
toCardanoScriptInEra (P.Versioned (P.Script s) P.PlutusV2) =
  C.ScriptInEra C.PlutusScriptV2InConway . C.PlutusScript C.PlutusScriptV2 $
    C.PlutusScriptSerialised s
toCardanoScriptInEra (P.Versioned (P.Script s) P.PlutusV3) =
  C.ScriptInEra C.PlutusScriptV3InConway . C.PlutusScript C.PlutusScriptV3 $
    C.PlutusScriptSerialised s

fromCardanoPlutusScript :: C.PlutusScript lang -> P.Script
fromCardanoPlutusScript (C.PlutusScriptSerialised s) = P.Script s

fromCardanoScriptInAnyLang :: C.ScriptInAnyLang -> Maybe (P.Versioned P.Script)
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.SimpleScript _)) = Nothing
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.PlutusScript psv ps)) = Just $ case psv of
  C.PlutusScriptV1 -> P.Versioned (fromCardanoPlutusScript ps) P.PlutusV1
  C.PlutusScriptV2 -> P.Versioned (fromCardanoPlutusScript ps) P.PlutusV2
  C.PlutusScriptV3 -> P.Versioned (fromCardanoPlutusScript ps) P.PlutusV3

toCardanoScriptInAnyLang :: P.Versioned P.Script -> C.ScriptInAnyLang
toCardanoScriptInAnyLang (P.Versioned (P.Script s) P.PlutusV1) =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) . C.PlutusScript C.PlutusScriptV1 $
    C.PlutusScriptSerialised s
toCardanoScriptInAnyLang (P.Versioned (P.Script s) P.PlutusV2) =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) . C.PlutusScript C.PlutusScriptV2 $
    C.PlutusScriptSerialised s
toCardanoScriptInAnyLang (P.Versioned (P.Script s) P.PlutusV3) =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) . C.PlutusScript C.PlutusScriptV3 $
    C.PlutusScriptSerialised s

fromCardanoReferenceScript :: C.ReferenceScript C.ConwayEra -> Maybe (P.Versioned P.Script)
fromCardanoReferenceScript C.ReferenceScriptNone = Nothing
fromCardanoReferenceScript (C.ReferenceScript _ script) = fromCardanoScriptInAnyLang script

toCardanoReferenceScript :: Maybe (P.Versioned P.Script) -> C.ReferenceScript C.ConwayEra
toCardanoReferenceScript (Just script) =
  C.ReferenceScript C.BabbageEraOnwardsConway $ toCardanoScriptInAnyLang script
toCardanoReferenceScript Nothing = C.ReferenceScriptNone

deserialiseFromRawBytes ::
  (C.SerialiseAsRawBytes t) => C.AsType t -> ByteString -> Either ToCardanoError t
deserialiseFromRawBytes asType = either (const (Left DeserialisationError)) Right . C.deserialiseFromRawBytes asType

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (Tag s)

data FromCardanoError
  = SimpleScriptsNotSupported
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToObject)

instance Pretty FromCardanoError where
  pretty SimpleScriptsNotSupported = "Simple scripts are not supported"

data ToCardanoError
  = -- | A C.TxBodyError converted to String
    TxBodyError String
  | DeserialisationError
  | InvalidValidityRange
  | ValueNotPureAda
  | OutputHasZeroAda
  | StakingPointersNotSupported
  | SimpleScriptsNotSupportedToCardano
  | MissingInputValidator
  | MissingDatum
  | MissingMintingPolicy
  | ScriptPurposeNotSupported PV1.ScriptTag
  | MissingMintingPolicyRedeemer
  | MissingStakeValidator
  | UnsupportedPlutusVersion P.Language
  | Tag String ToCardanoError
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty ToCardanoError where
  pretty (TxBodyError err) = "TxBodyError" <> colon <+> pretty err
  pretty DeserialisationError = "ByteString deserialisation failed"
  pretty InvalidValidityRange = "Invalid validity range"
  pretty ValueNotPureAda = "Fee values should only contain Ada"
  pretty OutputHasZeroAda = "Transaction outputs should not contain zero Ada"
  pretty StakingPointersNotSupported = "Staking pointers are not supported"
  pretty SimpleScriptsNotSupportedToCardano = "Simple scripts are not supported"
  pretty MissingMintingPolicy = "Missing minting policy"
  pretty (ScriptPurposeNotSupported p) = "Script purpose not supported:" <+> viaShow p
  pretty MissingMintingPolicyRedeemer = "Missing minting policy redeemer"
  pretty (UnsupportedPlutusVersion v) = "Plutus version not supported:" <+> viaShow v
  pretty MissingInputValidator = "Missing input validator."
  pretty MissingDatum = "Missing required datum."
  pretty MissingStakeValidator = "Missing stake validator."
  pretty (Tag t err) = pretty t <> colon <+> pretty err

zeroExecutionUnits :: C.ExecutionUnits
zeroExecutionUnits = C.ExecutionUnits 0 0
