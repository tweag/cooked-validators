{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Tx
  ( module Export,
    C.TxId (..),
    C.TxIn (..),
    C.TxIx (..),

    -- * DecoratedTxOut
    DecoratedTxOut (..),
    toTxOut,
    toTxInfoTxOut,
    toDecoratedTxOut,

    -- ** Lenses and Prisms
    decoratedTxOutPubKeyHash,
    decoratedTxOutAddress,
    decoratedTxOutDatum,
    decoratedTxOutValue,
    decoratedTxOutPubKeyDatum,
    decoratedTxOutScriptDatum,
    decoratedTxOutStakingCredential,
    decoratedTxOutReferenceScript,
    decoratedTxOutValidatorHash,
    decoratedTxOutValidator,
    _PublicKeyDecoratedTxOut,
    _ScriptDecoratedTxOut,
    _decoratedTxOutAddress,

    -- ** smart Constructors
    mkDecoratedTxOut,
    mkPubkeyDecoratedTxOut,
    mkScriptDecoratedTxOut,

    -- * DatumFromQuery
    DatumFromQuery (..),
    datumInDatumFromQuery,

    -- * Transactions
    getCardanoTxId,
    getCardanoTxInputs,
    getCardanoTxCollateralInputs,
    getCardanoTxOutRefs,
    getCardanoTxOutputs,
    getCardanoTxRedeemers,
    getCardanoTxSpentOutputs,
    getCardanoTxProducedOutputs,
    getCardanoTxReturnCollateral,
    getCardanoTxProducedReturnCollateral,
    getCardanoTxTotalCollateral,
    getCardanoTxFee,
    getCardanoTxMint,
    getCardanoTxValidityRange,
    getCardanoTxData,
    CardanoTx (.., CardanoEmulatorEraTx),
    ToCardanoError (..),
    addCardanoTxWitness,

    -- * TxBodyContent functions
    getTxBodyContentInputs,
    getTxBodyContentCollateralInputs,
    getTxBodyContentReturnCollateral,
    getTxBodyContentMint,
    getTxBodyContentCerts,
    txBodyContentIns,
    txBodyContentCollateralIns,
    txBodyContentOuts,

    -- * Utility
    decoratedTxOutPlutusValue,
    fromDecoratedIndex,
  )
where

-- for re-export

import Cardano.Api (TxBodyContent (txValidityUpperBound))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits (txwitsVKey)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Shelley.TxCert qualified as C.Api
import Codec.Serialise (Serialise)
import Control.Lens
  ( Getter,
    Lens',
    Traversal',
    lens,
    makeLenses,
    makePrisms,
    to,
    view,
    views,
    (^.),
    (^?),
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.List (genericLength)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered.Strict qualified as OMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Ledger.Address (Address, CardanoAddress, cardanoAddressCredential, cardanoStakingCredential)
import Ledger.Index.Internal (UtxoIndex)
import Ledger.Orphans ()
import Ledger.Slot (SlotRange)
import Ledger.Tx.CardanoAPI
  ( CardanoTx (CardanoTx),
    ToCardanoError (..),
    pattern CardanoEmulatorEraTx,
  )
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.Internal as Export
import Plutus.Script.Utils.Scripts (Script, Validator, ValidatorHash (..), toScriptHash)
import PlutusLedgerApi.V1 qualified as V1 hiding (TxOutRef (..))
import PlutusLedgerApi.V1.Tx as Export hiding
  ( TxId (..),
    TxOut (..),
    TxOutRef (..),
    outAddress,
    outValue,
    txOutDatum,
    txOutPubKey,
  )
import PlutusLedgerApi.V1.Value (Value)
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V2.Tx qualified as V2.Tx hiding (TxId (..))
import Prettyprinter (Pretty (pretty), colon, hang, nest, viaShow, vsep, (<+>))

-- | A datum in a transaction output that comes from a chain index query.
data DatumFromQuery
  = DatumUnknown
  | DatumInline V2.Datum
  | DatumInBody V2.Datum
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON)

makePrisms ''DatumFromQuery

datumInDatumFromQuery :: Traversal' DatumFromQuery V2.Datum
datumInDatumFromQuery _ DatumUnknown = pure DatumUnknown
datumInDatumFromQuery f (DatumInline d) = DatumInline <$> f d
datumInDatumFromQuery f (DatumInBody d) = DatumInBody <$> f d

-- | Offchain view of a transaction output.
data DecoratedTxOut
  = PublicKeyDecoratedTxOut
      { -- | The pubKey hash that protects the transaction address
        _decoratedTxOutPubKeyHash :: V1.PubKeyHash,
        -- | The staking credential of the transaction address, if any
        _decoratedTxOutStakingCredential :: Maybe V1.StakingCredential,
        -- | Value of the transaction output.
        _decoratedTxOutValue :: C.Value,
        -- | Optional datum (inline datum or datum in transaction body) attached to the transaction output.
        _decoratedTxOutPubKeyDatum :: Maybe (V2.DatumHash, DatumFromQuery),
        -- | Value of the transaction output.
        _decoratedTxOutReferenceScript :: Maybe (Versioned Script)
      }
  | ScriptDecoratedTxOut
      { -- | The hash of the script that protects the transaction address
        _decoratedTxOutValidatorHash :: ValidatorHash,
        -- | The staking credential of the transaction address, if any
        _decoratedTxOutStakingCredential :: Maybe V1.StakingCredential,
        -- | Value of the transaction output.
        _decoratedTxOutValue :: C.Value,
        -- | Datum attached to the transaction output, either in full (inline datum or datum in transaction body) or as a
        -- hash reference. A transaction output protected by a Plutus script
        -- is guardateed to have an associated datum.
        _decoratedTxOutScriptDatum :: (V2.DatumHash, DatumFromQuery),
        -- The reference script is, in genereal, unrelated to the validator
        -- script althought it could also be the same.
        _decoratedTxOutReferenceScript :: Maybe (Versioned Script),
        -- | Full version of the validator protecting the transaction output
        _decoratedTxOutValidator :: Maybe (Versioned Validator)
      }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON)

makeLenses ''DecoratedTxOut
makePrisms ''DecoratedTxOut

mkDecoratedTxOut ::
  CardanoAddress ->
  C.Value ->
  Maybe (V2.DatumHash, DatumFromQuery) ->
  Maybe (Versioned Script) ->
  Maybe DecoratedTxOut
mkDecoratedTxOut a v md rs =
  let sc = cardanoStakingCredential a
   in case cardanoAddressCredential a of
        (V2.PubKeyCredential c) -> Just (PublicKeyDecoratedTxOut c sc v md rs)
        (V2.ScriptCredential (V2.ScriptHash c)) -> (\dt -> ScriptDecoratedTxOut (ValidatorHash c) sc v dt rs Nothing) <$> md

mkPubkeyDecoratedTxOut ::
  CardanoAddress ->
  C.Value ->
  Maybe (V2.DatumHash, DatumFromQuery) ->
  Maybe (Versioned Script) ->
  Maybe DecoratedTxOut
mkPubkeyDecoratedTxOut a v dat rs =
  let sc = cardanoStakingCredential a
   in case cardanoAddressCredential a of
        (V2.PubKeyCredential c) -> Just $ PublicKeyDecoratedTxOut c sc v dat rs
        _ -> Nothing

mkScriptDecoratedTxOut ::
  CardanoAddress ->
  C.Value ->
  (V2.DatumHash, DatumFromQuery) ->
  Maybe (Versioned Script) ->
  Maybe (Versioned Validator) ->
  Maybe DecoratedTxOut
mkScriptDecoratedTxOut a v dat rs val =
  let sc = cardanoStakingCredential a
   in case cardanoAddressCredential a of
        (V2.ScriptCredential (V2.ScriptHash c)) -> pure $ ScriptDecoratedTxOut (ValidatorHash c) sc v dat rs val
        _ -> Nothing

_decoratedTxOutAddress :: DecoratedTxOut -> Address
_decoratedTxOutAddress PublicKeyDecoratedTxOut {_decoratedTxOutPubKeyHash, _decoratedTxOutStakingCredential} =
  V1.Address (V1.PubKeyCredential _decoratedTxOutPubKeyHash) _decoratedTxOutStakingCredential
_decoratedTxOutAddress ScriptDecoratedTxOut {_decoratedTxOutValidatorHash, _decoratedTxOutStakingCredential} =
  V1.Address
    (V1.ScriptCredential (V2.ScriptHash (getValidatorHash _decoratedTxOutValidatorHash)))
    _decoratedTxOutStakingCredential

decoratedTxOutAddress :: Getter DecoratedTxOut Address
decoratedTxOutAddress = to _decoratedTxOutAddress

decoratedTxOutDatum :: Traversal' DecoratedTxOut (V2.DatumHash, DatumFromQuery)
decoratedTxOutDatum f p@(PublicKeyDecoratedTxOut pkh sc v dat rs) =
  maybe (pure p) (fmap (\dat' -> PublicKeyDecoratedTxOut pkh sc v (Just dat') rs) . f) dat
decoratedTxOutDatum f (ScriptDecoratedTxOut vh sc v dat rs val) =
  (\dat' -> ScriptDecoratedTxOut vh sc v dat' rs val) <$> f dat

toDecoratedTxOut :: TxOut -> Maybe DecoratedTxOut
toDecoratedTxOut (TxOut (C.TxOut addr' val dt rs)) =
  mkDecoratedTxOut
    addr'
    (C.txOutValueToValue val)
    (toDecoratedDatum dt)
    (CardanoAPI.fromCardanoReferenceScript rs)
  where
    toDecoratedDatum :: C.TxOutDatum C.CtxTx C.ConwayEra -> Maybe (V2.DatumHash, DatumFromQuery)
    toDecoratedDatum C.TxOutDatumNone =
      Nothing
    toDecoratedDatum (C.TxOutDatumHash _ h) =
      Just (V2.DatumHash $ V2.toBuiltin (C.serialiseToRawBytes h), DatumUnknown)
    toDecoratedDatum (C.TxOutSupplementalDatum _ d) =
      Just
        ( V2.DatumHash $ V2.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d)),
          DatumInBody $ V2.Datum $ CardanoAPI.fromCardanoScriptData d
        )
    toDecoratedDatum (C.TxOutDatumInline _ d) =
      Just
        ( V2.DatumHash $ V2.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d)),
          DatumInline $ V2.Datum $ CardanoAPI.fromCardanoScriptData d
        )

toTxOut :: C.NetworkId -> DecoratedTxOut -> Either ToCardanoError TxOut
toTxOut networkId p =
  TxOut
    <$> ( C.TxOut
            <$> CardanoAPI.toCardanoAddressInEra networkId (p ^. decoratedTxOutAddress)
            <*> pure (CardanoAPI.toCardanoTxOutValue (p ^. decoratedTxOutValue))
            <*> toTxOutDatum (p ^? decoratedTxOutDatum)
            <*> pure (CardanoAPI.toCardanoReferenceScript (p ^. decoratedTxOutReferenceScript))
        )

toTxOutDatum ::
  Maybe (V2.DatumHash, DatumFromQuery) -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.ConwayEra)
toTxOutDatum = CardanoAPI.toCardanoTxOutDatum . toPlutusOutputDatum

-- | Converts a transaction output from the chain index to the plutus-ledger-api
-- transaction output.
--
-- Note that 'DecoratedTxOut' supports features such inline datums and
-- reference scripts which are not supported by V1 TxOut. Converting from
-- 'DecoratedTxOut' to 'TxOut' and back is therefore lossy.
toTxInfoTxOut :: DecoratedTxOut -> V2.Tx.TxOut
toTxInfoTxOut p =
  V2.Tx.TxOut
    (p ^. decoratedTxOutAddress)
    (CardanoAPI.fromCardanoValue $ p ^. decoratedTxOutValue)
    (toPlutusOutputDatum $ p ^? decoratedTxOutDatum)
    (views decoratedTxOutReferenceScript (fmap toScriptHash) p)

toPlutusOutputDatum :: Maybe (V2.DatumHash, DatumFromQuery) -> V2.Tx.OutputDatum
toPlutusOutputDatum Nothing = V2.Tx.NoOutputDatum
toPlutusOutputDatum (Just (_, DatumInline d)) = V2.Tx.OutputDatum d
toPlutusOutputDatum (Just (dh, _)) = V2.Tx.OutputDatumHash dh

fromDecoratedIndex ::
  C.Api.NetworkId -> Map TxOutRef DecoratedTxOut -> Either ToCardanoError UtxoIndex
fromDecoratedIndex networkId m = C.UTxO . Map.fromList <$> traverse toCardanoUtxo (Map.toList m)
  where
    toCardanoUtxo (outRef, txOut) = do
      txOut' <- toCtxUTxOTxOut <$> toTxOut networkId txOut
      txIn <- CardanoAPI.toCardanoTxIn outRef
      pure (txIn, txOut')

instance Pretty DecoratedTxOut where
  pretty p =
    hang 2 $
      vsep
        [ "-" <+> pretty (p ^. decoratedTxOutValue) <+> "addressed to",
          pretty (p ^. decoratedTxOutAddress)
        ]

instance Pretty CardanoTx where
  pretty tx =
    let renderScriptWitnesses (CardanoEmulatorEraTx (C.Api.Tx (C.Api.ShelleyTxBody _ _ scripts _ _ _) _)) =
          [hang 2 (vsep ("attached scripts:" : fmap viaShow scripts)) | not (null scripts)]
        lines' =
          [ hang 2 (vsep ("inputs:" : fmap (("-" <+>) . pretty) (getCardanoTxInputs tx))),
            hang 2 (vsep ("reference inputs:" : fmap (("-" <+>) . pretty) (getCardanoTxReferenceInputs tx))),
            hang 2 (vsep ("collateral inputs:" : fmap (("-" <+>) . pretty) (getCardanoTxCollateralInputs tx))),
            hang 2 (vsep ("outputs:" : fmap pretty (getCardanoTxOutputs tx)))
          ]
            <> maybe
              []
              (\out -> [hang 2 (vsep ["return collateral:", pretty out])])
              (getCardanoTxReturnCollateral tx)
            <> maybe [] (\val -> ["total collateral:" <+> pretty val]) (getCardanoTxTotalCollateral tx)
            ++ [ "mint:" <+> pretty (getCardanoTxMint tx),
                 "fee:" <+> pretty (getCardanoTxFee tx),
                 "validity range:" <+> viaShow (getCardanoTxValidityRange tx),
                 hang 2 (vsep ("data:" : fmap pretty (Map.toList (getCardanoTxData tx)))),
                 hang
                   2
                   ( vsep
                       ( "redeemers:"
                           : fmap
                             (\(k, V2.Redeemer red) -> viaShow k <+> ":" <+> viaShow red)
                             (Map.toList $ getCardanoTxRedeemers tx)
                       )
                   )
               ]
            ++ [ hang 2 (vsep ("required signatures:" : (viaShow <$> wits)))
                 | let wits = getCardanoTxExtraKeyWitnesses tx,
                   not (null wits)
               ]
            ++ renderScriptWitnesses tx
     in nest 2 $ vsep ["Tx" <+> pretty (getCardanoTxId tx) <> colon, vsep lines']

instance Pretty CardanoAPI.CardanoBuildTx where
  pretty txBodyContent = case C.makeSignedTransaction [] <$> CardanoAPI.createTransactionBody txBodyContent of
    Right tx -> pretty $ CardanoEmulatorEraTx tx
    _ -> viaShow txBodyContent

getTxBodyContent :: CardanoTx -> C.TxBodyContent C.ViewTx C.ConwayEra
getTxBodyContent (CardanoEmulatorEraTx tx) = C.getTxBodyContent $ C.getTxBody tx

getCardanoTxId :: CardanoTx -> C.TxId
getCardanoTxId = getCardanoApiTxId

getCardanoApiTxId :: CardanoTx -> C.TxId
getCardanoApiTxId (CardanoTx (C.Tx body _) _) = C.getTxId body

getCardanoTxInputs :: CardanoTx -> [C.TxIn]
getCardanoTxInputs = getTxBodyContentInputs . getTxBodyContent

getTxBodyContentInputs :: C.TxBodyContent ctx era -> [C.TxIn]
getTxBodyContentInputs C.TxBodyContent {..} =
  fmap fst txIns

getCardanoTxCollateralInputs :: CardanoTx -> [C.TxIn]
getCardanoTxCollateralInputs = getTxBodyContentCollateralInputs . getTxBodyContent

getTxBodyContentCollateralInputs :: C.TxBodyContent ctx era -> [C.TxIn]
getTxBodyContentCollateralInputs C.TxBodyContent {..} = CardanoAPI.fromCardanoTxInsCollateral txInsCollateral

getCardanoTxReferenceInputs :: CardanoTx -> [C.TxIn]
getCardanoTxReferenceInputs (CardanoTx tx _) = txInsReferenceToTxIns $ C.txInsReference $ C.getTxBodyContent $ C.getTxBody tx
  where
    txInsReferenceToTxIns C.TxInsReferenceNone = []
    txInsReferenceToTxIns (C.TxInsReference _ txIns') = txIns'

getCardanoTxOutRefs :: CardanoTx -> [(TxOut, C.TxIn)]
getCardanoTxOutRefs (CardanoEmulatorEraTx tx) =
  mkOut <$> zip [0 ..] (coerce $ C.txOuts $ C.getTxBodyContent $ C.getTxBody tx)
  where
    mkOut (i, o) = (o, C.TxIn (C.getTxId $ C.getTxBody tx) (C.TxIx i))

getCardanoTxOutputs :: CardanoTx -> [TxOut]
getCardanoTxOutputs = fmap fst . getCardanoTxOutRefs

getCardanoTxProducedOutputs :: CardanoTx -> Map C.TxIn TxOut
getCardanoTxProducedOutputs = Map.fromList . fmap swap . getCardanoTxOutRefs

getCardanoTxSpentOutputs :: CardanoTx -> Set C.TxIn
getCardanoTxSpentOutputs = Set.fromList . getCardanoTxInputs

getCardanoTxReturnCollateral :: CardanoTx -> Maybe TxOut
getCardanoTxReturnCollateral = getTxBodyContentReturnCollateral . getTxBodyContent

getTxBodyContentReturnCollateral :: C.TxBodyContent ctx C.Api.ConwayEra -> Maybe TxOut
getTxBodyContentReturnCollateral C.TxBodyContent {..} =
  case txReturnCollateral of
    C.TxReturnCollateralNone -> Nothing
    C.TxReturnCollateral _ txOut -> Just $ TxOut txOut

getCardanoTxProducedReturnCollateral :: CardanoTx -> Map C.TxIn TxOut
getCardanoTxProducedReturnCollateral tx@(CardanoEmulatorEraTx tx') =
  maybe Map.empty (Map.singleton (C.TxIn (getCardanoTxId tx) (C.TxIx (genericLength $ C.txOuts $ C.getTxBodyContent $ C.getTxBody tx')))) $
    getCardanoTxReturnCollateral tx

getCardanoTxTotalCollateral :: CardanoTx -> Maybe Coin
getCardanoTxTotalCollateral (CardanoEmulatorEraTx tx) =
  CardanoAPI.fromCardanoTotalCollateral $ C.txTotalCollateral $ C.getTxBodyContent $ C.getTxBody tx

getCardanoTxFee :: CardanoTx -> Coin
getCardanoTxFee (CardanoTx tx _) = CardanoAPI.fromCardanoFee $ C.txFee $ C.getTxBodyContent $ C.getTxBody tx

getCardanoTxMint :: CardanoTx -> C.Value
getCardanoTxMint = getTxBodyContentMint . getTxBodyContent

getTxBodyContentMint :: C.TxBodyContent ctx era -> C.Value
getTxBodyContentMint C.TxBodyContent {..} = C.txMintValueToValue txMintValue

getCardanoTxValidityRange :: CardanoTx -> SlotRange
getCardanoTxValidityRange (CardanoTx tx _) =
  let bodyContent = C.getTxBodyContent $ C.getTxBody tx
   in CardanoAPI.fromCardanoValidityRange (C.txValidityLowerBound bodyContent) (C.txValidityUpperBound bodyContent)

getCardanoTxData :: CardanoTx -> Map V1.DatumHash V1.Datum
getCardanoTxData (CardanoEmulatorEraTx (C.Tx txBody _)) = fst $ CardanoAPI.scriptDataFromCardanoTxBody txBody

getTxBodyContentCerts :: C.TxBodyContent ctx era -> [C.Api.TxCert (C.Api.ShelleyLedgerEra era)]
getTxBodyContentCerts C.TxBodyContent {..} = case txCertificates of
  C.TxCertificatesNone -> mempty
  C.TxCertificates _ certs -> C.Api.toShelleyCertificate . fst <$> OMap.toAscList certs

-- TODO: add txMetaData

txBodyContentIns ::
  Lens'
    (C.TxBodyContent C.BuildTx C.ConwayEra)
    [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.ConwayEra))]
txBodyContentIns = lens C.txIns (\bodyContent ins -> bodyContent {C.txIns = ins})

txBodyContentCollateralIns :: Lens' (C.TxBodyContent C.BuildTx C.ConwayEra) [C.TxIn]
txBodyContentCollateralIns =
  lens
    ( \bodyContent -> case C.txInsCollateral bodyContent of
        C.TxInsCollateralNone -> []
        C.TxInsCollateral _ txIns -> txIns
    )
    ( \bodyContent ins ->
        bodyContent
          { C.txInsCollateral = case ins of [] -> C.TxInsCollateralNone; _ -> C.TxInsCollateral C.AlonzoEraOnwardsConway ins
          }
    )

txBodyContentOuts :: Lens' (C.TxBodyContent ctx C.ConwayEra) [TxOut]
txBodyContentOuts = lens (map TxOut . C.txOuts) (\bodyContent outs -> bodyContent {C.txOuts = map getTxOut outs})

getCardanoTxRedeemers :: CardanoTx -> V2.Tx.Redeemers
getCardanoTxRedeemers (CardanoEmulatorEraTx (C.Tx txBody _)) = snd $ CardanoAPI.scriptDataFromCardanoTxBody txBody

getCardanoTxExtraKeyWitnesses :: CardanoTx -> [C.Hash C.PaymentKey]
getCardanoTxExtraKeyWitnesses (CardanoEmulatorEraTx tx) = case C.txExtraKeyWits $ C.getTxBodyContent $ C.getTxBody tx of
  C.Api.TxExtraKeyWitnessesNone -> mempty
  C.Api.TxExtraKeyWitnesses _ txwits -> txwits

addCardanoTxWitness :: C.Api.ShelleyWitnessSigningKey -> CardanoTx -> CardanoTx
addCardanoTxWitness witness (CardanoEmulatorEraTx ctx) = CardanoEmulatorEraTx (addWitness ctx)
  where
    addWitness (C.Api.ShelleyTx shelleyBasedEra (AlonzoTx body wits isValid aux)) =
      C.Api.ShelleyTx shelleyBasedEra (AlonzoTx body wits' isValid aux)
      where
        wits' = wits <> mempty {txwitsVKey = newWits}
        newWits = case fromShelleyWitnessSigningKey body of
          C.Api.ShelleyKeyWitness _ wit -> Set.singleton wit
          _ -> Set.empty

    fromShelleyWitnessSigningKey txBody =
      C.Api.makeShelleyKeyWitness
        C.shelleyBasedEra
        (C.Api.ShelleyTxBody C.Api.ShelleyBasedEraConway txBody notUsed notUsed notUsed notUsed)
        witness
      where
        notUsed = undefined -- hack so we can reuse code from cardano-api

decoratedTxOutPlutusValue :: DecoratedTxOut -> Value
decoratedTxOutPlutusValue = CardanoAPI.fromCardanoValue . view decoratedTxOutValue
