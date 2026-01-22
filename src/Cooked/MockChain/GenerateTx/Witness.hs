-- | This module exposes the generation of witnesses and reward account
module Cooked.MockChain.GenerateTx.Witness
  ( toRewardAccount,
    toCardanoCredential,
    toScriptWitness,
    toKeyWitness,
    toStakeCredential,
    deserialiseFromBuiltinByteString,
    toScriptHash,
    toKeyHash,
    toDRepCredential,
    toStakePoolKeyHash,
    toColdCredential,
    toHotCredential,
    toVRFVerKeyHash,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as C.Ledger
import Cardano.Ledger.Hashes qualified as C.Ledger
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cooked.MockChain.Error
import Cooked.MockChain.Read
import Cooked.Skeleton
import Ledger.Address qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Translates a given credential to a reward account.
toRewardAccount ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.Credential ->
  Sem effs C.Ledger.RewardAccount
toRewardAccount =
  (C.Ledger.RewardAccount C.Ledger.Testnet <$>)
    . toCardanoCredential Cardano.AsStakeKey Cardano.unStakeKeyHash

-- TODO: if this works, migrate to plutus-ledger

-- | Converts an 'Api.PubKeyHash' to any kind of key
deserialiseFromBuiltinByteString ::
  ( Members '[MockChainRead, Error Ledger.ToCardanoError] effs,
    Cardano.SerialiseAsRawBytes a
  ) =>
  Cardano.AsType a ->
  Api.BuiltinByteString ->
  Sem effs a
deserialiseFromBuiltinByteString asType =
  fromEither
    . Ledger.deserialiseFromRawBytes asType
    . Api.fromBuiltin

-- | Converts a plutus script hash into a cardano ledger script hash
toScriptHash ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.ScriptHash ->
  Sem effs C.Ledger.ScriptHash
toScriptHash (Api.ScriptHash sHash) = do
  Cardano.ScriptHash cHash <- deserialiseFromBuiltinByteString Cardano.AsScriptHash sHash
  return cHash

-- | Converts a plutus pkhash into a certain cardano ledger hash
toKeyHash ::
  ( Members '[MockChainRead, Error Ledger.ToCardanoError] effs,
    Cardano.SerialiseAsRawBytes (Cardano.Hash key)
  ) =>
  Cardano.AsType key ->
  (Cardano.Hash key -> C.Ledger.KeyHash kr) ->
  Api.PubKeyHash ->
  Sem effs (C.Ledger.KeyHash kr)
toKeyHash asType unwrap =
  fmap unwrap
    . deserialiseFromBuiltinByteString (Cardano.AsHash asType)
    . Api.getPubKeyHash

-- | Converts an 'Api.PubKeyHash' into a cardano ledger stake pool key hash
toStakePoolKeyHash ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.PubKeyHash ->
  Sem effs (C.Ledger.KeyHash 'C.Ledger.StakePool)
toStakePoolKeyHash = toKeyHash Cardano.AsStakePoolKey Cardano.unStakePoolKeyHash

-- | Converts an 'Api.PubKeyHash' into a cardano ledger VRFVerKeyHash
toVRFVerKeyHash ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.PubKeyHash ->
  Sem effs (C.Ledger.VRFVerKeyHash a)
toVRFVerKeyHash (Api.PubKeyHash pkh) = do
  Cardano.VrfKeyHash key <- deserialiseFromBuiltinByteString (Cardano.AsHash Cardano.AsVrfKey) pkh
  return $ C.Ledger.toVRFVerKeyHash key

-- | Converts an 'Api.Credential' to a Cardano Credential of the expected kind
toCardanoCredential ::
  ( Members '[MockChainRead, Error Ledger.ToCardanoError] effs,
    Cardano.SerialiseAsRawBytes (Cardano.Hash key)
  ) =>
  Cardano.AsType key ->
  (Cardano.Hash key -> C.Ledger.KeyHash kr) ->
  Api.Credential ->
  Sem effs (C.Ledger.Credential kr)
toCardanoCredential _ _ (Api.ScriptCredential sHash) = C.Ledger.ScriptHashObj <$> toScriptHash sHash
toCardanoCredential asType unwrap (Api.PubKeyCredential pkHash) = C.Ledger.KeyHashObj <$> toKeyHash asType unwrap pkHash

-- | Translates a credential into a Cardano stake credential
toStakeCredential ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.Staking)
toStakeCredential = toCardanoCredential Cardano.AsStakeKey Cardano.unStakeKeyHash

-- | Translates a credential into a Cardano drep credential
toDRepCredential ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.DRepRole)
toDRepCredential = toCardanoCredential Cardano.AsDRepKey Cardano.unDRepKeyHash

-- | Translates a credential into a Cardano cold committee credential
toColdCredential ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.ColdCommitteeRole)
toColdCredential = toCardanoCredential Cardano.AsCommitteeColdKey Cardano.unCommitteeColdKeyHash

-- | Translates a credential into a Cardano hot committee credential
toHotCredential ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.HotCommitteeRole)
toHotCredential = toCardanoCredential Cardano.AsCommitteeHotKey Cardano.unCommitteeHotKeyHash

-- | Translates a script and a reference script utxo into either a plutus script
-- or a reference input containing the right script
toPlutusScriptOrReferenceInput ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs) =>
  VScript ->
  Maybe Api.TxOutRef ->
  Sem effs (Cardano.PlutusScriptOrReferenceInput lang)
toPlutusScriptOrReferenceInput (Script.Versioned (Script.Script script) _) Nothing =
  return $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
toPlutusScriptOrReferenceInput (Script.toScriptHash -> scriptHash) (Just scriptOutRef) = do
  (preview txSkelOutReferenceScriptHashAF -> mScriptHash) <- txSkelOutByRef scriptOutRef
  case mScriptHash of
    Just scriptHash'
      | scriptHash == scriptHash' -> do
          s <- fromEither $ Ledger.toCardanoTxIn scriptOutRef
          return $ Cardano.PReferenceScript s
    _ -> throw $ MCEWrongReferenceScriptError scriptOutRef scriptHash mScriptHash

-- | Translates a script with its associated redeemer and datum to a script
-- witness. Note on the usage of 'Ledger.zeroExecutionUnits': at this stage of
-- the transaction create, we cannot know the execution units used by the
-- script. They will be filled out later on once the full body has been
-- generated. So, for now, we temporarily leave them to 0.
toScriptWitness ::
  ( Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs,
    ToVScript a
  ) =>
  a ->
  TxSkelRedeemer ->
  Cardano.ScriptDatum b ->
  Sem effs (Cardano.ScriptWitness b Cardano.ConwayEra)
toScriptWitness (toVScript -> script@(Script.Versioned _ version)) (TxSkelRedeemer {..}) datum = do
  let scriptData = Ledger.toCardanoScriptData $ Api.toBuiltinData txSkelRedeemerContent
  case version of
    Script.PlutusV1 ->
      (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 x datum scriptData Ledger.zeroExecutionUnits)
        <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput
    Script.PlutusV2 ->
      (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 x datum scriptData Ledger.zeroExecutionUnits)
        <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput
    Script.PlutusV3 ->
      (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 x datum scriptData Ledger.zeroExecutionUnits)
        <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput

-- | Generates a key witnesses for a given signatory and body, when the
-- signatory contains a private key.
toKeyWitness ::
  Cardano.TxBody Cardano.ConwayEra ->
  TxSkelSignatory ->
  Maybe (Cardano.KeyWitness Cardano.ConwayEra)
toKeyWitness txBody =
  fmap
    ( Cardano.makeShelleyKeyWitness Cardano.ShelleyBasedEraConway txBody
        . Ledger.toWitness
        . Ledger.PaymentPrivateKey
    )
    . preview txSkelSignatoryPrivateKeyAT
