-- | This module exposes the generation of various kinds of credentials
module Cooked.MockChain.GenerateTx.Credential
  ( toRewardAccount,
    toCardanoCredential,
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
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Translates a given credential to a reward account.
toRewardAccount ::
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.Credential ->
  Sem effs C.Ledger.RewardAccount
toRewardAccount =
  (C.Ledger.RewardAccount C.Ledger.Testnet <$>)
    . toCardanoCredential Cardano.AsStakeKey Cardano.unStakeKeyHash

-- TODO: if this works, migrate to plutus-ledger

-- | Converts an 'Api.PubKeyHash' to any kind of key
deserialiseFromBuiltinByteString ::
  ( Member (Error Ledger.ToCardanoError) effs,
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
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.ScriptHash ->
  Sem effs C.Ledger.ScriptHash
toScriptHash (Api.ScriptHash sHash) = do
  Cardano.ScriptHash cHash <- deserialiseFromBuiltinByteString Cardano.AsScriptHash sHash
  return cHash

-- | Converts a plutus pkhash into a certain cardano ledger hash
toKeyHash ::
  ( Member (Error Ledger.ToCardanoError) effs,
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
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.PubKeyHash ->
  Sem effs (C.Ledger.KeyHash 'C.Ledger.StakePool)
toStakePoolKeyHash = toKeyHash Cardano.AsStakePoolKey Cardano.unStakePoolKeyHash

-- | Converts an 'Api.PubKeyHash' into a cardano ledger VRFVerKeyHash
toVRFVerKeyHash ::
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.PubKeyHash ->
  Sem effs (C.Ledger.VRFVerKeyHash a)
toVRFVerKeyHash (Api.PubKeyHash pkh) = do
  Cardano.VrfKeyHash key <- deserialiseFromBuiltinByteString (Cardano.AsHash Cardano.AsVrfKey) pkh
  return $ C.Ledger.toVRFVerKeyHash key

-- | Converts an 'Api.Credential' to a Cardano Credential of the expected kind
toCardanoCredential ::
  ( Member (Error Ledger.ToCardanoError) effs,
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
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.Staking)
toStakeCredential = toCardanoCredential Cardano.AsStakeKey Cardano.unStakeKeyHash

-- | Translates a credential into a Cardano drep credential
toDRepCredential ::
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.DRepRole)
toDRepCredential = toCardanoCredential Cardano.AsDRepKey Cardano.unDRepKeyHash

-- | Translates a credential into a Cardano cold committee credential
toColdCredential ::
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.ColdCommitteeRole)
toColdCredential = toCardanoCredential Cardano.AsCommitteeColdKey Cardano.unCommitteeColdKeyHash

-- | Translates a credential into a Cardano hot committee credential
toHotCredential ::
  (Member (Error Ledger.ToCardanoError) effs) =>
  Api.Credential ->
  Sem effs (C.Ledger.Credential 'C.Ledger.HotCommitteeRole)
toHotCredential = toCardanoCredential Cardano.AsCommitteeHotKey Cardano.unCommitteeHotKeyHash
