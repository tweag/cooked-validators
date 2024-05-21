module Cooked.Classes where

import Cooked.Wallet
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

class HasAddress a where
  toAddress :: a -> Api.Address

instance HasAddress Wallet where
  toAddress = walletAddress

instance HasAddress Api.Address where
  toAddress = id

instance HasAddress (Script.TypedValidator a) where
  toAddress = Script.validatorAddress

class HasPubKeyHash a where
  toPubKeyHash :: a -> Api.PubKeyHash

instance HasPubKeyHash Api.PubKeyHash where
  toPubKeyHash = id

instance HasPubKeyHash Wallet where
  toPubKeyHash = walletPKHash

class ToCredential a where
  toCredential :: a -> Api.Credential

instance ToCredential Api.Credential where
  toCredential = id

instance ToCredential (Script.TypedValidator a) where
  toCredential = Api.ScriptCredential . toScriptHash . Script.tvValidatorHash

instance ToCredential Api.PubKeyHash where
  toCredential = Api.PubKeyCredential

class ToOutputDatum a where
  toOutputDatum :: a -> Api.OutputDatum

instance ToOutputDatum Api.OutputDatum where
  toOutputDatum = id

instance ToOutputDatum Api.Datum where
  toOutputDatum = Api.OutputDatum

instance ToOutputDatum () where
  toOutputDatum = const Api.NoOutputDatum

instance ToOutputDatum Api.DatumHash where
  toOutputDatum = Api.OutputDatumHash

instance ToOutputDatum Api.BuiltinData where
  toOutputDatum = toOutputDatum . Api.Datum

class ToValue a where
  toValue :: a -> Api.Value

instance ToValue Api.Value where
  toValue = id

instance ToValue Script.Ada where
  toValue = Script.toValue

class ToScript a where
  toScript :: a -> Script.Versioned Script.Script

instance ToScript (Script.Versioned Script.Script) where
  toScript = id

instance ToScript (Script.Versioned Script.Validator) where
  toScript (Script.Versioned (Script.Validator script) version) = Script.Versioned script version

instance ToScript (Script.TypedValidator a) where
  toScript = toScript . Script.vValidatorScript

class ToScriptHash a where
  toScriptHash :: a -> Api.ScriptHash

instance ToScriptHash Api.ScriptHash where
  toScriptHash = id

instance ToScriptHash (Script.Versioned Script.Script) where
  toScriptHash = Script.scriptHash

instance ToScriptHash (Script.Versioned Script.Validator) where
  toScriptHash = toScriptHash . toScript

instance ToScriptHash Script.ValidatorHash where
  toScriptHash (Script.ValidatorHash h) = Script.ScriptHash h

instance ToScriptHash (Script.TypedValidator a) where
  toScriptHash = toScriptHash . Script.tvValidator
