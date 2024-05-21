-- | This module provides various wrapper classes to identify which
-- objects possess which properties. This is meant to greatly simplify
-- the use of the rest of cooked-validators. For instance, when paying
-- to a wallet, all we need is its address. Retrieving it will
-- walletAddress is inconvenient as we know a wallet possess an
-- address. By creating a type class `HasAddress` we overcome this
-- limitation and allow payments directly both to addresses or
-- wallets.
module Cooked.Wrappers where

import Cooked.Wallet
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Objects from which and address can be extracted
class HasAddress a where
  toAddress :: a -> Api.Address

instance HasAddress Wallet where
  toAddress = walletAddress

instance HasAddress Api.Address where
  toAddress = id

instance HasAddress (Script.TypedValidator a) where
  toAddress = Script.validatorAddress

-- | Objects from which a public key hash can be extracted
class HasPubKeyHash a where
  toPubKeyHash :: a -> Api.PubKeyHash

instance HasPubKeyHash Api.PubKeyHash where
  toPubKeyHash = id

instance HasPubKeyHash Wallet where
  toPubKeyHash = walletPKHash

-- | Objects from which a credential can be extracted
class ToCredential a where
  toCredential :: a -> Api.Credential

instance ToCredential Api.Credential where
  toCredential = id

instance ToCredential (Script.TypedValidator a) where
  toCredential = Api.ScriptCredential . toScriptHash

instance ToCredential Api.PubKeyHash where
  toCredential = Api.PubKeyCredential

-- | Objects from which an output datum can be extracted
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

-- | Objects from which a value can be extracted
class ToValue a where
  toValue :: a -> Api.Value

instance ToValue Api.Value where
  toValue = id

instance ToValue Script.Ada where
  toValue = Script.toValue

-- | Objects from which a versioned script can be extracted
class ToScript a where
  toScript :: a -> Script.Versioned Script.Script

instance ToScript (Script.Versioned Script.Script) where
  toScript = id

instance ToScript (Script.Versioned Script.Validator) where
  toScript (Script.Versioned (Script.Validator script) version) = Script.Versioned script version

instance ToScript (Script.TypedValidator a) where
  toScript = toScript . Script.vValidatorScript

-- | Objects from which a script hash can be extracted
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
