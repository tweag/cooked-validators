-- | Objects from which a credential can be extracted
module Cooked.Conversion.ToCredential where

import Cooked.Conversion.ToScriptHash
import Cooked.Wallet
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

class ToCredential a where
  toCredential :: a -> Api.Credential

instance ToCredential Api.Credential where
  toCredential = id

instance ToCredential Api.PubKeyHash where
  toCredential = Api.PubKeyCredential

instance ToCredential Wallet where
  toCredential = toCredential . walletPKHash

instance ToCredential Api.ScriptHash where
  toCredential = Api.ScriptCredential

instance ToCredential Script.ValidatorHash where
  toCredential = toCredential . toScriptHash

instance ToCredential (Script.Versioned Script.Script) where
  toCredential = toCredential . toScriptHash

instance ToCredential (Script.Versioned Script.Validator) where
  toCredential = toCredential . toScriptHash

instance ToCredential (Script.TypedValidator a) where
  toCredential = toCredential . toScriptHash

instance ToCredential (Script.Versioned Script.MintingPolicy) where
  toCredential = toCredential . toScriptHash
