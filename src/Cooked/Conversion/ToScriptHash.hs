-- | Objects from which a script hash can be extracted
module Cooked.Conversion.ToScriptHash where

import Cooked.Conversion.ToVersionedScript
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

class ToScriptHash a where
  toScriptHash :: a -> Api.ScriptHash

instance ToScriptHash Api.ScriptHash where
  toScriptHash = id

instance ToScriptHash Script.MintingPolicyHash where
  toScriptHash (Script.MintingPolicyHash hash) = Script.ScriptHash hash

instance ToScriptHash Script.ValidatorHash where
  toScriptHash (Script.ValidatorHash hash) = Script.ScriptHash hash

instance ToScriptHash (Script.Versioned Script.Script) where
  toScriptHash = Script.scriptHash

instance ToScriptHash (Script.Versioned Script.Validator) where
  toScriptHash = toScriptHash . toVersionedScript

instance ToScriptHash (Script.TypedValidator a) where
  toScriptHash = toScriptHash . toVersionedScript

instance ToScriptHash (Script.Versioned Script.MintingPolicy) where
  toScriptHash = toScriptHash . toVersionedScript
