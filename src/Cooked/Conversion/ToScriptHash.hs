-- | Objects from which a script hash can be extracted
module Cooked.Conversion.ToScriptHash where

import Cooked.Conversion.ToScript
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Scripts qualified as Script (scriptHash)
import PlutusLedgerApi.V3 qualified as Api

class ToScriptHash a where
  toScriptHash :: a -> Api.ScriptHash

instance ToScriptHash Api.ScriptHash where
  toScriptHash = id

instance ToScriptHash Script.Script where
  toScriptHash = Script.scriptHash

instance ToScriptHash Api.SerialisedScript where
  toScriptHash = toScriptHash . Script.Script

instance ToScriptHash Script.ValidatorHash where
  toScriptHash (Script.ValidatorHash h) = Script.ScriptHash h

instance ToScriptHash (Script.Versioned Script.Script) where
  toScriptHash = Script.scriptHash

instance ToScriptHash (Script.Versioned Script.Validator) where
  toScriptHash = toScriptHash . toScript

instance ToScriptHash (Script.TypedValidator a) where
  toScriptHash = toScriptHash . toScript

instance ToScriptHash (Script.Versioned Script.MintingPolicy) where
  toScriptHash = toScriptHash . toScript
