-- | Objects from which a versioned script can be extracted
module Cooked.Conversion.ToVersionedScript where

import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script

class ToVersionedScript a where
  toVersionedScript :: a -> Script.Versioned Script.Script

instance ToVersionedScript (Script.Versioned Script.Script) where
  toVersionedScript = id

instance ToVersionedScript (Script.Versioned Script.Validator) where
  toVersionedScript (Script.Versioned (Script.Validator script) version) = Script.Versioned script version

instance ToVersionedScript (Script.TypedValidator a) where
  toVersionedScript = toVersionedScript . Script.vValidatorScript

instance ToVersionedScript (Script.Versioned Script.MintingPolicy) where
  toVersionedScript (Script.Versioned (Script.MintingPolicy script) version) = Script.Versioned script version
