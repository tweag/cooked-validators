-- | Objects from which a versioned script can be extracted
module Cooked.Conversion.ToScript where

import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script

class ToScript a where
  toScript :: a -> Script.Versioned Script.Script

instance ToScript (Script.Versioned Script.Script) where
  toScript = id

instance ToScript (Script.Versioned Script.Validator) where
  toScript (Script.Versioned (Script.Validator script) version) = Script.Versioned script version

instance ToScript (Script.TypedValidator a) where
  toScript = toScript . Script.vValidatorScript

instance ToScript (Script.Versioned Script.MintingPolicy) where
  toScript (Script.Versioned (Script.MintingPolicy script) version) = Script.Versioned script version
