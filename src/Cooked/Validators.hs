{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | This module introduces standard dummy validators to be used in attacks,
-- traces or tests. More precisely, it introduces the always True and always
-- False validators, which will respectively always succeed or always fail.
module Cooked.Validators
  ( alwaysTrueValidator,
    alwaysFalseValidator,
    mkScript,
    validatorToTypedValidator,
    MockContract,
  )
where

import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies qualified as Script
import PlutusTx.Builtins.Internal qualified as PlutusTx
import PlutusTx.Code qualified as PlutusTx
import PlutusTx.TH qualified as PlutusTx

validatorToTypedValidator :: Script.Validator -> Script.TypedValidator a
validatorToTypedValidator val =
  Script.TypedValidator
    { Script.tvValidator = vValidator,
      Script.tvValidatorHash = vValidatorHash,
      Script.tvForwardingMPS = vMintingPolicy,
      Script.tvForwardingMPSHash = Script.toMintingPolicyHash vMintingPolicy
    }
  where
    vValidator = Script.Versioned val Script.PlutusV2
    vValidatorHash = Script.toValidatorHash vValidator
    forwardingPolicy = Script.mkForwardingMintingPolicy vValidatorHash
    vMintingPolicy = Script.Versioned forwardingPolicy Script.PlutusV2

-- | The trivial validator that always succeds; this is in particular a
-- sufficient target for the datum hijacking attack since we only want to show
-- feasibility of the attack.
alwaysTrueValidator :: forall a. Script.TypedValidator a
alwaysTrueValidator =
  validatorToTypedValidator @a $
    Script.toValidator
      $$( PlutusTx.compile
            [||
            \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> PlutusTx.unitval
            ||]
        )

-- | The trivial validator that always fails
alwaysFalseValidator :: forall a. Script.TypedValidator a
alwaysFalseValidator =
  validatorToTypedValidator @a $
    Script.toValidator
      $$( PlutusTx.compile
            [||
            \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> PlutusTx.error @PlutusTx.BuiltinUnit PlutusTx.unitval
            ||]
        )

-- | A Mock contract type to instantiate validators with
data MockContract

instance Script.ValidatorTypes MockContract where
  type DatumType MockContract = ()

-- | Helper to build a script. This should come from plutus-script-utils at some
-- point.
mkScript :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit) -> Script.Versioned Script.Script
mkScript = (`Script.Versioned` Script.PlutusV2) . Script.toScript
