-- | This module introduces standard dummy validators to be used in attacks,
-- traces or tests. More precisely, it introduces the always True and always
-- False validators, which will respectively always succeed or always fail.
module Cooked.Validators
  ( alwaysTrueValidator,
    alwaysFalseValidator,
    alwaysFalseProposingValidator,
    alwaysTrueProposingValidator,
    mkScript,
    validatorToTypedValidator,
    validatorToTypedValidatorV2,
    MockContract,
  )
where

import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.V3.Generators qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Code qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.TH qualified as PlutusTx

validatorToTypedValidator :: Script.Validator -> Script.TypedValidator a
validatorToTypedValidator val =
  Script.TypedValidator
    { Script.tvValidator = vValidator,
      Script.tvValidatorHash = vValidatorHash,
      Script.tvForwardingMPS = vMintingPolicy,
      Script.tvForwardingMPSHash = Script.mintingPolicyHash vMintingPolicy
    }
  where
    vValidator = Script.Versioned val Script.PlutusV3
    vValidatorHash = Script.validatorHash vValidator
    forwardingPolicy = Script.mkForwardingMintingPolicy vValidatorHash
    vMintingPolicy = Script.Versioned forwardingPolicy Script.PlutusV3

validatorToTypedValidatorV2 :: Script.Validator -> Script.TypedValidator a
validatorToTypedValidatorV2 val =
  Script.TypedValidator
    { Script.tvValidator = vValidator,
      Script.tvValidatorHash = vValidatorHash,
      Script.tvForwardingMPS = vMintingPolicy,
      Script.tvForwardingMPSHash = Script.mintingPolicyHash vMintingPolicy
    }
  where
    vValidator = Script.Versioned val Script.PlutusV2
    vValidatorHash = Script.validatorHash vValidator
    forwardingPolicy = Script.mkForwardingMintingPolicy vValidatorHash
    vMintingPolicy = Script.Versioned forwardingPolicy Script.PlutusV2

-- | The trivial validator that always succeds; this is in particular a
-- sufficient target for the datum hijacking attack since we only want to show
-- feasibility of the attack.
alwaysTrueValidator :: forall a. Script.TypedValidator a
alwaysTrueValidator = validatorToTypedValidator @a Script.alwaysSucceedValidator

-- | The trivial validator that always fails
alwaysFalseValidator :: forall a. Script.TypedValidator a
alwaysFalseValidator = validatorToTypedValidator @a $ Script.mkValidatorScript $$(PlutusTx.compile [||\_ _ _ -> PlutusTx.error ()||])

-- | A Mock contract type to instantiate validators with
data MockContract

instance Script.ValidatorTypes MockContract where
  type RedeemerType MockContract = ()
  type DatumType MockContract = ()

-- | A dummy false proposing validator
alwaysFalseProposingValidator :: Script.Versioned Script.Script
alwaysFalseProposingValidator =
  mkScript $$(PlutusTx.compile [||PlutusTx.traceError "False proposing validator"||])

-- | A dummy true proposing validator
alwaysTrueProposingValidator :: Script.Versioned Script.Script
alwaysTrueProposingValidator =
  mkScript $$(PlutusTx.compile [||\_ _ -> ()||])

-- | Helper to build a script. This should come from plutus-script-utils at some
-- point.
mkScript :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()) -> Script.Versioned Script.Script
mkScript code = Script.Versioned (Script.Script $ Api.serialiseCompiledCode code) Script.PlutusV3
