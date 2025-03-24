module Plutus.Script.Utils.V2.Typed.Scripts.Validators
  ( UntypedValidator,
    ---
    ValidatorTypes (..),
    ValidatorType,
    TypedValidator,
    mkTypedValidator,
    mkTypedValidatorParam,
    forwardingMintingPolicy,
    vForwardingMintingPolicy,
    forwardingMintingPolicyHash,
    generalise,
    validatorToTypedValidator,
  )
where

import Data.Kind (Type)
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV2),
    Validator,
    Versioned (Versioned),
    toValidator,
  )
import Plutus.Script.Utils.Typed
  ( DatumType,
    RedeemerType,
    TypedValidator (TypedValidator, tvForwardingMPS, tvForwardingMPSHash, tvValidator, tvValidatorHash),
    UntypedValidator,
    ValidatorTypes,
    forwardingMintingPolicy,
    forwardingMintingPolicyHash,
    generalise,
    vForwardingMintingPolicy,
  )
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies qualified as MPS
import PlutusCore.Default (DefaultUni)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx (CompiledCode, Lift, liftCode, unsafeApplyCode)

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> PV2.ScriptContext -> Bool

validatorToTypedValidator :: Validator -> TypedValidator a
validatorToTypedValidator val =
  TypedValidator
    { tvValidator = Versioned val PlutusV2,
      tvValidatorHash = hsh,
      tvForwardingMPS = Versioned mps PlutusV2,
      tvForwardingMPSHash = Scripts.mintingPolicyHash mps
    }
  where
    hsh = Scripts.validatorHash val
    mps = MPS.mkForwardingMintingPolicy hsh

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
  -- | Validator script (compiled)
  CompiledCode (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  validatorToTypedValidator $ toValidator $ wrapper `unsafeApplyCode` vc

-- | Make a 'TypedValidator' from the 'CompiledCode' of a parameterized validator script and its wrapper.
mkTypedValidatorParam ::
  forall a param.
  (Lift DefaultUni param) =>
  -- | Validator script (compiled)
  CompiledCode (param -> ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  -- | The extra paramater for the validator script
  param ->
  TypedValidator a
mkTypedValidatorParam vc wrapper param =
  mkTypedValidator (vc `unsafeApplyCode` liftCode plcVersion100 param) wrapper
