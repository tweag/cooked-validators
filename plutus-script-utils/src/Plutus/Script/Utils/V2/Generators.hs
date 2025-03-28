{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Plutus.Script.Utils.V2.Generators
  ( alwaysSucceedValidator,
    alwaysFailValidator,
    alwaysSucceedTypedValidator,
    alwaysFailTypedValidator,
    alwaysSucceedValidatorVersioned,
    alwaysFailValidatorVersioned,
    alwaysSucceedValidatorHash,
    alwaysFailValidatorHash,
    alwaysSucceedPolicy,
    alwaysFailPolicy,
    alwaysSucceedPolicyVersioned,
    alwaysFailPolicyVersioned,
    alwaysSucceedPolicyHash,
    alwaysFailPolicyHash,
    alwaysSucceedCurrencySymbol,
    alwaysSucceedTokenValue,
    alwaysFailCurrencySymbol,
    alwaysFailTokenValue,
  )
where

import Plutus.Script.Utils.Scripts
  ( Language (PlutusV2),
    MintingPolicy,
    MintingPolicyHash,
    Validator,
    ValidatorHash,
    Versioned (Versioned),
    toCurrencySymbol,
    toMintingPolicy,
    toMintingPolicyHash,
    toValidator,
    toValidatorHash,
  )
import Plutus.Script.Utils.V2.Scripts ()
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (TypedValidator, validatorToTypedValidator)
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName, Value, singleton)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as PlutusTx

alwaysSucceedValidator :: Validator
alwaysSucceedValidator = toValidator $$(PlutusTx.compile [||trueVal||])
  where
    trueVal :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    trueVal _ _ _ = PlutusTx.unitval

alwaysFailValidator :: Validator
alwaysFailValidator = toValidator $$(PlutusTx.compile [||falseVal||])
  where
    falseVal :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    falseVal _ _ _ = PlutusTx.error PlutusTx.unitval

alwaysSucceedTypedValidator :: TypedValidator a
alwaysSucceedTypedValidator = validatorToTypedValidator alwaysSucceedValidator

alwaysFailTypedValidator :: TypedValidator a
alwaysFailTypedValidator = validatorToTypedValidator alwaysFailValidator

alwaysSucceedValidatorVersioned :: Versioned Validator
alwaysSucceedValidatorVersioned = Versioned alwaysSucceedValidator PlutusV2

alwaysFailValidatorVersioned :: Versioned Validator
alwaysFailValidatorVersioned = Versioned alwaysFailValidator PlutusV2

alwaysSucceedValidatorHash :: ValidatorHash
alwaysSucceedValidatorHash = toValidatorHash alwaysSucceedValidator

alwaysFailValidatorHash :: ValidatorHash
alwaysFailValidatorHash = toValidatorHash alwaysFailValidator

alwaysSucceedPolicy :: MintingPolicy
alwaysSucceedPolicy = toMintingPolicy $$(PlutusTx.compile [||trueMP||])
  where
    trueMP :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    trueMP _ _ = PlutusTx.unitval

alwaysFailPolicy :: MintingPolicy
alwaysFailPolicy = toMintingPolicy $$(PlutusTx.compile [||falseMP||])
  where
    falseMP :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    falseMP _ _ = PlutusTx.error PlutusTx.unitval

alwaysSucceedPolicyVersioned :: Versioned MintingPolicy
alwaysSucceedPolicyVersioned = Versioned alwaysSucceedPolicy PlutusV2

alwaysFailPolicyVersioned :: Versioned MintingPolicy
alwaysFailPolicyVersioned = Versioned alwaysFailPolicy PlutusV2

alwaysSucceedPolicyHash :: MintingPolicyHash
alwaysSucceedPolicyHash = toMintingPolicyHash alwaysSucceedPolicy

alwaysFailPolicyHash :: MintingPolicyHash
alwaysFailPolicyHash = toMintingPolicyHash alwaysFailPolicy

alwaysSucceedCurrencySymbol :: CurrencySymbol
alwaysSucceedCurrencySymbol = toCurrencySymbol alwaysSucceedPolicyVersioned

alwaysSucceedTokenValue :: TokenName -> Integer -> Value
alwaysSucceedTokenValue = singleton alwaysSucceedCurrencySymbol

alwaysFailCurrencySymbol :: CurrencySymbol
alwaysFailCurrencySymbol = toCurrencySymbol alwaysFailPolicyVersioned

alwaysFailTokenValue :: TokenName -> Integer -> Value
alwaysFailTokenValue = singleton alwaysFailCurrencySymbol
