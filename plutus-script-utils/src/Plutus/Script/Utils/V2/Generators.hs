{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutus.Script.Utils.V2.Generators
  ( alwaysSucceedValidator,
    alwaysSucceedValidatorVersioned,
    alwaysSucceedValidatorHash,
    alwaysSucceedPolicy,
    alwaysSucceedPolicyVersioned,
    alwaysSucceedPolicyHash,
    someTokenValue,
  )
where

import Plutus.Script.Utils.Scripts
  ( Language (PlutusV2),
    MintingPolicy,
    MintingPolicyHash,
    Validator,
    ValidatorHash,
    Versioned (Versioned),
    scriptCurrencySymbol,
    toMintingPolicy,
    toValidator,
  )
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import PlutusLedgerApi.V1.Value (TokenName, Value, singleton)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as PlutusTx

alwaysSucceedValidator :: Validator
alwaysSucceedValidator = toValidator $$(PlutusTx.compile [||trueVal||])
  where
    trueVal :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    trueVal _ _ _ = PlutusTx.unitval

alwaysSucceedValidatorVersioned :: Versioned Validator
alwaysSucceedValidatorVersioned = Versioned alwaysSucceedValidator PlutusV2

alwaysSucceedValidatorHash :: ValidatorHash
alwaysSucceedValidatorHash = Scripts.validatorHash alwaysSucceedValidator

alwaysSucceedPolicy :: MintingPolicy
alwaysSucceedPolicy = toMintingPolicy $$(PlutusTx.compile [||trueMP||])
  where
    trueMP :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    trueMP _ _ = PlutusTx.unitval

alwaysSucceedPolicyVersioned :: Versioned MintingPolicy
alwaysSucceedPolicyVersioned = Versioned alwaysSucceedPolicy PlutusV2

alwaysSucceedPolicyHash :: MintingPolicyHash
alwaysSucceedPolicyHash = Scripts.mintingPolicyHash alwaysSucceedPolicy

someTokenValue :: TokenName -> Integer -> Value
someTokenValue = singleton (scriptCurrencySymbol alwaysSucceedPolicyVersioned)
