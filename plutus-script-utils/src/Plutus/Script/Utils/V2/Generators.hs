{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Plutus.Script.Utils.V2.Generators
  ( alwaysSucceedValidator,
    alwaysFailValidator,
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
    mkForwardingMintingPolicy,
    mkForwardingStakeValidator,
  )
where

import Plutus.Script.Utils.Scripts
  ( Language (PlutusV2),
    MintingPolicy,
    MintingPolicyHash,
    StakeValidator,
    Validator,
    ValidatorHash (ValidatorHash),
    Versioned (Versioned),
    toCurrencySymbol,
    toMintingPolicy,
    toMintingPolicyHash,
    toStakeValidator,
    toValidator,
    toValidatorHash,
  )
import Plutus.Script.Utils.V2.Scripts
  ( mkUntypedMintingPolicy,
    mkUntypedStakeValidator,
  )
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2
  ( Address (Address),
    Credential (ScriptCredential),
    CurrencySymbol,
    ScriptContext (ScriptContext),
    ScriptHash (ScriptHash),
    ScriptPurpose (Certifying, Minting, Rewarding),
    TokenName,
    TxInInfo (txInInfoResolved),
    TxInfo (TxInfo, txInfoInputs),
    TxOut (TxOut),
    Value,
    singleton,
  )
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

alwaysSucceedValidatorVersioned :: Versioned Validator
alwaysSucceedValidatorVersioned = Versioned alwaysSucceedValidator PlutusV2

alwaysFailValidatorVersioned :: Versioned Validator
alwaysFailValidatorVersioned = Versioned alwaysFailValidator PlutusV2

alwaysSucceedValidatorHash :: ValidatorHash
alwaysSucceedValidatorHash = toValidatorHash alwaysSucceedValidatorVersioned

alwaysFailValidatorHash :: ValidatorHash
alwaysFailValidatorHash = toValidatorHash alwaysFailValidatorVersioned

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
alwaysSucceedPolicyHash = toMintingPolicyHash alwaysSucceedPolicyVersioned

alwaysFailPolicyHash :: MintingPolicyHash
alwaysFailPolicyHash = toMintingPolicyHash alwaysFailPolicyVersioned

alwaysSucceedCurrencySymbol :: CurrencySymbol
alwaysSucceedCurrencySymbol = toCurrencySymbol alwaysSucceedPolicyVersioned

alwaysSucceedTokenValue :: TokenName -> Integer -> Value
alwaysSucceedTokenValue = singleton alwaysSucceedCurrencySymbol

alwaysFailCurrencySymbol :: CurrencySymbol
alwaysFailCurrencySymbol = toCurrencySymbol alwaysFailPolicyVersioned

alwaysFailTokenValue :: TokenName -> Integer -> Value
alwaysFailTokenValue = singleton alwaysFailCurrencySymbol

-- | A minting policy that checks whether the validator script was run
--  in the minting transaction.
mkForwardingMintingPolicy :: ValidatorHash -> MintingPolicy
mkForwardingMintingPolicy vshsh =
  toMintingPolicy $
    $$(PlutusTx.compile [||mkUntypedMintingPolicy . forwardToValidator||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh
  where
    {-# INLINEABLE forwardToValidator #-}
    forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
    forwardToValidator (ValidatorHash h) _ (ScriptContext (TxInfo {txInfoInputs}) (Minting _)) =
      let checkHash (TxOut (Address (ScriptCredential (ScriptHash vh)) _) _ _ _) = vh == h
          checkHash _ = False
       in any (checkHash . txInInfoResolved) txInfoInputs
    forwardToValidator _ _ _ = False

-- | A stake validator that checks whether the validator script was run
--  in the right transaction.
mkForwardingStakeValidator :: ValidatorHash -> StakeValidator
mkForwardingStakeValidator vshsh =
  toStakeValidator $
    $$(PlutusTx.compile [||mkUntypedStakeValidator . forwardToValidator||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh
  where
    {-# INLINEABLE forwardToValidator #-}
    forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
    forwardToValidator (ValidatorHash h) _ (ScriptContext (TxInfo {txInfoInputs}) scriptContextPurpose) =
      let checkHash (TxOut (Address (ScriptCredential (ScriptHash vh)) _) _ _ _) = vh == h
          checkHash _ = False
          result = any (checkHash . txInInfoResolved) txInfoInputs
       in case scriptContextPurpose of
            Rewarding _ -> result
            Certifying _ -> result
            _ -> False
