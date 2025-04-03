{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Test where

import Cardano.Api qualified as C.Api
import Ledger.Address qualified as Ledger
import Ledger.Value.CardanoAPI (policyId)
import Plutus.Script.Utils.Address (ToCardanoAddress (toCardanoAddress))
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV1, PlutusV2, PlutusV3),
    MintingPolicy,
    MintingPolicyHash,
    Validator,
    ValidatorHash,
    Versioned (Versioned),
    toCurrencySymbol,
    toMintingPolicy,
    toMintingPolicyHash,
    toValidator,
  )
import Plutus.Script.Utils.V1 (Any, TypedValidator)
import Plutus.Script.Utils.V1 qualified as V1
import Plutus.Script.Utils.V2 qualified as V2
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as PlutusTx
import Prelude hiding (not)

someCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
someCode = $$(PlutusTx.compile [||\_ _ _ -> PlutusTx.unitval||])

someValidator :: Validator
someValidator = toValidator someCode

someVersionedValidator :: Versioned Validator
someVersionedValidator = Versioned someValidator PlutusV1

someTypedValidator :: TypedValidator Any
someTypedValidator = V1.validatorToTypedValidator someValidator

someValidatorHash :: ValidatorHash
someValidatorHash = V1.tvValidatorHash someTypedValidator

someCardanoAddress :: C.Api.NetworkId -> Ledger.CardanoAddress
someCardanoAddress = flip toCardanoAddress someVersionedValidator

someAddress :: V1.Address
someAddress = Ledger.scriptValidatorHashAddress someValidatorHash Nothing

someValidatorV2 :: Validator
someValidatorV2 = toValidator someCode

someVersionedValidatorV2 :: Versioned Validator
someVersionedValidatorV2 = Versioned someValidatorV2 PlutusV2

someTypedValidatorV2 :: TypedValidator Any
someTypedValidatorV2 = V2.validatorToTypedValidator someValidator

someValidatorHashV2 :: ValidatorHash
someValidatorHashV2 = V2.tvValidatorHash someTypedValidatorV2

someCardanoAddressV2 :: C.Api.NetworkId -> Ledger.CardanoAddress
someCardanoAddressV2 = flip toCardanoAddress someVersionedValidatorV2

someAddressV2 :: V2.Address
someAddressV2 = Ledger.scriptValidatorHashAddress someValidatorHashV2 Nothing

{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> V1.ScriptContext -> Bool
mkPolicy _ _ = True

{-# INLINEABLE mkPolicyV2 #-}
mkPolicyV2 :: () -> V2.ScriptContext -> Bool
mkPolicyV2 _ _ = True

coinMintingPolicy :: Language -> Versioned MintingPolicy
coinMintingPolicy lang = case lang of
  PlutusV1 -> Versioned coinMintingPolicyV1 lang
  PlutusV2 -> Versioned coinMintingPolicyV2 lang
  PlutusV3 -> error "Unsupported"

coinMintingPolicyV1 :: MintingPolicy
coinMintingPolicyV1 = toMintingPolicy $$(PlutusTx.compile [||V1.mkUntypedMintingPolicy mkPolicy||])

coinMintingPolicyV2 :: MintingPolicy
coinMintingPolicyV2 = toMintingPolicy $$(PlutusTx.compile [||V2.mkUntypedMintingPolicy mkPolicyV2||])

coinMintingPolicyHash :: Language -> MintingPolicyHash
coinMintingPolicyHash = toMintingPolicyHash . coinMintingPolicy

coinMintingPolicyCurrencySymbol :: Language -> V1.CurrencySymbol
coinMintingPolicyCurrencySymbol = toCurrencySymbol . coinMintingPolicyHash

someToken :: Language -> V1.Value
someToken lang = V1.singleton (coinMintingPolicyCurrencySymbol lang) (V1.TokenName "someToken") 1

asRedeemer :: (PlutusTx.ToData a) => a -> V2.Redeemer
asRedeemer a = V2.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

asDatum :: (PlutusTx.ToData a) => a -> V2.Datum
asDatum a = V2.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

coinMintingPolicyId :: Language -> C.Api.PolicyId
coinMintingPolicyId = policyId . coinMintingPolicy

testNetworkMagic :: C.Api.NetworkMagic
testNetworkMagic = C.Api.NetworkMagic 1097911063

testnet :: C.Api.NetworkId
testnet = C.Api.Testnet testNetworkMagic
