{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Script.Utils.V1.Scripts
  ( module Plutus.Script.Utils.Scripts,
    toCardanoScript,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import Plutus.Script.Utils.Address
  ( ToAddress (toAddress),
    ToCardanoAddress (toCardanoAddress),
  )
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV1),
    MintingPolicy,
    Script (unScript),
    StakeValidator,
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScriptHash (toScriptHash),
    ToStakeValidatorHash (toStakeValidatorHash),
    ToValidatorHash (toValidatorHash),
    Validator,
    Versioned (Versioned),
    toMintingPolicyHash,
    toScriptHash,
    toStakeValidatorHash,
    toValidatorHash,
  )

instance ToValidatorHash Validator where
  {-# INLINEABLE toValidatorHash #-}
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV1)

instance ToMintingPolicyHash MintingPolicy where
  {-# INLINEABLE toMintingPolicyHash #-}
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV1)

instance ToStakeValidatorHash StakeValidator where
  {-# INLINEABLE toStakeValidatorHash #-}
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV1)

instance ToScriptHash Script where
  {-# INLINEABLE toScriptHash #-}
  toScriptHash = toScriptHash . (`Versioned` PlutusV1)

instance ToAddress Validator where
  {-# INLINEABLE toAddress #-}
  toAddress = toAddress . (`Versioned` PlutusV1)

toCardanoScript :: Script -> C.Api.Script C.Api.PlutusScriptV1
toCardanoScript =
  C.Api.PlutusScript C.Api.PlutusScriptV1
    . C.Api.PlutusScriptSerialised
    . unScript

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)
