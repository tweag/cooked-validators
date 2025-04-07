{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Script.Utils.V3
  ( module X,
    toCardanoScript,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import Plutus.Script.Utils.Address as X
import Plutus.Script.Utils.Data as X
import Plutus.Script.Utils.Scripts as X
import Plutus.Script.Utils.V3.Contexts as X
import Plutus.Script.Utils.V3.Generators as X
import Plutus.Script.Utils.V3.Typed as X
import Plutus.Script.Utils.Value as X

instance ToValidatorHash Validator where
  {-# INLINEABLE toValidatorHash #-}
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV3)

instance ToMintingPolicyHash MintingPolicy where
  {-# INLINEABLE toMintingPolicyHash #-}
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV3)

instance ToStakeValidatorHash StakeValidator where
  {-# INLINEABLE toStakeValidatorHash #-}
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV3)

instance ToScriptHash Script where
  {-# INLINEABLE toScriptHash #-}
  toScriptHash = toScriptHash . (`Versioned` PlutusV3)

instance ToAddress Validator where
  {-# INLINEABLE toAddress #-}
  toAddress = toAddress . (`Versioned` PlutusV3)

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

toCardanoScript :: Script -> C.Api.Script C.Api.PlutusScriptV3
toCardanoScript =
  C.Api.PlutusScript C.Api.PlutusScriptV3
    . C.Api.PlutusScriptSerialised
    . unScript
