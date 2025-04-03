{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Script.Utils.V2
  ( module X,
    toCardanoScript,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import Plutus.Script.Utils.Address as X
import Plutus.Script.Utils.Data as X
import Plutus.Script.Utils.Scripts as X
import Plutus.Script.Utils.V2.Contexts as X
import Plutus.Script.Utils.V2.Generators as X
import Plutus.Script.Utils.V2.Scripts as X
import Plutus.Script.Utils.V2.Typed as X
import Plutus.Script.Utils.Value as X

instance ToValidatorHash Validator where
  {-# INLINEABLE toValidatorHash #-}
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV2)

instance ToMintingPolicyHash MintingPolicy where
  {-# INLINEABLE toMintingPolicyHash #-}
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV2)

instance ToStakeValidatorHash StakeValidator where
  {-# INLINEABLE toStakeValidatorHash #-}
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV2)

instance ToScriptHash Script where
  {-# INLINEABLE toScriptHash #-}
  toScriptHash = toScriptHash . (`Versioned` PlutusV2)

instance ToAddress Validator where
  {-# INLINEABLE toAddress #-}
  toAddress = toAddress . (`Versioned` PlutusV2)

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

toCardanoScript :: Script -> C.Api.Script C.Api.PlutusScriptV2
toCardanoScript =
  C.Api.PlutusScript C.Api.PlutusScriptV2
    . C.Api.PlutusScriptSerialised
    . unScript
