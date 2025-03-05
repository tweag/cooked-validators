{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | This module contains functions related to the computation of script hashes
-- for PlutusV1.
module Plutus.Script.Utils.V1.Scripts
  ( module Plutus.Script.Utils.Scripts,
    validatorHash,
    mintingPolicyHash,
    stakeValidatorHash,
    scriptHash,
    toCardanoApiScript,
  )
where

import Cardano.Api qualified as C.Api
import Cardano.Api.Shelley qualified as C.Api
import Plutus.Script.Utils.Scripts

-- | Hash a 'Validator' script.
validatorHash :: Validator -> ValidatorHash
validatorHash = toValidatorHash . (`Versioned` PlutusV1)

-- | Hash a 'MintingPolicy' script.
mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV1)

-- | Hash a 'StakeValidator' script.
stakeValidatorHash :: StakeValidator -> StakeValidatorHash
stakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV1)

-- | Hash a 'Script'
scriptHash :: Script -> ScriptHash
scriptHash = toScriptHash . (`Versioned` PlutusV1)

-- | Convert a 'Script' to a 'cardano-api' script.
--
-- For why we depend on `cardano-api`, see note [Hash computation of datums,
-- redeemers and scripts] in module Plutus.Script.Utils.Scripts
toCardanoApiScript :: Script -> C.Api.Script C.Api.PlutusScriptV1
toCardanoApiScript = C.Api.PlutusScript C.Api.PlutusScriptV1 . C.Api.PlutusScriptSerialised . unScript
