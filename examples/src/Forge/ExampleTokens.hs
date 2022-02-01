{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Forge.ExampleTokens where

import qualified Forge
import qualified Ledger
import qualified Ledger.Contexts as Validation
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude hiding (Applicative (..))

-- * Minting Policies

-- The NFT to control everything.
bigBossTok :: Value.TokenName
bigBossTok = Value.TokenName "BigBossNFT"

-- The MintingPolicy itself is parameterized by an output of a given transaction
type BigBossId = (Ledger.TxId, Integer)

bigBossPolicy :: BigBossId -> Scripts.MintingPolicy
bigBossPolicy = Currency.curPolicy . flip Currency.OneShotCurrency (AssocMap.fromList [(bigBossTok, 1)])

bigBossCurr :: BigBossId -> Value.CurrencySymbol
bigBossCurr = Validation.scriptCurrencySymbol . bigBossPolicy

bigBossNFT :: BigBossId -> Value.AssetClass
bigBossNFT hi = Value.assetClass (bigBossCurr hi) bigBossTok

-- An auth token can only be minted or destroyed if the BigBossNFT is used.
-- It is important to note that one has to create a function taking a 'Pl.AssetClass' argument,
-- because if we directly put 'bigBossNFT', then we obtain a 'CekEvaluationFailure',
-- due to a non-inlinable thing in its definition.
{-# INLINEABLE mkAuthTokenPolicy #-}
mkAuthTokenPolicy :: Value.AssetClass -> () -> Ledger.ScriptContext -> Bool
mkAuthTokenPolicy nft _ ctx =
  traceIfFalse "NFT missing" (Value.assetClassValueOf (Validation.valueSpent info) nft == 1)
  where
    info :: Validation.TxInfo
    info = Validation.scriptContextTxInfo ctx

authTokenPolicy :: (Ledger.TxId, Integer) -> Scripts.MintingPolicy
authTokenPolicy hi =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkAuthTokenPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (bigBossNFT hi)

authTokenCurrency :: BigBossId -> Value.CurrencySymbol
authTokenCurrency = Validation.scriptCurrencySymbol . authTokenPolicy

authTokenTok :: Value.TokenName
authTokenTok = Value.TokenName "ForgeAuth"

authToken :: BigBossId -> Value.AssetClass
authToken hi = Value.assetClass (authTokenCurrency hi) authTokenTok

-- The smithed token can only be forged if the auth token is used.

{-# INLINEABLE mkSmithingPolicy #-}
mkSmithingPolicy :: Value.AssetClass -> () -> Ledger.ScriptContext -> Bool
mkSmithingPolicy authTok _ ctx =
  traceIfFalse "AuthToken missing" (Value.assetClassValueOf (Validation.valueSpent info) authTok == 1)
  where
    info :: Validation.TxInfo
    info = Validation.scriptContextTxInfo ctx

smithingPolicy :: BigBossId -> Scripts.MintingPolicy
smithingPolicy hi =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkSmithingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (authToken hi)

smithingCurrency :: BigBossId -> Value.CurrencySymbol
smithingCurrency = Validation.scriptCurrencySymbol . smithingPolicy

smithingToken :: Value.TokenName
smithingToken = Value.TokenName "Token"

smithed :: BigBossId -> Value.AssetClass
smithed hi = Value.assetClass (smithingCurrency hi) smithingToken

params :: BigBossId -> Forge.Params
params hi = Forge.Params (bigBossNFT hi) (authToken hi) (smithed hi)

bigBossVal :: BigBossId -> Scripts.TypedValidator Forge.BigBoss
bigBossVal = Forge.bigBossTypedValidator . params

smithVal :: BigBossId -> Scripts.TypedValidator Forge.Smith
smithVal = Forge.smithTypedValidator . params
