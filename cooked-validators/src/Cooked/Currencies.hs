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

module Cooked.Currencies where

import qualified Ledger
import qualified Ledger.Contexts as Validation
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))

-- * QuickValue Minting Policy

{-# INLINEABLE mkQuickCurrencyPolicy #-}
mkQuickCurrencyPolicy :: () -> Ledger.ScriptContext -> Bool
mkQuickCurrencyPolicy _ _ = True

quickCurrencyPolicy :: Scripts.MintingPolicy
quickCurrencyPolicy =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkQuickCurrencyPolicy||])

quickCurrencySymbol :: Value.CurrencySymbol
quickCurrencySymbol = Validation.scriptCurrencySymbol quickCurrencyPolicy

{-# INLINEABLE mkPermanentCurrencyPolicy #-}
mkPermanentCurrencyPolicy :: () -> Ledger.ScriptContext -> Bool
mkPermanentCurrencyPolicy _ _ = False

permanentCurrencyPolicy :: Scripts.MintingPolicy
permanentCurrencyPolicy =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkPermanentCurrencyPolicy||])

permanentCurrencySymbol :: Value.CurrencySymbol
permanentCurrencySymbol = Validation.scriptCurrencySymbol permanentCurrencyPolicy
