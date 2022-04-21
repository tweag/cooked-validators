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

-- * Quick Values

-- $quickvalues
-- /Quick/ values are a convenience to manipulate assets that are supposed to
-- be in existence when running a mock chain. For example, a market
-- maker would exchange Ada against other assets. Yet, when writing traces
-- for such contract we'd need to define a minting policy for those tokens,
-- which is very repetivie, moreover, most of the times we'd want wallets to
-- start with some positive valance of tokens.
--
-- There are two classes of functions for using custom tokens on traces:
--
-- 1. The @quick@ prefixed functions provide access to tokens from the
--    @const (const True)@ minting policy. That is, these can be minted and
--    burnt at will, at any point in time.
--
-- 2. The @perpetual@ prefixed functions provide access to tokens from
--    the @const (const False)@ minting policy. That is, these /cannot/ ever
--    be minted or burnt and must be present in an initial distribution to be useful.
--
-- See the docs for 'InitialDistribution' for an example usage.

-- | Token name of a /quick/ asset class; prefixes the name with a @'q'@ to
-- make it easy to distinguish between quick and permanent tokens.
quickTokenName :: String -> Pl.TokenName
quickTokenName = Pl.TokenName . Pl.stringToBuiltinByteString . ("q" ++)

-- | /Quick/ asset class from a token name
quickAssetClass :: String -> Pl.AssetClass
quickAssetClass = curry Pl.AssetClass quickCurrencySymbol . quickTokenName

-- | Constructor for /quick/ values from token name and amount
quickValue :: String -> Integer -> Pl.Value
quickValue = Pl.assetClassValue . quickAssetClass

-- | Token name of a /permanent/ asset class
permanentTokenName :: String -> Pl.TokenName
permanentTokenName = Pl.TokenName . Pl.stringToBuiltinByteString . ("p" ++)

-- | /Permanent/ asset class from a token name
permanentAssetClass :: String -> Pl.AssetClass
permanentAssetClass = curry Pl.AssetClass permanentCurrencySymbol . permanentTokenName

-- | Constructor for /Permanent/ values from token name and amount
permanentValue :: String -> Integer -> Pl.Value
permanentValue = Pl.assetClassValue . permanentAssetClass

-- ** QuickValue Minting Policies

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
