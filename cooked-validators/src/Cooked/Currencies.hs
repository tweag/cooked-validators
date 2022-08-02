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

import Cooked.PlutusDeps
import qualified Prelude as Haskell

-- * Quick Values

-- $quickvalues
-- /Quick/ values are convenient to manipulate assets that are supposed to
-- exist when running a mock chain. For example, a market
-- maker would exchange Ada against other assets. Yet, when writing traces
-- for such contract we'd need to define a minting policy for those tokens,
-- which is very repetitive. Moreover, most of the times we'd want wallets to
-- start with some positive balance of tokens.
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
quickTokenName :: Haskell.String -> TokenName
quickTokenName = TokenName . stringToBuiltinByteString

-- | /Quick/ asset class from a token name
quickAssetClass :: Haskell.String -> AssetClass
quickAssetClass = curry AssetClass quickCurrencySymbol . quickTokenName

-- | Constructor for /quick/ values from token name and amount
quickValue :: Haskell.String -> Integer -> Value
quickValue = assetClassValue . quickAssetClass

-- | Token name of a /permanent/ asset class
permanentTokenName :: Haskell.String -> TokenName
permanentTokenName = TokenName . stringToBuiltinByteString

-- | /Permanent/ asset class from a token name
permanentAssetClass :: Haskell.String -> AssetClass
permanentAssetClass = curry AssetClass permanentCurrencySymbol . permanentTokenName

-- | Constructor for /Permanent/ values from token name and amount
permanentValue :: Haskell.String -> Integer -> Value
permanentValue = assetClassValue . permanentAssetClass

-- ** QuickValue Minting Policies

{-# INLINEABLE mkQuickCurrencyPolicy #-}
mkQuickCurrencyPolicy :: () -> ScriptContext -> Bool
mkQuickCurrencyPolicy _ _ = True

quickCurrencyPolicy :: MintingPolicy
quickCurrencyPolicy =
  mkMintingPolicyScript
    $$(compile [||mkUntypedMintingPolicy mkQuickCurrencyPolicy||])

quickCurrencySymbol :: CurrencySymbol
quickCurrencySymbol = scriptCurrencySymbol quickCurrencyPolicy

{-# INLINEABLE mkPermanentCurrencyPolicy #-}
mkPermanentCurrencyPolicy :: () -> ScriptContext -> Bool
mkPermanentCurrencyPolicy _ _ = False

permanentCurrencyPolicy :: MintingPolicy
permanentCurrencyPolicy =
  mkMintingPolicyScript
    $$(compile [||mkUntypedMintingPolicy mkPermanentCurrencyPolicy||])

permanentCurrencySymbol :: CurrencySymbol
permanentCurrencySymbol = scriptCurrencySymbol permanentCurrencyPolicy
