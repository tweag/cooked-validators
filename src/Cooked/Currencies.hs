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

-- | This module introduces currencies (namely the /quick values/ and the
-- /permanent values/) which make it convenient to manipulate assets that are
-- already supposed to exist when running a mock chain. For example, a market
-- maker would exchange Ada against other assets. Yet, when writing traces for
-- such a contract we would need to define a minting policy for those tokens,
-- which is tedious. Moreover, we often want wallets to have some of such
-- tokens from the start (see 'initialDistributions' in "Cooked.Wallet").
--
-- The @quick@ prefixed functions provide access to tokens from the @const
-- (const True)@ minting policy. That is, these can be minted and burnt at
-- will, at any point in time.
--
-- The @permanent@ prefixed functions provide access to tokens from the @const
-- (const False)@ minting policy. That is, these /cannot/ ever be minted or
-- burnt and must be present in an initial distribution (see
-- 'initialDistribution') to be useful.
module Cooked.Currencies
  ( quickTokenName,
    quickAssetClass,
    quickValue,
    permanentTokenName,
    permanentAssetClass,
    quickCurrencyPolicy,
    quickCurrencySymbol,
    permanentCurrencyPolicy,
    permanentCurrencySymbol,
  )
where

import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Scripts as Validation
import qualified Plutus.V1.Ledger.Scripts as V1
import qualified Plutus.V1.Ledger.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx
import qualified PlutusTx.Builtins.Class as Pl
import PlutusTx.Prelude hiding (Applicative (..))
import qualified Prelude as Haskell

-- * Quick Values

-- | Token name of a /quick/ asset class; prefixes the name with a @'q'@ to
-- make it easy to distinguish between quick and permanent tokens.
quickTokenName :: Haskell.String -> Pl.TokenName
quickTokenName = Pl.TokenName . Pl.stringToBuiltinByteString

-- | /Quick/ asset class from a token name
quickAssetClass :: Haskell.String -> Pl.AssetClass
quickAssetClass = curry Pl.AssetClass quickCurrencySymbol . quickTokenName

-- | Constructor for /quick/ values from token name and amount
quickValue :: Haskell.String -> Integer -> Pl.Value
quickValue = Pl.assetClassValue . quickAssetClass

{-# INLINEABLE mkQuickCurrencyPolicy #-}
mkQuickCurrencyPolicy :: () -> Pl.ScriptContext -> Bool
mkQuickCurrencyPolicy _ _ = True

quickCurrencyPolicy :: Scripts.MintingPolicy
quickCurrencyPolicy =
  V1.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy mkQuickCurrencyPolicy||])

quickCurrencySymbol :: Pl.CurrencySymbol
quickCurrencySymbol = Validation.scriptCurrencySymbol quickCurrencyPolicy

-- * Permanent values

-- | Token name of a /permanent/ asset class
permanentTokenName :: Haskell.String -> Pl.TokenName
permanentTokenName = Pl.TokenName . Pl.stringToBuiltinByteString

-- | /Permanent/ asset class from a token name
permanentAssetClass :: Haskell.String -> Pl.AssetClass
permanentAssetClass = curry Pl.AssetClass permanentCurrencySymbol . permanentTokenName

-- | Constructor for /Permanent/ values from token name and amount
permanentValue :: Haskell.String -> Integer -> Pl.Value
permanentValue = Pl.assetClassValue . permanentAssetClass

{-# INLINEABLE mkPermanentCurrencyPolicy #-}
mkPermanentCurrencyPolicy :: () -> Pl.ScriptContext -> Bool
mkPermanentCurrencyPolicy _ _ = False

permanentCurrencyPolicy :: Scripts.MintingPolicy
permanentCurrencyPolicy =
  V1.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy mkPermanentCurrencyPolicy||])

permanentCurrencySymbol :: Pl.CurrencySymbol
permanentCurrencySymbol = Validation.scriptCurrencySymbol permanentCurrencyPolicy
