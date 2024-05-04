{-# LANGUAGE NoImplicitPrelude #-}

-- | This module introduces currencies (namely the /quick values/ and
-- the /permanent values/) which make it convenient to manipulate
-- assets that are already supposed to exist when running a mock
-- chain. For example, a market maker would exchange Ada against other
-- assets. Yet, when writing traces for such a contract we would need
-- to define a minting policy for those tokens, which is
-- tedious. Moreover, we often want wallets to have some of such
-- tokens from the start (see "Cooked.InitialDistribution").
--
-- The @quick@ prefixed functions provide access to tokens from the
-- @const (const True)@ minting policy. That is, these can be minted
-- and burnt at will, at any point in time.
--
-- The @permanent@ prefixed functions provide access to tokens from
-- the @const (const False)@ minting policy. That is, these /cannot/
-- ever be minted or burnt and must be present in an initial
-- distribution.
module Cooked.Currencies
  ( quickTokenName,
    quickAssetClass,
    quickValue,
    permanentTokenName,
    permanentAssetClass,
    permanentValue,
    quickCurrencyPolicy,
    quickCurrencySymbol,
    permanentCurrencyPolicy,
    permanentCurrencySymbol,
    currencySymbolFromLanguageAndMP,
  )
where

import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3.Contexts qualified as Api
import PlutusTx qualified
import PlutusTx.Builtins.Class qualified as PlutusTx
import PlutusTx.Prelude
import Prelude qualified as Haskell

-- | Takes a minting policy and a language version and returns the
-- associated currency symbol
currencySymbolFromLanguageAndMP :: Script.Language -> Script.MintingPolicy -> Script.CurrencySymbol
currencySymbolFromLanguageAndMP lang = Script.scriptCurrencySymbol . flip Script.Versioned lang

-- * Quick Values

-- | Token name of a /quick/ asset class; prefixes the name with a
-- @'q'@ to make it easy to distinguish between quick and permanent
-- tokens.
quickTokenName :: Haskell.String -> Script.TokenName
quickTokenName = Script.TokenName . PlutusTx.stringToBuiltinByteString

-- | /Quick/ asset class from a token name
quickAssetClass :: Haskell.String -> Script.AssetClass
quickAssetClass = Script.assetClass quickCurrencySymbol . quickTokenName

-- | Constructor for /quick/ values from token name and amount
quickValue :: Haskell.String -> Integer -> Script.Value
quickValue = Script.assetClassValue . quickAssetClass

{-# INLINEABLE mkQuickCurrencyPolicy #-}
mkQuickCurrencyPolicy :: () -> Api.ScriptContext -> Bool
mkQuickCurrencyPolicy _ _ = True

quickCurrencyPolicy :: Script.MintingPolicy
quickCurrencyPolicy = Script.mkMintingPolicyScript $$(PlutusTx.compile [||Script.mkUntypedMintingPolicy mkQuickCurrencyPolicy||])

quickCurrencySymbol :: Script.CurrencySymbol
quickCurrencySymbol = currencySymbolFromLanguageAndMP Script.PlutusV3 quickCurrencyPolicy

-- * Permanent values

-- | Token name of a /permanent/ asset class
permanentTokenName :: Haskell.String -> Script.TokenName
permanentTokenName = Script.TokenName . PlutusTx.stringToBuiltinByteString

-- | /Permanent/ asset class from a token name
permanentAssetClass :: Haskell.String -> Script.AssetClass
permanentAssetClass = Script.assetClass permanentCurrencySymbol . permanentTokenName

-- | Constructor for /Permanent/ values from token name and amount
permanentValue :: Haskell.String -> Integer -> Script.Value
permanentValue = Script.assetClassValue . permanentAssetClass

{-# INLINEABLE mkPermanentCurrencyPolicy #-}
mkPermanentCurrencyPolicy :: () -> Api.ScriptContext -> Bool
mkPermanentCurrencyPolicy _ _ = False

permanentCurrencyPolicy :: Script.MintingPolicy
permanentCurrencyPolicy = Script.mkMintingPolicyScript $$(PlutusTx.compile [||Script.mkUntypedMintingPolicy mkPermanentCurrencyPolicy||])

permanentCurrencySymbol :: Script.CurrencySymbol
permanentCurrencySymbol = currencySymbolFromLanguageAndMP Script.PlutusV3 permanentCurrencyPolicy
