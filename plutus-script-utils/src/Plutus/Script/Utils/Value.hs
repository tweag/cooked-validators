{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.Value
  ( module Export,
    adaAssetClass,
    lovelace,
    ada,
    noAdaValue,
    adaOnlyValue,
    isAdaOnlyValue,
    currencyValueOf,
    mpsSymbol,
    currencyMPSHash,
    adaL,
    flattenValueI,
  )
where

import Optics.Core (Iso', Lens', iso, lens, over)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Scripts (MintingPolicyHash (MintingPolicyHash))
import PlutusLedgerApi.V1.Value as Export
  ( AssetClass (AssetClass, unAssetClass),
    CurrencySymbol (CurrencySymbol, unCurrencySymbol),
    TokenName (TokenName, unTokenName),
    Value (Value, getValue),
    adaSymbol,
    adaToken,
    assetClass,
    assetClassValue,
    assetClassValueOf,
    currencySymbol,
    flattenValue,
    geq,
    gt,
    isZero,
    leq,
    lt,
    scale,
    singleton,
    split,
    symbols,
    toString,
    tokenName,
    unionWith,
    valueOf,
  )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude
  ( Bool,
    Eq ((==)),
    Integer,
    Maybe (Just, Nothing),
    filter,
    foldl,
    fst,
    map,
    mempty,
    (*),
    (-),
    (.),
    (/=),
    (<>),
  )

{-# INLINEABLE adaAssetClass #-}

-- | Ada asset class
adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken

{-# INLINEABLE lovelace #-}

-- | Create a value from a certain amount of lovelace
lovelace :: Integer -> Value
lovelace = Ada.toValue . Ada.Lovelace

{-# INLINEABLE ada #-}

-- | Create a value from a certain amount of ada
ada :: Integer -> Value
ada = lovelace . (* 1_000_000)

{-# INLINEABLE noAdaValue #-}

-- | Value without any Ada.
noAdaValue :: Value -> Value
noAdaValue v = v - adaOnlyValue v

{-# INLINEABLE adaOnlyValue #-}

-- | Value without any non-Ada.
adaOnlyValue :: Value -> Value
adaOnlyValue v = Ada.toValue (Ada.fromValue v)

{-# INLINEABLE isAdaOnlyValue #-}

-- | Check if a value only contains ada
isAdaOnlyValue :: Value -> Bool
isAdaOnlyValue v = adaOnlyValue v == v

{-# INLINEABLE currencyValueOf #-}

-- | Get the quantities of just the given 'CurrencySymbol' in the 'Value'. This
-- is useful when implementing minting policies as they are responsible for
-- checking all minted/burnt tokens of their own 'CurrencySymbol'.
currencyValueOf :: Value -> CurrencySymbol -> Value
currencyValueOf (Value m) c = case Map.lookup c m of
  Nothing -> mempty
  Just t -> Value (Map.singleton c t)

{-# INLINEABLE mpsSymbol #-}

-- | The currency symbol of a monetay policy hash
mpsSymbol :: MintingPolicyHash -> CurrencySymbol
mpsSymbol (MintingPolicyHash h) = CurrencySymbol h

{-# INLINEABLE currencyMPSHash #-}

-- | The minting policy hash of a currency symbol
currencyMPSHash :: CurrencySymbol -> MintingPolicyHash
currencyMPSHash (CurrencySymbol h) = MintingPolicyHash h

{-# INLINEABLE flattenValueI #-}

-- | Isomorphism between values and lists of pairs AssetClass and Integers
flattenValueI :: Iso' Value [(AssetClass, Integer)]
flattenValueI =
  iso
    (map (\(cSymbol, tName, amount) -> (assetClass cSymbol tName, amount)) . flattenValue)
    (foldl (\v (ac, amount) -> v <> assetClassValue ac amount) mempty)

{-# INLINEABLE adaL #-}

-- | Focus the Ada part in a value.
adaL :: Lens' Value Ada.Ada
adaL =
  lens
    Ada.fromValue
    ( \value (Ada.Lovelace amount) ->
        over
          flattenValueI
          (((adaAssetClass, amount) :) . filter ((/= adaAssetClass) . fst))
          value
    )
