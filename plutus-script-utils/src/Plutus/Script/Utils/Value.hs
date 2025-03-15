{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Script.Utils.Value
  ( module Export,
    adaAssetClass,
    lovelace,
    ada,
    noAdaValue,
    adaOnlyValue,
    isAdaOnlyValue,
    currencyValueOf,
    adaL,
    flattenValueI,
    divide,
    isZero,
    ToValue (..),
    getAda,
    adaOf,
    adaValueOf,
  )
where

import Codec.Serialise.Class (Serialise)
import Data.Fixed (Fixed (MkFixed), Micro)
import Optics.Core (Iso', Lens', iso, lens, over, (^.))
import PlutusLedgerApi.V1.Value as Export
  ( AssetClass (AssetClass, unAssetClass),
    CurrencySymbol (CurrencySymbol, unCurrencySymbol),
    Lovelace (Lovelace, getLovelace),
    TokenName (TokenName, unTokenName),
    Value (Value, getValue),
    adaSymbol,
    adaToken,
    assetClass,
    assetClassValue,
    assetClassValueOf,
    currencySymbol,
    currencySymbolValueOf,
    flattenValue,
    geq,
    gt,
    leq,
    lovelaceValue,
    lovelaceValueOf,
    lt,
    scale,
    singleton,
    split,
    symbols,
    toString,
    tokenName,
    unionWith,
    valueOf,
    withCurrencySymbol,
  )
import PlutusLedgerApi.V1.Value qualified as V1 (isZero)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude
  ( Bool,
    Eq ((==)),
    Integer,
    Maybe (Just, Nothing),
    Monoid,
    MultiplicativeMonoid,
    MultiplicativeSemigroup,
    Semigroup,
    filter,
    foldl,
    fst,
    map,
    mempty,
    (*),
    (+),
    (-),
    (.),
    (/=),
    (<>),
  )
import PlutusTx.Prelude qualified as P
import Prelude qualified as Haskell

class ToValue a where
  toValue :: a -> Value

instance ToValue Value where
  toValue = Haskell.id

{-# INLINEABLE isZero #-}
isZero :: (ToValue a) => a -> Bool
isZero = V1.isZero . toValue

deriving newtype instance (MultiplicativeSemigroup Lovelace)

deriving newtype instance (MultiplicativeMonoid Lovelace)

deriving newtype instance (Haskell.Integral Lovelace)

deriving newtype instance (Serialise Lovelace)

instance Haskell.Semigroup Lovelace where
  Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Semigroup Lovelace where
  Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Haskell.Monoid Lovelace where
  mempty = Lovelace 0

instance Monoid Lovelace where
  mempty = Lovelace 0

instance ToValue Lovelace where
  {-# INLINEABLE toValue #-}
  toValue (Lovelace i) = singleton adaSymbol adaToken i

{-# INLINEABLE divide #-}

-- | Divide one 'Lovelace' value by another.
divide :: Lovelace -> Lovelace -> Lovelace
divide (Lovelace a) (Lovelace b) = Lovelace (P.divide a b)

{-# INLINEABLE flattenValueI #-}

-- | Isomorphism between values and lists of pairs AssetClass and Integers
flattenValueI :: Iso' Value [(AssetClass, Integer)]
flattenValueI =
  iso
    (map (\(cSymbol, tName, amount) -> (assetClass cSymbol tName, amount)) . flattenValue)
    (foldl (\v (ac, amount) -> v <> assetClassValue ac amount) mempty)

{-# INLINEABLE adaL #-}

-- | Focus the Lovelace part in a value.
adaL :: Lens' Value Lovelace
adaL =
  lens
    lovelaceValueOf
    ( \value (Lovelace amount) ->
        over
          flattenValueI
          (((adaAssetClass, amount) :) . filter ((/= adaAssetClass) . fst))
          value
    )

{-# INLINEABLE adaAssetClass #-}

-- | Lovelace asset class
adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken

{-# INLINEABLE lovelace #-}

-- | Create a value from a certain amount of lovelace
lovelace :: Integer -> Value
lovelace = toValue . Lovelace

{-# INLINEABLE ada #-}

-- | Create a value from a certain amount of ada
ada :: Integer -> Value
ada = lovelace . (* 1_000_000)

{-# INLINEABLE noAdaValue #-}

-- | Value without any ada
noAdaValue :: Value -> Value
noAdaValue v = v - adaOnlyValue v

{-# INLINEABLE adaOnlyValue #-}

-- | Value without any non-ada tokens
adaOnlyValue :: Value -> Value
adaOnlyValue = toValue . (^. adaL)

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

{-# INLINEABLE getAda #-}

-- | Get the amount of Ada (the unit of the currency Ada) in this 'Ada' value.
getAda :: Lovelace -> Micro
getAda (Lovelace i) = MkFixed i

{-# INLINEABLE adaOf #-}

-- | Create 'Ada' representing the given quantity of Ada (1M Lovelace).
adaOf :: Micro -> Lovelace
adaOf (MkFixed x) = Lovelace x

{-# INLINEABLE adaValueOf #-}

-- | A 'Value' with the given amount of Ada (the currency unit).
--
--  @adaValueOf == toValue . adaOf@
adaValueOf :: Micro -> Value
adaValueOf (MkFixed x) = lovelace x
