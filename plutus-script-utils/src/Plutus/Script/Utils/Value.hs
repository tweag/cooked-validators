{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-identities #-}

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
    Ada (..),
    divide,
    isZero,
    fromValue,
    ToValue (..),
  )
where

import Codec.Serialise.Class (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Optics.Core (Iso', Lens', iso, lens, over, (^.))
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
import PlutusLedgerApi.V1.Value qualified as V1 (isZero)
import PlutusTx (FromData, ToData, UnsafeFromData)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude
  ( AdditiveGroup,
    AdditiveMonoid,
    AdditiveSemigroup,
    Bool,
    Eq ((==)),
    Integer,
    Maybe (Just, Nothing),
    Monoid,
    MultiplicativeMonoid,
    MultiplicativeSemigroup,
    Ord,
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
  toValue v = v

{-# INLINEABLE isZero #-}
isZero :: (ToValue a) => a -> Bool
isZero = V1.isZero . toValue

-- | ADA, the special currency on the Cardano blockchain. The unit of Ada is Lovelace, and
--  1M Lovelace is one
--  See note [Currencies] in 'Ledger.Validation.Value.Api'.
newtype Ada = Lovelace {getLovelace :: Integer}
  deriving (Haskell.Enum)
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype
    ( Eq,
      Ord,
      Haskell.Num,
      AdditiveSemigroup,
      AdditiveMonoid,
      AdditiveGroup,
      MultiplicativeSemigroup,
      MultiplicativeMonoid,
      Haskell.Integral,
      Haskell.Real,
      Serialise,
      ToData,
      FromData,
      UnsafeFromData
    )

instance Haskell.Semigroup Ada where
  Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Semigroup Ada where
  Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Haskell.Monoid Ada where
  mempty = Lovelace 0

instance Monoid Ada where
  mempty = Lovelace 0

instance ToValue Ada where
  {-# INLINEABLE toValue #-}
  toValue (Lovelace i) = singleton adaSymbol adaToken i

makeLift ''Ada

{-# INLINEABLE divide #-}

-- | Divide one 'Ada' value by another.
divide :: Ada -> Ada -> Ada
divide (Lovelace a) (Lovelace b) = Lovelace (P.divide a b)

{-# INLINEABLE flattenValueI #-}

-- | Isomorphism between values and lists of pairs AssetClass and Integers
flattenValueI :: Iso' Value [(AssetClass, Integer)]
flattenValueI =
  iso
    (map (\(cSymbol, tName, amount) -> (assetClass cSymbol tName, amount)) . flattenValue)
    (foldl (\v (ac, amount) -> v <> assetClassValue ac amount) mempty)

{-# INLINEABLE adaL #-}

-- | Focus the Ada part in a value.
adaL :: Lens' Value Ada
adaL =
  lens
    (\v -> Lovelace (valueOf v adaSymbol adaToken))
    ( \value (Lovelace amount) ->
        over
          flattenValueI
          (((adaAssetClass, amount) :) . filter ((/= adaAssetClass) . fst))
          value
    )

{-# INLINEABLE fromValue #-}
fromValue :: Value -> Ada
fromValue = (^. adaL)

{-# INLINEABLE adaAssetClass #-}

-- | Ada asset class
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
