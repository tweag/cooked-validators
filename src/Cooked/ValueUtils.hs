-- | Utilities to work with Value
module Cooked.ValueUtils
  ( flattenValueI,
    positivePart,
    negativePart,
    adaL,
    lovelace,
    ada,
  )
where

import Data.List
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value qualified as Pl
import PlutusTx.Numeric qualified as PlutusTx

flattenValueI :: Iso' Pl.Value [(Pl.AssetClass, Integer)]
flattenValueI =
  iso
    (map (\(cSymbol, tName, amount) -> (Pl.assetClass cSymbol tName, amount)) . Pl.flattenValue)
    (foldl' (\v (ac, amount) -> v <> Pl.assetClassValue ac amount) mempty)

-- | The positive part of a value. For every asset class in the given value,
-- this asset class and its amount are included in the output iff the amount is
-- strictly positive. It holds
--
-- > x == positivePart x <> Pl.negate negativePart x
positivePart :: Pl.Value -> Pl.Value
positivePart = over flattenValueI (filter $ (0 <) . snd)

-- | The negative part of a value. For every asset class in the given value,
-- this asset class and its negated amount are included in the output iff the
-- amount is strictly negative. It holds
--
-- > x == positivePart x <> Pl.negate negativePart x
negativePart :: Pl.Value -> Pl.Value
negativePart = positivePart . PlutusTx.negate

-- | Focus the Ada part in a value.
adaL :: Lens' Pl.Value Ada.Ada
adaL =
  lens
    Ada.fromValue
    ( \value (Ada.Lovelace amount) ->
        over
          flattenValueI
          (\l -> insertAssocList l adaAssetClass amount)
          value
    )
  where
    insertAssocList :: (Eq a) => [(a, b)] -> a -> b -> [(a, b)]
    insertAssocList l a b = (a, b) : filter ((/= a) . fst) l

-- * Helpers for manipulating ada and lovelace

adaAssetClass :: Pl.AssetClass
adaAssetClass = Pl.assetClass Pl.adaSymbol Pl.adaToken

lovelace :: Integer -> Pl.Value
lovelace = Pl.assetClassValue adaAssetClass

ada :: Integer -> Pl.Value
ada = lovelace . (* 1_000_000)
