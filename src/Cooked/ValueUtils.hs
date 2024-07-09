-- | This module provides utilities to work with Value
module Cooked.ValueUtils
  ( flattenValueI,
    adaL,
    lovelace,
    ada,
  )
where

import Data.List
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Value qualified as Script hiding (adaSymbol, adaToken)
import PlutusLedgerApi.V3 qualified as Api

flattenValueI :: Iso' Api.Value [(Script.AssetClass, Integer)]
flattenValueI =
  iso
    (map (\(cSymbol, tName, amount) -> (Script.assetClass cSymbol tName, amount)) . Script.flattenValue)
    (foldl' (\v (ac, amount) -> v <> Script.assetClassValue ac amount) mempty)

-- | Focus the Ada part in a value.
adaL :: Lens' Api.Value Script.Ada
adaL =
  lens
    Script.fromValue
    ( \value (Script.Lovelace amount) ->
        over flattenValueI (insertAssocList adaAssetClass amount) value
    )
  where
    insertAssocList :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
    insertAssocList a b l = (a, b) : filter ((/= a) . fst) l

-- * Helpers for manipulating ada and lovelace

adaAssetClass :: Script.AssetClass
adaAssetClass = Script.assetClass Script.adaSymbol Script.adaToken

lovelace :: Integer -> Api.Value
lovelace = Script.assetClassValue adaAssetClass

ada :: Integer -> Api.Value
ada = lovelace . (* 1_000_000)
