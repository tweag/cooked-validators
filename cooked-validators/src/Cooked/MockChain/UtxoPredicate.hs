module Cooked.MockChain.UtxoPredicate where

import Cooked.MockChain.Wallet
import Ledger.Ada (adaSymbol, adaToken)
import qualified Ledger.Value as Pl
import qualified PlutusTx.AssocMap as Map

-- * Predicates over Utxos

-- | A 'UtxoPredicate' is a predicate that is used to filter and select utxos
--  in 'utxosSuchThat'. We declare them through a type synonym to create a convenient
--  shallow DSL of predicates.
type UtxoPredicate a = Maybe a -> Pl.Value -> Bool

-- * Basic building blocks

infixr 3 .&&

-- | Conjunction of 'UtxoPredicate's
(.&&) :: UtxoPredicate a -> UtxoPredicate a -> UtxoPredicate a
p .&& q = \md vl -> p md vl && q md vl

infixr 2 .||

-- | Disjunction of 'UtxoPredicate's
(.||) :: UtxoPredicate a -> UtxoPredicate a -> UtxoPredicate a
p .|| q = \md vl -> p md vl || q md vl

-- | Lifts a predicate over values to a 'UtxoPredicate' by ignoring the datum
valueSat :: (Pl.Value -> Bool) -> UtxoPredicate a
valueSat predi _ = predi

-- | Lifts a predicate over datums to a 'UtxoPredicate' by forcing the datum
--  to be present and satisfy the predicate.
datumSat :: (a -> Bool) -> UtxoPredicate a
datumSat _ Nothing _ = False
datumSat predi (Just a) _ = predi a

-- * Predicatesover Values

-- | Returns whether or not a given value has a currency symbol present.
hasCurrencySymbol :: Pl.CurrencySymbol -> Pl.Value -> Bool
hasCurrencySymbol cs (Pl.Value vl) = Map.member cs vl

-- | Returns whether or not a given value has an asset class present
hasAssetClass :: Pl.AssetClass -> Pl.Value -> Bool
hasAssetClass ac vl = Pl.assetClassValueOf vl ac > 0

-- | Returns whether or not a given value has multiple asset classes present
hasAssetClasses :: [Pl.AssetClass] -> Pl.Value -> Bool
hasAssetClasses acs vl = all (`hasAssetClass` vl) acs

-- | Returns whether a value contains a "quick value". Quick values are tokens
--  belonging to "quick currencies". Check 'quickValue' for more info.
hasQuickValue :: String -> Pl.Value -> Bool
hasQuickValue str = hasAssetClass (quickAssetClass str)

-- | Returns whther or not a value has no token except for Ada
hasOnlyAda :: Pl.Value -> Bool
hasOnlyAda v = case Pl.flattenValue v of
  [] -> True
  [(sym, tok, _)] -> sym == adaSymbol && tok == adaToken
  _ -> False
