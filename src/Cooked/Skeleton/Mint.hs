{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.Skeleton.Mint
  ( TxSkelMints,
    addToTxSkelMints,
    txSkelMintsToList,
    txSkelMintsFromList,
    txSkelMintsFromList',
    txSkelMintsValue,
  )
where

import Cooked.Skeleton.Redeemer as X
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script hiding (adaSymbol, adaToken)
import PlutusLedgerApi.V3 qualified as Api
import Test.QuickCheck (NonZero (..))

-- | A description of what a transaction mints. For every policy, there can only
-- be one 'TxSkelRedeemer', and if there is, there must be some token names, each
-- with a non-zero amount of tokens.
--
-- You'll probably not construct this by hand, but use 'txSkelMintsFromList'.
type TxSkelMints =
  Map
    (Script.Versioned Script.MintingPolicy)
    (TxSkelRedeemer, NEMap Api.TokenName (NonZero Integer))

-- | Combining 'TxSkelMints' in a sensible way. In particular, this means that
--
-- > Map.fromList [(pol, (red, NEMap.fromList [(tName, 1)]))]
--
-- and
--
-- > Map.fromList [(pol, (red', NEMap.fromList [(tName, -1)]))]
--
-- will combine to become the empty 'TxSkelMints' (and similar examples, where
-- the values add up to zero, see the comment at the definition of
-- 'addToTxSkelMints').
--
-- In every case, if you add mints with a different redeemer for the same
-- policy, the redeemer used in the right argument takes precedence.
instance {-# OVERLAPPING #-} Semigroup TxSkelMints where
  a <> b = foldl (flip addToTxSkelMints) a (txSkelMintsToList b)

instance {-# OVERLAPPING #-} Monoid TxSkelMints where
  mempty = Map.empty

-- | Add a new entry to a 'TxSkelMints'. There are a few wrinkles:
--
-- (1) If for a given policy, redeemer, and token name, there are @n@ tokens in
-- the argument 'TxSkelMints', and you add @-n@ tokens, the corresponding entry
-- in the "inner map" of the policy will disappear (obviously, because all of
-- its values have to be non-zero). If that also means that the inner map
-- becomes empty, the policy will disappear from the 'TxSkelMints' altogether.
--
-- (2) If a policy is already present on the argument 'TxSkelMints' with a
-- redeemer @a@, and you add a mint with a different redeemer @b@, the old
-- redeemer is thrown away. The values associated with the token names of that
-- policy are added as described above, though. This means that any pre-existing
-- values will be minted with a new redeemer.
--
-- If, for some reason, you really want to generate a 'TxSkelMints' that has
-- both a negative and a positive entry of the same asset class and redeemer,
-- you'll have to do so manually. Note, however, that even if you do so, NO
-- VALIDATOR OR MINTING POLICY WILL EVER GET TO SEE A TRANSACTION WITH SUCH
-- CONFLICTING INFORMATION. This is not a design decision/limitation of
-- cooked-validators: The Cardano API 'TxBodyContent' type, that we're
-- translating everything into eventually, stores minting information as a
-- minted value together with a map from policy IDs to witnesses (which
-- represent the used redeemers). That means that we can only store _one_
-- redeemer per minting policy, and no conflicting mints of the same asset
-- class, since they'll just cancel.
addToTxSkelMints ::
  (Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer) ->
  TxSkelMints ->
  TxSkelMints
addToTxSkelMints (pol, red, tName, amount) mints
  | 0 == amount = mints
  | otherwise = case mints Map.!? pol of
      Nothing ->
        -- The policy isn't yet in the given 'TxSkelMints', so we can just add a
        -- new entry:
        Map.insert pol (red, NEMap.singleton tName (NonZero amount)) mints
      Just (_oldRed, innerMap) ->
        -- Ignore the old redeemer: If it's the same as the new one, nothing
        -- will change, if not, the new redeemer will be kept.
        case innerMap NEMap.!? tName of
          Nothing ->
            -- The given token name has not yet occurred for the given
            -- policy. This means that we can just add the new tokens to the
            -- inner map:
            Map.insert pol (red, NEMap.insert tName (NonZero amount) innerMap) mints
          Just (NonZero oldAmount) ->
            let newAmount = oldAmount + amount
             in if newAmount /= 0
                  then -- If the sum of the old amount of tokens and the
                  -- additional tokens is non-zero, we can just update the
                  -- amount in the inner map:
                    Map.insert pol (red, NEMap.insert tName (NonZero newAmount) innerMap) mints
                  else -- If the sum is zero, we'll have to delete the token
                  -- name from the inner map. If that yields a completely empty
                  -- inner map, we'll have to remove the entry altogether:
                  case NEMap.nonEmptyMap $ NEMap.delete tName innerMap of
                    Nothing -> Map.delete pol mints
                    Just newInnerMap -> Map.insert pol (red, newInnerMap) mints

-- | Convert from 'TxSkelMints' to a list of tuples describing eveything that's
-- being minted.
txSkelMintsToList :: TxSkelMints -> [(Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer)]
txSkelMintsToList =
  concatMap
    ( \(p, (r, m)) ->
        (\(t, NonZero n) -> (p, r, t, n))
          <$> NEList.toList (NEMap.toList m)
    )
    . Map.toList

-- | Smart constructor for 'TxSkelMints'. This function relies on
-- 'addToTxSkelMints'. So, some non-empty lists (where all amounts for a given
-- asset class an redeemer add up to zero) might be translated into the empty
-- 'TxSkelMints'.
txSkelMintsFromList :: [(Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer)] -> TxSkelMints
txSkelMintsFromList = foldr addToTxSkelMints mempty

-- | Another smart constructor for 'TxSkelMints', where the redeemer and minting
-- policies are not duplicated.
txSkelMintsFromList' :: [(Script.Versioned Script.MintingPolicy, TxSkelRedeemer, [(Api.TokenName, Integer)])] -> TxSkelMints
txSkelMintsFromList' = txSkelMintsFromList . concatMap (\(mp, r, m) -> (\(tn, i) -> (mp, r, tn, i)) <$> m)

-- | The value described by a 'TxSkelMints'
txSkelMintsValue :: TxSkelMints -> Api.Value
txSkelMintsValue =
  foldMapOf
    (to txSkelMintsToList % folded)
    ( \(policy, _, tName, amount) ->
        Script.assetClassValue
          ( Script.assetClass
              (Script.scriptCurrencySymbol policy)
              tName
          )
          amount
    )
