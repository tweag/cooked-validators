{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the minting constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities.
module Cooked.Skeleton.Mint
  ( TxSkelMints,
    Mint (..),
    mintRedeemerL,
    mintTokensL,
    mint,
    burn,
    addMint,
    addMints,
    txSkelMintsToList,
    txSkelMintsFromList,
    txSkelMintsValue,
    txSkelMintsListI,
    mintVersionedScriptL,
  )
where

import Cooked.Skeleton.Redeemer as X
import Data.Bifunctor
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusTx.AssocMap qualified as PMap
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

-- | A description of a new entry to be added in a 'TxSkelMints'. The users
-- should be using lists of those (using 'txSkelMintsFromList') instead of
-- building a 'TxSkelMints' directly.
data Mint where
  Mint ::
    (Script.ToVersioned Script.MintingPolicy a) =>
    { mintMintingPolicy :: a,
      mintRedeemer :: TxSkelRedeemer,
      mintTokens :: [(Api.TokenName, Integer)]
    } ->
    Mint

-- | A lens to set or get the redeemer of a 'Mint'
makeLensesFor [("mintRedeemer", "mintRedeemerL")] ''Mint

-- | A lens to set or get the token list of a 'Mint'
makeLensesFor [("mintTokens", "mintTokensL")] ''Mint

-- | A lens to set or get the versioned script of a 'Mint'
mintVersionedScriptL :: Lens' Mint (Script.Versioned Script.Script)
mintVersionedScriptL =
  lens
    (\(Mint mp _ _) -> Script.toScript <$> Script.toVersioned @Script.MintingPolicy mp)
    (\m mp -> m {mintMintingPolicy = mp})

-- | Additional helper to build some 'Mint' in the usual minting case where a
-- single type of token is minted for a given MP
mint :: (Script.ToVersioned Script.MintingPolicy a) => a -> TxSkelRedeemer -> Api.TokenName -> Integer -> Mint
mint mp red tn n = Mint mp red [(tn, n)]

-- | Similar to 'mint' but deducing the tokens instead
burn :: (Script.ToVersioned Script.MintingPolicy a) => a -> TxSkelRedeemer -> Api.TokenName -> Integer -> Mint
burn mp red tn n = mint mp red tn (-n)

-- | For each pair (tokenName, amount) in the input list, either:
--
-- - adds this new entry in the map if tokenName was not already a key
--
-- - updates the existing number of tokens associated with tokenName by adding
-- amount. Since amount can be negative, this addition can result in lowering
-- the amount of tokens present in the map. If it reaches exactly 0, the entry
-- is removed. As a consequences, if all inputs happen to cancel the existing
-- number of tokens for each tokenName, this will remove all entries in the map,
-- which is why the return value is wrapped in 'Maybe'.
addTokens :: [(Api.TokenName, Integer)] -> NEMap Api.TokenName (NonZero Integer) -> Maybe (NEMap Api.TokenName (NonZero Integer))
addTokens [] neMap = Just neMap
addTokens ((_, n) : l) neMap | n == 0 = addTokens l neMap
addTokens ((tn, n) : l) neMap = case addTokens l neMap of
  Nothing -> Just $ NEMap.singleton tn (NonZero n)
  Just neMap' -> case NEMap.lookup tn neMap' of
    Nothing -> Just $ NEMap.insert tn (NonZero n) neMap'
    Just (NonZero n') | n - n' == 0 -> NEMap.nonEmptyMap $ NEMap.delete tn neMap'
    Just (NonZero n') -> Just $ NEMap.insert tn (NonZero (n + n')) neMap'

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
addMint :: TxSkelMints -> Mint -> TxSkelMints
addMint txSkelMints (Mint _ _ []) = txSkelMints
addMint txSkelMints (Mint (Script.toVersioned -> mp) red tks@((tn, NonZero -> n) : tkxs)) =
  case Map.lookup mp txSkelMints of
    Nothing -> case addTokens tkxs (NEMap.singleton tn n) of
      Nothing -> txSkelMints
      Just newSubmap -> Map.insert mp (red, newSubmap) txSkelMints
    Just (_, subMap) -> case addTokens tks subMap of
      Nothing -> Map.delete mp txSkelMints
      Just newSubmap -> Map.insert mp (red, newSubmap) txSkelMints

-- | Adds a list of 'Mint' to a 'TxSkelMints', by iterating over the list
-- and using 'addMint'
addMints :: TxSkelMints -> [Mint] -> TxSkelMints
addMints = foldl addMint

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
-- 'addMint').
--
-- In every case, if you add mints with a different redeemer for the same
-- policy, the redeemer used in the right argument takes precedence.
instance {-# OVERLAPPING #-} Semigroup TxSkelMints where
  a <> b = addMints a (txSkelMintsToList b)

instance {-# OVERLAPPING #-} Monoid TxSkelMints where
  mempty = Map.empty

-- | Seing a 'TxSkelMints' as a list of 'Mint'
txSkelMintsListI :: Iso' TxSkelMints [Mint]
txSkelMintsListI =
  iso
    (map (\(p, (r, m)) -> Mint p r $ second getNonZero <$> NEList.toList (NEMap.toList m)) . Map.toList)
    (addMints mempty)

-- | Convert from 'TxSkelMints' to a list of 'Mint'
txSkelMintsToList :: TxSkelMints -> [Mint]
txSkelMintsToList = view txSkelMintsListI

-- | A smart constructor for 'TxSkelMints'
txSkelMintsFromList :: [Mint] -> TxSkelMints
txSkelMintsFromList = review txSkelMintsListI

-- | The value described by a 'TxSkelMints'
txSkelMintsValue :: TxSkelMints -> Api.Value
txSkelMintsValue txSkelMints =
  Api.Value $
    PMap.unsafeFromList $
      ( \(mp, (_, tks)) ->
          ( Script.toCurrencySymbol mp,
            PMap.unsafeFromList $
              (\(t, NonZero n) -> (t, n))
                <$> NEList.toList (NEMap.toList tks)
          )
      )
        <$> Map.toList txSkelMints
