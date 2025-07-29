{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the minting constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities.
module Cooked.Skeleton.Mint
  ( TxSkelMints,
    Mint (..),
    mintRedeemedScriptL,
    mintTokensL,
    mint,
    burn,
    txSkelMintsListI,
    txSkelMintsAssetClassAmountL,
    mintCurrencySymbolG,
    txSkelMintsAssetClassesG,
  )
where

import Cooked.Skeleton.Redeemer as X
import Cooked.Skeleton.Scripts
import Data.Bifunctor
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusTx.AssocMap qualified as PMap
import Test.QuickCheck (NonZero (..))

-- * Describing single mint entries

-- | A description of a new entry to be added in a 'TxSkelMints'. The users
-- should be using lists of those (using 'txSkelMintsFromList') instead of
-- building a 'TxSkelMints' directly.
data Mint where
  Mint ::
    { mintRedeemedScript :: User IsScript Redemption,
      mintTokens :: [(Api.TokenName, Integer)]
    } ->
    Mint

-- * Extra builders for single mint entries

-- | Builds some 'Mint' when a single type of token is minted for a given MP
mint :: (ToVScript a) => a -> TxSkelRedeemer -> Api.TokenName -> Integer -> Mint
mint mp red tn n = Mint (UserRedeemedScript mp red) [(tn, n)]

-- | Similar to 'mint' but deducing the tokens instead
burn :: (ToVScript a) => a -> TxSkelRedeemer -> Api.TokenName -> Integer -> Mint
burn mp red tn n = mint mp red tn (-n)

-- * Optics to manipulate elements of 'Mint'

-- | A lens to set or get the redeemer of a 'Mint'
makeLensesFor [("mintRedeemedScript", "mintRedeemedScriptL")] ''Mint

-- | A lens to set or get the token list of a 'Mint'
makeLensesFor [("mintTokens", "mintTokensL")] ''Mint

-- | Returns the currency symbol associated with a `Mint`
mintCurrencySymbolG :: Getter Mint Api.CurrencySymbol
mintCurrencySymbolG =
  mintRedeemedScriptL
    % userVScriptL
    % to
      ( Script.toCurrencySymbol
          . Script.toScriptHash
          . toVScript
      )

-- * Describing full minted values with associated redeemers

-- | A description of what a transaction mints. For every policy, there can only
-- be one 'TxSkelRedeemer', and if there is, there must be some token names, each
-- with a non-zero amount of tokens.
--
-- You'll probably not construct this by hand, but use 'txSkelMintsFromList'.
type TxSkelMints = Map VScript (TxSkelRedeemer, NEMap Api.TokenName (NonZero Integer))

-- * Optics to manipulate components of 'TxSkelMints' bind it to 'Mint'

-- | Sets or gets the amount of tokens minted for a certain asset class,
-- represented by a token name and a versioned minting policy. This removes the
-- appropriate entries (the token entry, and possible the mp entry if it would
-- leave it empty) when setting the amount to 0. This function is very similar
-- to 'Cooked.Skeleton.Value.valueAssetClassAmountL' but it also involves the
-- 'TxSkelRedeemer' associated with the minting policy.
--
-- This Lens is quite involved and is the main way to build 'TxSkelMints'
-- iteratively from a list of 'Mint' (see 'txSkelMintsListI'). If you're looking
-- for simpler optics working in a 'TxSkelMints', consider using @ix mp % _1@
-- for instance to modify an existing redeemer, or @ix mp % _2 % ix tk@ to
-- modify a token amount. Another option is to use the optics working on 'Mint'
-- and combining them with 'txSkelMintsListI'.
txSkelMintsAssetClassAmountL :: (ToVScript mp) => mp -> Api.TokenName -> Lens' TxSkelMints (Maybe TxSkelRedeemer, Integer)
txSkelMintsAssetClassAmountL (toVScript -> mp) tk =
  lens
    -- We return (Nothing, 0) when the mp is not in the map, (Just red, 0) when
    -- the mp is present but not the token, and (Just red, n) otherwise.
    (maybe (Nothing, 0) (bimap Just (maybe 0 getNonZero . NEMap.lookup tk)) . Map.lookup mp)
    ( \mints (newRed, i) -> case Map.lookup mp mints of
        -- No previous mp entry and nothing to add
        Nothing | i == 0 -> mints
        -- No previous mp entry and something to add but no redeemer to attach
        Nothing | Nothing <- newRed -> mints
        -- No previous mp entry, something to add and a redeemer to attach
        Nothing | Just newRed' <- newRed -> Map.insert mp (newRed', NEMap.singleton tk (NonZero i)) mints
        -- A previous mp and tk entry, which needs to be removed and the whole
        -- mp entry as well because it only containes this tk.
        Just (NEMap.nonEmptyMap . NEMap.delete tk . snd -> Nothing) | i == 0 -> Map.delete mp mints
        -- A prevous mp and tk entry, which needs to be removed, but the whole
        -- mp entry has other tokens and thus is kept with the new redeemer if
        -- it exists. If it does not, the previous one is kept.
        Just (prevRed, NEMap.nonEmptyMap . NEMap.delete tk -> Just tokenMap) | i == 0 -> Map.insert mp (fromMaybe prevRed newRed, tokenMap) mints
        -- A previous mp entry, in which we insert the new tk, regardless if
        -- it's already there or not, with the new redeemer.
        Just (prevRed, tokenMap) -> Map.insert mp (fromMaybe prevRed newRed, NEMap.insert tk (NonZero i) tokenMap) mints
    )

instance Script.ToValue TxSkelMints where
  toValue =
    Api.Value
      . PMap.unsafeFromList
      . fmap
        ( bimap
            (Script.toCurrencySymbol . Script.toScriptHash)
            ( PMap.unsafeFromList
                . fmap (second getNonZero)
                . NEList.toList
                . NEMap.toList
                . snd
            )
        )
      . Map.toList

-- | The list of assets classes contained in this 'TxSkelMints'
txSkelMintsAssetClassesG :: Getter TxSkelMints [(VScript, Api.TokenName)]
txSkelMintsAssetClassesG = txSkelMintsListI % to (\l -> [(toVScript mp, tk) | Mint (UserRedeemedScript mp _) tks <- l, (tk, _) <- tks])

-- | Seeing a 'TxSkelMints' as a list of 'Mint'
txSkelMintsListI :: Iso' TxSkelMints [Mint]
txSkelMintsListI =
  iso
    (map (\(p, (r, m)) -> Mint (UserRedeemedScript p r) $ second getNonZero <$> NEList.toList (NEMap.toList m)) . Map.toList)
    ( foldl
        ( \mints (Mint (UserRedeemedScript mp red) tks) ->
            foldl
              (\mints' (tk, n) -> mints' & txSkelMintsAssetClassAmountL mp tk %~ (\(_, n') -> (Just red, n + n')))
              mints
              tks
        )
        Map.empty
    )

-- * Additional instances an useful helpers

-- | Combining 'TxSkelMints' in a sensible way. In particular, this means that
--
-- > Map.fromList [(pol, (red, NEMap.fromList [(tName, 1)]))]
--
-- and
--
-- > Map.fromList [(pol, (red', NEMap.fromList [(tName, -1)]))]
--
-- will combine to become the empty 'TxSkelMints'
--
-- In every case, if you add mints with a different redeemer for the same
-- policy, the redeemer used in the right argument takes precedence.
instance {-# OVERLAPPING #-} Semigroup TxSkelMints where
  a <> b = review txSkelMintsListI $ view txSkelMintsListI a ++ view txSkelMintsListI b

instance {-# OVERLAPPING #-} Monoid TxSkelMints where
  mempty = Map.empty
