{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the minting constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities. To mint or burn
-- tokens in a skeleton, the usual way is to invoke @txSkelMints =
-- txSkelMintsFromList [mint script redeemer token quantity, burn ...]@
module Cooked.Skeleton.Mint
  ( -- * Data types
    Mint (..),
    TxSkelMints (unTxSkelMints),

    -- * Optics
    mintRedeemedScriptL,
    mintTokensL,
    mintCurrencySymbolG,
    txSkelMintsListI,
    txSkelMintsAssetClassAmountL,
    txSkelMintsAssetClassesG,
    txSkelMintsPolicyTokensL,

    -- * Smart constructors
    mint,
    burn,
    txSkelMintsFromList,
  )
where

import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.User
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.String (IsString (fromString))
import Data.Typeable
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as PMap

-- * Describing single mint entries

-- | A description of a new entry to be added in a 'TxSkelMints'. The users
-- should be using lists of those (using @txSkelMintsFromList@) instead of
-- building a 'TxSkelMints' directly.
data Mint where
  Mint ::
    { mintRedeemedScript :: User IsScript Redemption,
      mintTokens :: [(Api.TokenName, Integer)]
    } ->
    Mint

-- * Extra builders for single mint entries

-- | Conveniency instance to be able to use Strings as 'Api.TokenName', which
-- used to be present in plutus-ledger-api.
instance IsString Api.TokenName where
  fromString = Api.TokenName . fromString

-- | Builds some 'Mint' when a single type of token is minted for a given MP
mint :: (ToVScript script, Typeable script, RedeemerConstrs red) => script -> red -> Api.TokenName -> Integer -> Mint
mint mp red tn n = Mint (UserRedeemedScript mp (someTxSkelRedeemer red)) [(tn, n)]

-- | Similar to 'mint' but deducing the tokens instead
burn :: (ToVScript script, Typeable script, RedeemerConstrs red) => script -> red -> Api.TokenName -> Integer -> Mint
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
-- with a non-zero amount of tokens. This invariant is guaranteed because the
-- raw constructor is not exposed, and functions working around it preserve it.
-- To build a 'TxSkelMints', use 'txSkelMintsFromList'.
newtype TxSkelMints = TxSkelMints
  { unTxSkelMints ::
      Map
        Api.ScriptHash
        (User 'IsScript 'Redemption, Map Api.TokenName Integer)
  }
  deriving (Show, Eq)

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
txSkelMintsAssetClassAmountL :: (ToVScript mp, Typeable mp) => mp -> Api.TokenName -> Lens' TxSkelMints (Maybe TxSkelRedeemer, Integer)
txSkelMintsAssetClassAmountL mp@(Script.toScriptHash . toVScript -> mph) tk =
  lens
    -- We return (Nothing, 0) when the mp is not in the map, (Just red, 0) when
    -- the mp is present but not the token, and (Just red, n) otherwise.
    (maybe (Nothing, 0) (bimap (Just . view userTxSkelRedeemerL) (fromMaybe 0 . Map.lookup tk)) . Map.lookup mph . unTxSkelMints)
    ( \(TxSkelMints mints) (newRed, i) -> TxSkelMints $ case Map.lookup mph mints of
        -- No previous mp entry and nothing to add
        Nothing | i == 0 -> mints
        -- No previous mp entry and something to add but no redeemer to attach
        Nothing | Nothing <- newRed -> mints
        -- No previous mp entry, something to add and a redeemer to attach
        Nothing | Just newRed' <- newRed -> Map.insert mph (UserRedeemedScript mp newRed', Map.singleton tk i) mints
        -- A previous mp and tk entry, which needs to be removed and the whole
        -- mp entry as well because it only contains this tk.
        Just (Map.delete tk . snd -> subMap) | subMap == mempty, i == 0 -> Map.delete mph mints
        -- A prevous mp and tk entry, which either needs to be removed in case
        -- of i == 0, or updated otherwise.
        Just (prevUser, if i == 0 then Map.delete tk else Map.insert tk i -> subMap)
          | newUser <- maybe prevUser (flip (set userTxSkelRedeemerL) prevUser) newRed -> Map.insert mph (newUser, subMap) mints
    )

-- | Focuses on the submap for a given minting policy, following the same rules
-- as 'txSkelMintsAssetClassAmountL' when setting a new submap.
txSkelMintsPolicyTokensL :: (ToVScript mp, Typeable mp) => mp -> Lens' TxSkelMints (Maybe (TxSkelRedeemer, Map Api.TokenName Integer))
txSkelMintsPolicyTokensL mp@(Script.toScriptHash . toVScript -> mph) =
  lens
    (fmap (first (view userTxSkelRedeemerL)) . view (to unTxSkelMints % at mph))
    ( \mints -> \case
        Nothing -> TxSkelMints . Map.delete mph . unTxSkelMints $ mints
        Just (red, Map.toList -> tokens) -> foldl (flip $ \(tk, n) -> set (txSkelMintsAssetClassAmountL mp tk) (Just red, n)) mints tokens
    )

instance Script.ToValue TxSkelMints where
  toValue =
    Api.Value
      . PMap.unsafeFromList
      . fmap
        ( bimap
            Script.toCurrencySymbol
            (PMap.unsafeFromList . Map.toList . snd)
        )
      . Map.toList
      . unTxSkelMints

-- | The list of assets classes contained in this 'TxSkelMints'
txSkelMintsAssetClassesG :: Getter TxSkelMints [(VScript, Api.TokenName)]
txSkelMintsAssetClassesG = txSkelMintsListI % to (\l -> [(toVScript mp, tk) | Mint (UserRedeemedScript mp _) tks <- l, (tk, _) <- tks])

-- | Seeing a 'TxSkelMints' as a list of 'Mint'
txSkelMintsListI :: Iso' TxSkelMints [Mint]
txSkelMintsListI =
  iso
    (map (\(user, m) -> Mint user (Map.toList m)) . Map.elems . unTxSkelMints)
    ( foldl
        ( \mints (Mint (UserRedeemedScript mp red) tks) ->
            foldl
              (\mints' (tk, n) -> mints' & txSkelMintsAssetClassAmountL mp tk %~ (\(_, n') -> (Just red, n + n')))
              mints
              tks
        )
        mempty
    )

-- | Builds a 'TxSkelMints' from a list of 'Mint'. This is equivalent to calling
-- @review txSkelMintsListI@
txSkelMintsFromList :: [Mint] -> TxSkelMints
txSkelMintsFromList = review txSkelMintsListI

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
instance Semigroup TxSkelMints where
  txSkelM <> txSkelM' =
    review txSkelMintsListI $
      view txSkelMintsListI txSkelM
        <> view txSkelMintsListI txSkelM'

instance Monoid TxSkelMints where
  mempty = TxSkelMints Map.empty
