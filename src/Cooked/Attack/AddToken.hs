-- | This module provides two automated attacks to mint and give extra tokens to
-- a certain target.
module Cooked.Attack.AddToken
  ( -- * Add token params
    AddTokenParams (..),
    fromMintsAddTokenParams,
    fromCurrencyAddTokenParams,
    fromAssetClassAddTokenParams,

    -- * Add token label
    AddTokenLabel (..),

    -- * Add token attack
    addTokenAttack,
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Cooked.Tweak.Mint
import Cooked.Tweak.Outputs
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import Polysemy
import Polysemy.NonDet

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- 'addTokenAttack'
newtype AddTokenLabel = AddTokenLabel Api.Value
  deriving (Show, Eq, Ord)

instance PrettyCooked AddTokenLabel where
  prettyCookedOpt ops (AddTokenLabel val) =
    "Added value: " <> prettyCookedOpt ops val

-- | Parameters of the add token attack
data AddTokenParams owner effs
  = AddTokenParams
  { -- | The new mints to add in the transaction. These are effectful because
    -- they can depend on the existing mints.
    atpNewMints :: Sem effs [Mint],
    -- | The target of the added tokens.
    atpThief :: owner
  }

-- | Add tokens based on a list of 'Mint'.
fromMintsAddTokenParams ::
  -- | The 'Mint's to add.
  [Mint] ->
  -- | The attacker, who receives the extra tokens.
  owner ->
  AddTokenParams owner effs
fromMintsAddTokenParams mints =
  AddTokenParams (return mints)

-- | Add tokens based on a function applied to existing currencies (cannot add
-- new currencies, but can add new types of tokens).
fromCurrencyAddTokenParams ::
  (Member Tweak effs) =>
  -- | For each policy that occurs in some 'Mint' constraint, return a list of
  -- token names together with how many tokens with that name should be minted,
  -- in addition to the existing tokens.
  (VScript -> [(Api.TokenName, Integer)]) ->
  -- | The attacker, who receives the extra tokens.
  owner ->
  AddTokenParams owner effs
fromCurrencyAddTokenParams newTokens = AddTokenParams $ do
  currencies <- viewAllTweak (txSkelMintsL % txSkelMintsListI % traversed % mintRedeemedScriptL)
  return $
    foldl
      ( \newMints rScript@(UserRedeemedScript (toVScript -> script) _) ->
          Mint rScript (newTokens script) : newMints
      )
      []
      currencies

-- | Add tokens based on a function applied to both existing currencies and
-- token (cannot add new currencies nor new types of tokens).
fromAssetClassAddTokenParams ::
  (Member Tweak effs) =>
  -- | A function returning the new amount of tokens to mint given a specific
  -- currency, token name and amount. This new amount replaces the old one.
  (VScript -> Api.TokenName -> Integer -> Integer) ->
  -- | The attacker, who receives the extra tokens.
  owner ->
  AddTokenParams owner effs
fromAssetClassAddTokenParams newTokens = AddTokenParams $ do
  mints <- viewTweak (txSkelMintsL % txSkelMintsListI)
  return $
    foldl
      ( \newMints (Mint rScript@(UserRedeemedScript (toVScript -> script) _) tks) ->
          Mint rScript (foldl (\newTks (tk, i) -> (tk, newTokens script tk i - i) : newTks) [] tks) : newMints
      )
      []
      mints

-- | This attack adds extra tokens of any kind in the minted value. The
-- additional minted value is redirected to a certain owner in a dedicated
-- output.
addTokenAttack ::
  ( Members '[Tweak, NonDet] effs,
    IsTxSkelOutAllowedOwner owner
  ) =>
  AddTokenParams owner effs ->
  Sem effs Api.Value
addTokenAttack AddTokenParams {..} = do
  newMints <- atpNewMints
  let totalIncrement = Script.toValue $ review txSkelMintsListI newMints
  guard (totalIncrement /= mempty)
  addMintsTweak newMints
  addOutputTweak $ atpThief `receives` Value totalIncrement
  addLabelTweak $ AddTokenLabel totalIncrement
  return totalIncrement
