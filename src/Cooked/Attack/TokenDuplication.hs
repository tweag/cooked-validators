-- | This module provides an automated attack to add minted tokens in a
-- 'TxSkel'. In principle, a token duplication attack consist in minting a
-- higher amount of tokens already minted in the transaction, but we generalise
-- it to also add arbitrary tokens if needed.
module Cooked.Attack.TokenDuplication
  ( -- * Token duplication params
    TokenDuplicationParams (..),
    anyMintTokenDuplicationParams,
    existingCurrencyTokenDuplicationParams,
    existingAssetClassTokenDuplicationParams,

    -- * Token duplication label
    TokenDuplicationLabel (..),

    -- * Token duplication attack
    tokenDuplicationAttack,
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
-- 'tokenDuplicationAttack'
newtype TokenDuplicationLabel = TokenDuplicationLabel Api.Value
  deriving (Show, Eq, Ord)

instance PrettyCooked TokenDuplicationLabel where
  prettyCookedOpt ops (TokenDuplicationLabel val) =
    "Added value: " <> prettyCookedOpt ops val

-- | Parameters of the add token attack
data TokenDuplicationParams owner effs
  = TokenDuplicationParams
  { -- | The new mints to add in the transaction. These are effectful because
    -- they can depend on the existing mints.
    atpNewMints :: Sem effs [Mint],
    -- | The target of the added tokens.
    atpThief :: owner
  }

-- | Token duplications based on a list of 'Mint'.
anyMintTokenDuplicationParams ::
  -- | The 'Mint's to add.
  [Mint] ->
  -- | The attacker, who receives the extra tokens.
  owner ->
  TokenDuplicationParams owner effs
anyMintTokenDuplicationParams mints =
  TokenDuplicationParams (return mints)

-- | Token duplications based on a function applied to existing currencies (cannot add
-- new currencies, but can add new types of tokens).
existingCurrencyTokenDuplicationParams ::
  (Member Tweak effs) =>
  -- | For each policy that occurs in some 'Mint' constraint, return a list of
  -- token names together with how many tokens with that name should be minted,
  -- in addition to the existing tokens.
  (VScript -> [(Api.TokenName, Integer)]) ->
  -- | The attacker, who receives the extra tokens.
  owner ->
  TokenDuplicationParams owner effs
existingCurrencyTokenDuplicationParams newTokens = TokenDuplicationParams $ do
  currencies <- viewAllTweak (txSkelMintsL % txSkelMintsListI % traversed % mintRedeemedScriptL)
  return $
    foldl
      ( \newMints rScript@(UserRedeemedScript (toVScript -> script) _) ->
          Mint rScript (newTokens script) : newMints
      )
      []
      currencies

-- | Token duplications based on a function applied to both existing currencies and
-- token (cannot add new currencies nor new types of tokens).
existingAssetClassTokenDuplicationParams ::
  (Member Tweak effs) =>
  -- | A function returning the new amount of tokens to mint given a specific
  -- currency, token name and amount. This new amount replaces the old one.
  (VScript -> Api.TokenName -> Integer -> Integer) ->
  -- | The attacker, who receives the extra tokens.
  owner ->
  TokenDuplicationParams owner effs
existingAssetClassTokenDuplicationParams newTokens = TokenDuplicationParams $ do
  mints <- viewTweak (txSkelMintsL % txSkelMintsListI)
  return $
    foldl
      ( \newMints (Mint rScript@(UserRedeemedScript (toVScript -> script) _) tks) ->
          Mint rScript (foldl (\newTks (tk, i) -> (tk, newTokens script tk i - i) : newTks) [] tks) : newMints
      )
      []
      mints

-- | This attack adds extra tokens of any kind in the minted value. The
-- additional minted value is redirected to the attacker.
tokenDuplicationAttack ::
  ( Members '[Tweak, NonDet] effs,
    IsTxSkelOutAllowedOwner owner
  ) =>
  -- | The parameters of the attack
  TokenDuplicationParams owner effs ->
  Sem effs Api.Value
tokenDuplicationAttack TokenDuplicationParams {..} = do
  -- We compute the additional minting to add.
  newMints <- atpNewMints
  -- We compute the total value added this way.
  let totalIncrement = Script.toValue $ review txSkelMintsListI newMints
  -- We ensure the total value is positive
  guard (totalIncrement `Api.gt` mempty)
  -- We add the new mints into the 'TxSkel'
  addMintsTweak newMints
  -- We redirect the extra value to an attacker
  addOutputTweak $ atpThief `receives` Value totalIncrement
  -- We label the transaction by the added tokens
  addLabelTweak $ TokenDuplicationLabel totalIncrement
  -- We return the added tokens
  return totalIncrement
