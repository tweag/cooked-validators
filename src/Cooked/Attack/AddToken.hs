-- | This module provides two automated attacks to mint and give extra tokens to
-- a certain target.
module Cooked.Attack.AddToken
  ( addTokenAttack,
    dupTokenAttack,
    AddTokenLabel (..),
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Cooked.Tweak.Modification
import Cooked.Tweak.Outputs
import Data.Map qualified as Map
import Optics.Core
import PlutusLedgerApi.V1.Value qualified as Api
import Polysemy
import Polysemy.NonDet

-- | This attack adds extra tokens of any kind for minting policies already
-- present in the minted value. The additional minted value is redirected to a
-- certain owner in a dedicated output.
--
-- This attack adds an 'AddTokenLbl' label.
addTokenAttack ::
  ( Members '[Tweak, NonDet] effs,
    IsTxSkelOutAllowedOwner o
  ) =>
  -- | For each policy that occurs in some 'Mint' constraint, return a list of
  -- token names together with how many tokens with that name should be minted.
  (VScript -> [(Api.TokenName, Integer)]) ->
  -- | The attacker, who receives the extra tokens.
  o ->
  Sem effs Api.Value
addTokenAttack extraTokens attacker = do
  res <-
    modifyTweak
      (: [])
      (txSkelMintsL % txSkelMintsMapL % itraversed)
      ( \_ (rScript@(UserRedeemedScript (toVScript -> script) _), subMap) -> do
          let (surplus, newSubMap) =
                foldl
                  ( \(value, sMap) (tn, i) ->
                      ( over (valueAssetClassAmountL script tn) (+ i) value,
                        over (at tn) (maybe (Just i) (Just . (+ i))) sMap
                      )
                  )
                  (mempty, subMap)
                  (extraTokens script)
          return ((rScript, newSubMap), surplus)
      )
  let surplus = mconcat res
  guard $ surplus `Api.geq` mempty
  addOutputTweak $ attacker `receives` Value surplus
  addLabelTweak $ AddTokenLabel surplus
  return surplus

-- | This attack is similar to 'addTokenAttack' with the exception that it only
-- tampers with token names already present.
--
-- This attack adds an 'AddTokenLabel' label
dupTokenAttack ::
  ( Members '[Tweak, NonDet] effs,
    IsTxSkelOutAllowedOwner o
  ) =>
  -- | A function describing how the amount of tokens specified by a 'Mint'
  -- constraint should be changed, depending on the asset class and the amount
  -- specified by the constraint. The given function @f@ should probably satisfy
  -- @f ac i > i@ for all @ac@ and @i@, i.e. it should increase the minted
  -- amount. If it does not, the tweak will still succeed but this might result
  -- in negative portions in the value paid to the attacker.
  (VScript -> Api.TokenName -> Integer -> Integer) ->
  -- | The target of the extra tokens. Any additional tokens that are minted by
  -- the modified transaction but were not minted by the original transaction
  -- are paid to this target.
  o ->
  Sem effs Api.Value
dupTokenAttack change attacker = do
  mints <- viewTweak txSkelMintsL
  addTokenAttack
    ( \s ->
        maybe
          []
          (\(_, subMap) -> [(tk, change s tk n - n) | (tk, n) <- Map.toList subMap])
          (view (txSkelMintsPolicyTokensL s) mints)
    )
    attacker

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- 'addTokenAttack'
newtype AddTokenLabel = AddTokenLabel Api.Value deriving (Show, Eq, Ord)

instance PrettyCooked AddTokenLabel where
  prettyCookedOpt ops val = "Added value: " <> prettyCookedOpt ops val
