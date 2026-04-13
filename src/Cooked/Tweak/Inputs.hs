-- | This module provides some 'Tweak's that add or remove inputs and outputs
-- from transactions. Some also operate on the minted value.
module Cooked.Tweak.Inputs
  ( ensureInputTweak,
    addInputTweak,
    removeInputTweak,
    modifySpendRedeemersOfTypeTweak,
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Map qualified as Map
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.NonDet

-- | Ensure that a given 'Api.TxOutRef' is being spent with a given
-- 'TxSkelRedeemer'. The return value will be @Just@ the added data, if anything
-- changed.
ensureInputTweak ::
  (Member Tweak effs) =>
  Api.TxOutRef ->
  TxSkelRedeemer ->
  Sem effs (Maybe (Api.TxOutRef, TxSkelRedeemer))
ensureInputTweak oref howConsumed = do
  presentInputs <- viewTweak txSkelInsL
  if presentInputs Map.!? oref == Just howConsumed
    then return Nothing
    else do
      overTweak txSkelInsL (Map.insert oref howConsumed)
      return $ Just (oref, howConsumed)

-- | Add an input to a transaction. If the given 'Api.TxOutRef' is already being
-- consumed by the transaction, fail.
addInputTweak ::
  (Members '[Tweak, NonDet] effs) =>
  Api.TxOutRef ->
  TxSkelRedeemer ->
  Sem effs ()
addInputTweak oref howConsumed = do
  presentInputs <- viewTweak txSkelInsL
  guard (Map.notMember oref presentInputs)
  overTweak txSkelInsL (Map.insert oref howConsumed)

-- | Remove transaction inputs according to a given predicate. The returned list
-- contains all removed inputs.
removeInputTweak ::
  (Member Tweak effs) =>
  (Api.TxOutRef -> TxSkelRedeemer -> Bool) ->
  Sem effs [(Api.TxOutRef, TxSkelRedeemer)]
removeInputTweak removePred = do
  presentInputs <- viewTweak txSkelInsL
  let (removed, kept) = Map.partitionWithKey removePred presentInputs
  setTweak txSkelInsL kept
  return $ Map.toList removed

-- | Applies an optional modification to all spend redeemers of type a. Returns
-- the list of modified spending redemeers, as they were before being modified.
modifySpendRedeemersOfTypeTweak ::
  forall a b effs.
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Member Tweak effs
  ) =>
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifySpendRedeemersOfTypeTweak f =
  overMaybeTweak (txSkelInsL % iso Map.toList Map.fromList % traversed % _2) $ \red -> do
    typedRedeemer <- red ^? txSkelRedeemerTypedAT
    typedRedeemerModified <- f typedRedeemer
    return $ red & txSkelRedeemerTypedAT @a .~ typedRedeemerModified
