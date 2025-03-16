-- | This module provides some 'Tweak's that add or remove inputs and outputs
-- from transactions. Some also operate on the minted value.
module Cooked.Tweak.Inputs
  ( ensureInputTweak,
    addInputTweak,
    removeInputTweak,
    modifySpendRedeemersOfTypeTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Type.Reflection (Typeable)

-- | Ensure that a given 'Api.TxOutRef' is being spent with a given
-- 'TxSkelRedeemer'. The return value will be @Just@ the added data, if anything
-- changed.
ensureInputTweak :: (MonadTweak m) => Api.TxOutRef -> TxSkelRedeemer -> m (Maybe (Api.TxOutRef, TxSkelRedeemer))
ensureInputTweak oref howConsumed = do
  presentInputs <- viewTweak txSkelInsL
  if presentInputs Map.!? oref == Just howConsumed
    then return Nothing
    else do
      overTweak txSkelInsL (Map.insert oref howConsumed)
      return $ Just (oref, howConsumed)

-- | Add an input to a transaction. If the given 'Api.TxOutRef' is already being
-- consumed by the transaction, replaces the previous redeemer by the new one.
addInputTweak :: (MonadTweak m) => Api.TxOutRef -> TxSkelRedeemer -> m ()
addInputTweak oref = overTweak txSkelInsL . Map.insert oref

-- | Remove transaction inputs according to a given predicate. The returned list
-- contains all removed inputs.
removeInputTweak :: (MonadTweak m) => (Api.TxOutRef -> TxSkelRedeemer -> Bool) -> m [(Api.TxOutRef, TxSkelRedeemer)]
removeInputTweak removePred = do
  presentInputs <- viewTweak txSkelInsL
  let (removed, kept) = Map.partitionWithKey removePred presentInputs
  setTweak txSkelInsL kept
  return $ Map.toList removed

-- | Applies an optional modification to all spend redeemers of type a
modifySpendRedeemersOfTypeTweak :: forall a b m. (Typeable a, RedeemerConstrs b, MonadTweak m) => (a -> Maybe b) -> m ()
modifySpendRedeemersOfTypeTweak f = do
  presentInputs <- Map.toList <$> viewTweak txSkelInsL
  setTweak txSkelInsL $
    Map.fromList $
      presentInputs <&> \(oRef, TxSkelRedeemer red refInput) -> (oRef,) . fromMaybe (TxSkelRedeemer red refInput) $ do
        typedRedeemer <- toTypedRedeemer red
        typedRedeemerModified <- f typedRedeemer
        return $ TxSkelRedeemer (SomeRedeemer typedRedeemerModified) refInput
