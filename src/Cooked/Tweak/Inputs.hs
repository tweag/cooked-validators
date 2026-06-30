-- | This module provides some 'Tweak's that add or remove inputs and outputs
-- from transactions. Some also operate on the minted value.
module Cooked.Tweak.Inputs
  ( ensureInputTweak,
    addInputTweak,
    removeInputsTweak,
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Map qualified as Map
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
  presentInputs <- viewTweak txSkelInputsL
  if presentInputs Map.!? oref == Just howConsumed
    then return Nothing
    else do
      overTweak txSkelInputsL (Map.insert oref howConsumed)
      return $ Just (oref, howConsumed)

-- | Add an input to a transaction. If the given 'Api.TxOutRef' is already being
-- consumed by the transaction, fail.
addInputTweak ::
  (Members '[Tweak, NonDet] effs) =>
  Api.TxOutRef ->
  TxSkelRedeemer ->
  Sem effs ()
addInputTweak oref howConsumed = do
  presentInputs <- viewTweak txSkelInputsL
  guard (Map.notMember oref presentInputs)
  overTweak txSkelInputsL (Map.insert oref howConsumed)

-- | Remove transaction inputs according to a given predicate. The returned list
-- contains all removed inputs.
removeInputsTweak ::
  (Member Tweak effs) =>
  (Api.TxOutRef -> TxSkelRedeemer -> Bool) ->
  Sem effs [(Api.TxOutRef, TxSkelRedeemer)]
removeInputsTweak removePred = do
  presentInputs <- viewTweak txSkelInputsL
  let (removed, kept) = Map.partitionWithKey removePred presentInputs
  setTweak txSkelInputsL kept
  return $ Map.toList removed
