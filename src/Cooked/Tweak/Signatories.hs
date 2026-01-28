-- | This module defines 'Cooked.Tweak.Common.Tweak's revolving around the
-- signatories of a transaction. They assume but do not ensure that the list of
-- signatories is free of duplicates.
module Cooked.Tweak.Signatories
  ( getSignatoriesTweak,
    modifySignatoriesTweak,
    setSignatoriesTweak,
    signatoriesSatisfyTweak,
    isSignatoryTweak,
    hasSignatoriesTweak,
    addFirstSignatoryTweak,
    addSignatoriesTweak,
    addLastSignatoryTweak,
    removeSignatoriesTweak,
    removeSignatoryTweak,
    replaceFirstSignatoryTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List (delete, (\\))
import Polysemy

-- | Returns the current list of signatories
getSignatoriesTweak ::
  (Member Tweak effs) =>
  Sem effs [TxSkelSignatory]
getSignatoriesTweak = viewTweak txSkelSignatoriesL

-- | Apply a function to the list of signatories and return the old ones
modifySignatoriesTweak ::
  (Member Tweak effs) =>
  ([TxSkelSignatory] -> [TxSkelSignatory]) ->
  Sem effs [TxSkelSignatory]
modifySignatoriesTweak f = do
  oldSignatories <- getSignatoriesTweak
  setTweak txSkelSignatoriesL (f oldSignatories)
  return oldSignatories

-- | Change the current signatories and return the old ones
setSignatoriesTweak ::
  (Member Tweak effs) =>
  [TxSkelSignatory] ->
  Sem effs [TxSkelSignatory]
setSignatoriesTweak = modifySignatoriesTweak . const

-- | Check if the signatories satisfy a certain predicate
signatoriesSatisfyTweak ::
  (Member Tweak effs) =>
  ([TxSkelSignatory] -> Bool) ->
  Sem effs Bool
signatoriesSatisfyTweak = (<$> getSignatoriesTweak)

-- | Check if a signatory signs a transaction
isSignatoryTweak ::
  (Member Tweak effs) =>
  TxSkelSignatory ->
  Sem effs Bool
isSignatoryTweak = signatoriesSatisfyTweak . elem

-- | Check if the transaction has at least a signatory
hasSignatoriesTweak ::
  (Member Tweak effs) =>
  Sem effs Bool
hasSignatoriesTweak = signatoriesSatisfyTweak (not . null)

-- | Add a signatory to the transaction, at the head of the list of signatories, and
-- return the old list of signatories
addFirstSignatoryTweak ::
  (Member Tweak effs) =>
  TxSkelSignatory ->
  Sem effs [TxSkelSignatory]
addFirstSignatoryTweak = modifySignatoriesTweak . (:)

-- | Add signatories at the end of the list of signatories, and return the old list of
-- signatories
addSignatoriesTweak ::
  (Member Tweak effs) =>
  [TxSkelSignatory] ->
  Sem effs [TxSkelSignatory]
addSignatoriesTweak = modifySignatoriesTweak . (<>)

-- | Add a signatory to the transaction, at the end of the list of signatories, and
-- return the old list of signatories
addLastSignatoryTweak ::
  (Member Tweak effs) =>
  TxSkelSignatory ->
  Sem effs [TxSkelSignatory]
addLastSignatoryTweak = addSignatoriesTweak . (: [])

-- | Remove signatories from the transaction and return the old list of signatories
removeSignatoriesTweak ::
  (Member Tweak effs) =>
  [TxSkelSignatory] ->
  Sem effs [TxSkelSignatory]
removeSignatoriesTweak = modifySignatoriesTweak . (\\)

-- | Remove a signatory from the transaction and return the old list of signatories
removeSignatoryTweak ::
  (Member Tweak effs) =>
  TxSkelSignatory ->
  Sem effs [TxSkelSignatory]
removeSignatoryTweak = modifySignatoriesTweak . delete

-- | Changes the first signatory (adds it if there are no signatories) and return the
-- old list of signatories.
replaceFirstSignatoryTweak ::
  (Member Tweak effs) =>
  TxSkelSignatory ->
  Sem effs [TxSkelSignatory]
replaceFirstSignatoryTweak =
  modifySignatoriesTweak
    . ( \newSignatory -> \case
          [] -> [newSignatory]
          (_ : ss) -> newSignatory : ss
      )
