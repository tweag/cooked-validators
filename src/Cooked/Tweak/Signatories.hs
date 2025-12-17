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

import Cooked.Skeleton (TxSkelSignatory, txSkelSignatoriesL)
import Cooked.Tweak.Common (MonadTweak, setTweak, viewTweak)
import Data.List (delete, (\\))

-- | Returns the current list of signatories
getSignatoriesTweak :: (MonadTweak m) => m [TxSkelSignatory]
getSignatoriesTweak = viewTweak txSkelSignatoriesL

-- | Apply a function to the list of signatories and return the old ones
modifySignatoriesTweak :: (MonadTweak m) => ([TxSkelSignatory] -> [TxSkelSignatory]) -> m [TxSkelSignatory]
modifySignatoriesTweak f = do
  oldSignatories <- getSignatoriesTweak
  setTweak txSkelSignatoriesL (f oldSignatories)
  return oldSignatories

-- | Change the current signatories and return the old ones
setSignatoriesTweak :: (MonadTweak m) => [TxSkelSignatory] -> m [TxSkelSignatory]
setSignatoriesTweak = modifySignatoriesTweak . const

-- | Check if the signatories satisfy a certain predicate
signatoriesSatisfyTweak :: (MonadTweak m) => ([TxSkelSignatory] -> Bool) -> m Bool
signatoriesSatisfyTweak = (<$> getSignatoriesTweak)

-- | Check if a signatory signs a transaction
isSignatoryTweak :: (MonadTweak m) => TxSkelSignatory -> m Bool
isSignatoryTweak = signatoriesSatisfyTweak . elem

-- | Check if the transaction has at least a signatory
hasSignatoriesTweak :: (MonadTweak m) => m Bool
hasSignatoriesTweak = signatoriesSatisfyTweak (not . null)

-- | Add a signatory to the transaction, at the head of the list of signatories, and
-- return the old list of signatories
addFirstSignatoryTweak :: (MonadTweak m) => TxSkelSignatory -> m [TxSkelSignatory]
addFirstSignatoryTweak = modifySignatoriesTweak . (:)

-- | Add signatories at the end of the list of signatories, and return the old list of
-- signatories
addSignatoriesTweak :: (MonadTweak m) => [TxSkelSignatory] -> m [TxSkelSignatory]
addSignatoriesTweak = modifySignatoriesTweak . (<>)

-- | Add a signatory to the transaction, at the end of the list of signatories, and
-- return the old list of signatories
addLastSignatoryTweak :: (MonadTweak m) => TxSkelSignatory -> m [TxSkelSignatory]
addLastSignatoryTweak = addSignatoriesTweak . (: [])

-- | Remove signatories from the transaction and return the old list of signatories
removeSignatoriesTweak :: (MonadTweak m) => [TxSkelSignatory] -> m [TxSkelSignatory]
removeSignatoriesTweak = modifySignatoriesTweak . (\\)

-- | Remove a signatory from the transaction and return the old list of signatories
removeSignatoryTweak :: (MonadTweak m) => TxSkelSignatory -> m [TxSkelSignatory]
removeSignatoryTweak = modifySignatoriesTweak . delete

-- | Changes the first signatory (adds it if there are no signatories) and return the
-- old list of signatories.
replaceFirstSignatoryTweak :: (MonadTweak m) => TxSkelSignatory -> m [TxSkelSignatory]
replaceFirstSignatoryTweak =
  modifySignatoriesTweak
    . ( \newSignatory -> \case
          [] -> [newSignatory]
          (_ : ss) -> newSignatory : ss
      )
