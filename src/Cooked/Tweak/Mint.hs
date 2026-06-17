-- | 'Tweak's working on the minting part of a 'TxSkel'
module Cooked.Tweak.Mint
  ( addMintTweak,
    addMintsTweak,
    removeMintsTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List (partition)
import Optics.Core
import Polysemy

-- | Adds a single entry to the 'TxSkelMints' of the transaction skeleton under
-- modification.
addMintTweak ::
  (Member Tweak effs) =>
  Mint ->
  Sem effs ()
addMintTweak = addMintsTweak . (: [])

-- | Adds new entries to the 'TxSkelMints' of the transaction skeleton under
-- modification.
addMintsTweak ::
  (Member Tweak effs) =>
  [Mint] ->
  Sem effs ()
addMintsTweak newMints = overTweak (txSkelMintsL % txSkelMintsListI) (++ newMints)

-- | Remove some entries from the 'TxSkelMints' of a transaction, according to
-- some predicate. The returned list holds the removed entries.
removeMintsTweak ::
  (Member Tweak effs) =>
  (Mint -> Bool) ->
  Sem effs [Mint]
removeMintsTweak removePred = do
  presentMints <- viewTweak $ txSkelMintsL % txSkelMintsListI
  let (removed, kept) = partition removePred presentMints
  setTweak (txSkelMintsL % txSkelMintsListI) kept
  return removed
