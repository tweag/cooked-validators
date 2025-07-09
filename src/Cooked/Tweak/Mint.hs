-- | 'Tweak's working on the minting part of a 'TxSkel'
module Cooked.Tweak.Mint
  ( addMintsTweak,
    removeMintTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List (partition)
import Optics.Core

-- | Adds new entries to the 'TxSkelMints' of the transaction skeleton under
-- modification.
addMintsTweak :: (MonadTweak m) => [Mint] -> m ()
addMintsTweak newMints = overTweak (txSkelMintsL % txSkelMintsListI) (++ newMints)

-- | Remove some entries from the 'TxSkelMints' of a transaction, according to
-- some predicate. The returned list holds the removed entries.
removeMintTweak :: (MonadTweak m) => (Mint -> Bool) -> m [Mint]
removeMintTweak removePred = do
  presentMints <- viewTweak $ txSkelMintsL % txSkelMintsListI
  let (removed, kept) = partition removePred presentMints
  setTweak (txSkelMintsL % txSkelMintsListI) kept
  return removed
