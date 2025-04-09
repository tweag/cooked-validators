-- | 'Tweak's working on the minting part of a 'TxSkel'
module Cooked.Tweak.Mint
  ( addMintTweak,
    removeMintTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List (partition)
import Optics.Core

-- | Adds a new entry to the 'TxSkelMints' of the transaction skeleton under
-- modification. As this is implemented in terms of 'addMint', the same caveats
-- apply as do to that function!
addMintTweak :: (MonadTweak m) => Mint -> m ()
addMintTweak = overTweak txSkelMintsL . flip addMint

-- | Remove some entries from the 'TxSkelMints' of a transaction, according to
-- some predicate. The returned list holds the removed entries.
removeMintTweak :: (MonadTweak m) => (Mint -> Bool) -> m [Mint]
removeMintTweak removePred = do
  presentMints <- viewTweak $ txSkelMintsL % to txSkelMintsToList
  let (removed, kept) = partition removePred presentMints
  setTweak txSkelMintsL $ txSkelMintsFromList kept
  return removed
