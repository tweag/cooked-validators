-- | Tweaks working on the minting part of a skeleton
module Cooked.Tweak.Mint
  ( addMintTweak,
    removeMintTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List (partition)
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Add a new entry to the 'TxSkelMints' of the transaction skeleton under
-- modification. As this is implemented in terms of 'addToTxSkelMints', the same
-- caveats apply as do to that function!
addMintTweak :: (MonadTweak m) => (Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer) -> m ()
addMintTweak mint = overTweak txSkelMintsL $ addToTxSkelMints mint

-- | Remove some entries from the 'TxSkelMints' of a transaction, according to
-- some predicate. The returned list holds the removed entries.
removeMintTweak ::
  (MonadTweak m) =>
  ((Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer) -> Bool) ->
  m [(Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer)]
removeMintTweak removePred = do
  presentMints <- viewTweak $ txSkelMintsL % to txSkelMintsToList
  let (removed, kept) = partition removePred presentMints
  setTweak txSkelMintsL $ txSkelMintsFromList kept
  return removed
