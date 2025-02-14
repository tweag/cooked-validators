-- | Tweaks working on the outputs of a skeleton
module Cooked.Tweak.Outputs
  ( ensureOutputTweak,
    addOutputTweak,
    removeOutputTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List

-- | Ensure that a certain output is produced by a transaction. The return value
-- will be @Just@ the added output, when applicable.
ensureOutputTweak :: (MonadTweak m) => TxSkelOut -> m (Maybe TxSkelOut)
ensureOutputTweak txSkelOut = do
  presentOutputs <- viewTweak txSkelOutsL
  if txSkelOut `elem` presentOutputs
    then return Nothing
    else do
      addOutputTweak txSkelOut
      return $ Just txSkelOut

-- | Add a transaction output, at the end of the current list of outputs, thus
-- retaining the initial outputs order.
addOutputTweak :: (MonadTweak m) => TxSkelOut -> m ()
addOutputTweak txSkelOut = overTweak txSkelOutsL (++ [txSkelOut])

-- | Remove transaction outputs according to some predicate. The returned list
-- contains all the removed outputs.
removeOutputTweak :: (MonadTweak m) => (TxSkelOut -> Bool) -> m [TxSkelOut]
removeOutputTweak removePred = do
  presentOutputs <- viewTweak txSkelOutsL
  let (removed, kept) = partition removePred presentOutputs
  setTweak txSkelOutsL kept
  return removed
