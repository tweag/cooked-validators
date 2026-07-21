-- | 'Tweak's working on the outputs of a 'TxSkel'
module Cooked.Tweak.Outputs
  ( addOutputTweak,
    removeOutputsTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Insertion
import Cooked.Tweak.Removal
import Polysemy

-- | Adds a transaction output, at the end of the current list of outputs, thus
-- retaining the initial output order.
addOutputTweak ::
  (Member Tweak effs) =>
  TxSkelOut ->
  Sem effs ()
addOutputTweak = insertLastTweak txSkelOutputsL

-- | Removes transaction outputs according to some predicate. The returned list
-- contains all the removed outputs.
removeOutputsTweak ::
  (Member Tweak effs) =>
  (TxSkelOut -> Bool) ->
  Sem effs [TxSkelOut]
removeOutputsTweak = removeIfTweak txSkelOutputsL
