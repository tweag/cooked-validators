-- | This module provides straightforward tweaks to modify parts of the current
-- 'TxSkel' based on various kinds of optics.
module Cooked.Tweak.Update
  ( -- * Setting tweaks
    setTweak,
    isetTweak,

    -- * Overing tweaks
    overTweak,
    ioverTweak,

    -- * Traversing tweaks
    traverseTweak,
    itraverseTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Optics.Core
import Polysemy

-- * Basic modifying 'Tweak's

-- | The tweak that sets a certain value in the 'TxSkel'.
setTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic' k is TxSkel a ->
  a ->
  Sem effs ()
setTweak optic a = getTxSkel >>= putTxSkel . set optic a

-- | Like 'setTweak', but the value to set is computed from the index of each
-- focus.
isetTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic' k (WithIx i) TxSkel a ->
  (i -> a) ->
  Sem effs ()
isetTweak optic f = getTxSkel >>= putTxSkel . iset optic f

-- | The tweak that modifies a certain value in the 'TxSkel'.
overTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic k is TxSkel TxSkel a b ->
  (a -> b) ->
  Sem effs ()
overTweak optic change = getTxSkel >>= putTxSkel . over optic change

-- | Like 'overTweak', but the modification of each focus also depends on its
-- index.
ioverTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic k (WithIx i) TxSkel TxSkel a b ->
  (i -> a -> b) ->
  Sem effs ()
ioverTweak optic change = getTxSkel >>= putTxSkel . iover optic change

-- | Like 'overTweak', but the modification of each focus runs in the tweak's
-- effect stack. The foci are visited in the order in which they occur in the
-- 'TxSkel'.
traverseTweak ::
  (Member Tweak effs, Is k A_Traversal) =>
  Optic' k is TxSkel a ->
  (a -> Sem effs a) ->
  Sem effs ()
traverseTweak optic change = getTxSkel >>= traverseOf optic change >>= putTxSkel

-- | Like 'traverseTweak', for indexed optics
itraverseTweak ::
  (Member Tweak effs, Is k A_Traversal) =>
  Optic' k (WithIx is) TxSkel a ->
  (is -> a -> Sem effs a) ->
  Sem effs ()
itraverseTweak optic change = getTxSkel >>= itraverseOf optic change >>= putTxSkel
