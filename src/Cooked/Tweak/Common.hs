-- | This module defines 'Tweak's which are the building blocks of our DSL for
-- attacks. They are skeleton modifications aware of the mockchain state.
module Cooked.Tweak.Common
  ( -- * Tweak effect
    Tweak (..),
    getTxSkel,
    putTxSkel,

    -- * Running a tweak
    runTweak,
    evalTweak,
    execTweak,

    -- * Viewing tweaks
    viewTweak,
    iviewTweak,
    viewAllTweak,
    viewAnyTweak,

    -- * Setting tweaks
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

import Control.Monad
import Cooked.Skeleton
import Optics.Core
import Polysemy
import Polysemy.NonDet
import Polysemy.State

-- * Tweaks: state aware modifications over a 'TxSkel'

-- | An effect that allows to store or retrieve a 'TxSkel' from a context
data Tweak :: Effect where
  -- | Retrieves the 'TxSkel' from the context
  GetTxSkel :: Tweak m TxSkel
  -- | Overrides the 'TxSkel' in the context
  PutTxSkel :: TxSkel -> Tweak m ()

makeSem ''Tweak

-- * Running 'Tweak's

-- | Running a Tweak is equivalent to running a state monad storing a 'TxSkel'
runTweak ::
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs (TxSkel, a)
runTweak txSkel =
  runState txSkel
    . reinterpret
      ( \case
          GetTxSkel -> get
          PutTxSkel skel -> put skel
      )

-- | Same as 'runTweak' but discards the returned 'TxSkel'
evalTweak ::
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs a
evalTweak skel = (snd <$>) . runTweak skel

-- | Same as 'runTweak' but discards the returned value
execTweak ::
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs TxSkel
execTweak skel = (fst <$>) . runTweak skel

-- * Basic viewing 'Tweak's

-- | Retrieves the focus from the 'TxSkel' given a getter
viewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k is TxSkel a ->
  Sem effs a
viewTweak optic = getTxSkel <&> view optic

-- | Like 'viewTweak', but also returns the index associated with the retrieved
-- focus.
iviewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k (WithIx i) TxSkel a ->
  Sem effs (i, a)
iviewTweak optic = getTxSkel <&> iview optic

-- | Retrieves all the foci targeted by a given fold within a 'TxSkel' and
-- returns them as a list.
viewAllTweak ::
  (Member Tweak effs, Is k A_Fold) =>
  Optic' k is TxSkel a ->
  Sem effs [a]
viewAllTweak optic = getTxSkel <&> toListOf optic

-- | Like 'viewAllTweak', but returns each focus in a separate branch
viewAnyTweak ::
  (Members '[Tweak, NonDet] effs, Is k A_Fold) =>
  Optic' k is TxSkel a ->
  Sem effs a
viewAnyTweak optic = viewAllTweak optic >>= msum . fmap return

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
