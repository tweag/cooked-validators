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
  )
where

import Cooked.Skeleton
import Polysemy
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
