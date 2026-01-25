{-# LANGUAGE TemplateHaskell #-}

-- | This module defines primitives that offer quality of life features when
-- operating a mockchain without interacting with the mockchain state itself.
module Cooked.MockChain.Misc
  ( -- * Misc effect
    MockChainMisc,
    runMockChainMisc,

    -- * Misc primitives
    define,
    defineM,
    note,
    noteP,
  )
where

import Cooked.Pretty.Class
import Cooked.Pretty.Hashable
import Data.Map (Map)
import Data.Map qualified as Map
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Writer

-- | An effect that corresponds to extra QOL capabilities of the MockChain
data MockChainMisc :: Effect where
  Define :: (ToHash a) => String -> a -> MockChainMisc m a
  Note :: (Show s) => s -> MockChainMisc m ()

makeSem_ ''MockChainMisc

-- | Interpreting a `MockChainMisc` in terms of a writer of @Map
-- BuiltinByteString String@
runMockChainMisc ::
  forall effs a.
  (Members '[Writer (Map Api.BuiltinByteString String), Writer [String]] effs) =>
  Sem (MockChainMisc : effs) a ->
  Sem effs a
runMockChainMisc = interpret $ \case
  (Define name hashable) -> do
    tell $ Map.singleton (toHash hashable) name
    return hashable
  (Note s) -> tell [show s]

-- | Stores an alias matching a hashable data for pretty printing purpose
define :: forall effs a. (Member MockChainMisc effs, ToHash a) => String -> a -> Sem effs a

-- | Takes note of a showable element to trace at the end of the run
note :: forall effs s. (Member MockChainMisc effs, Show s) => s -> Sem effs ()

-- | Takes note of a pretty-printable element to trace at the end of the run
noteP :: forall effs s. (Member MockChainMisc effs, PrettyCooked s) => s -> Sem effs ()
noteP = note . prettyCooked

-- | Like `define`, but binds the result of a monadic computation instead
defineM :: (Member MockChainMisc effs, ToHash a) => String -> Sem effs a -> Sem effs a
defineM name = (define name =<<)
