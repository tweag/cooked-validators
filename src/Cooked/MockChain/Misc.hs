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
  )
where

import Cooked.Pretty
import Polysemy

-- | An effect that corresponds to extra QOL capabilities of the MockChain
data MockChainMisc :: Effect where
  Define :: (ToHash a) => String -> a -> MockChainMisc m a

makeSem_ ''MockChainMisc

-- | Interpreting a `MockChainMisc` in terms of a writer of @Map
-- BuiltinByteString String@
runMockChainMisc ::
  forall effs a.
  (Member (Writer (Map Api.BuiltinByteString String)) effs) =>
  Sem (MockChainMisc : effs) a ->
  Sem effs a
runMockChainMisc = interpret $
  \(Define name hashable) -> do
    tell $ Map.singleton (toHash hashable) name
    return hashable

-- | Stores an alias matching a hashable data for pretty printing purpose
define :: (Member MockChainMisc effs, ToHash a) => String -> a -> Sem effs a

-- | Like `define`, but binds the result of a monadic computation instead
defineM :: (Member MockChainMisc effs) => String -> Sem effs a -> Sem effs a
defineM name = (define name =<<)
