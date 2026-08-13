{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives required to log internal pieces of
-- information during a mockchain run. This includes, in particular, all the
-- adjustment automatically done by \cooked-validators\ during the transaction
-- processing phase. This effect is typically not available to users, and should
-- solely be used to track internal events. To trace additional elements from a
-- user's perspective, use `Cooked.Effect.Misc.note` instead.
module Cooked.Effect.Log
  ( -- * Logging effect
    Log,
    runMockChainLog,
    runBlockChainLog,

    -- * Logging primitive
    logEvent,
  )
where

import Cooked.Pretty.Class
import Cooked.Pretty.MockChain ()
import Cooked.Pretty.Options
import Cooked.Pretty.Skeleton
import Cooked.Runtime.Journal
import Cooked.Runtime.State
import Polysemy
import Polysemy.State
import Polysemy.Writer

-- | An effect to allow logging of mockchain events
data Log :: Effect where
  LogEvent :: ChainLogEntry -> Log m ()

makeSem_ ''Log

-- | Logs an internal event occurring while processing a transaction skeleton
logEvent :: (Member Log effs) => ChainLogEntry -> Sem effs ()

-- | Interpreting a `Log` in terms of a writer of
-- @[MockChainLogEntry]@
runMockChainLog ::
  (Member (Writer ChainJournal) effs) =>
  Sem (Log : effs) a ->
  Sem effs a
runMockChainLog = interpret $ \(LogEvent event) -> tell $ fromLogEntry event

-- | Interpreting a 'Log' by directly producing a trace on the standard output
-- for each log entry.
runBlockChainLog ::
  ( Members
      '[ Embed IO,
         State PrettyCookedOpts,
         State ChainIndex
       ]
      effs
  ) =>
  Sem (Log : effs) a ->
  Sem effs a
runBlockChainLog = interpret $ \(LogEvent event) -> do
  opts <- get
  index <- gets chainIndexOutputs
  embed $ printCookedOpt opts $ Contextualized index event
