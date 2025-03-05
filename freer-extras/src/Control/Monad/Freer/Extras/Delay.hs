{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Freer.Extras.Delay where

import Control.Concurrent (threadDelay)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, type (~>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Units (TimeUnit, toMicroseconds)

data DelayEffect r where
  DelayThread :: (TimeUnit a) => a -> DelayEffect ()

delayThread :: (TimeUnit a) => (Member DelayEffect effs) => a -> Eff effs ()
delayThread = send . DelayThread

handleDelayEffect ::
  forall effs m.
  (LastMember m effs, MonadIO m) =>
  Eff (DelayEffect ': effs) ~> Eff effs
handleDelayEffect =
  interpret $ \case
    DelayThread t ->
      liftIO . threadDelay . fromIntegral . toMicroseconds $ t
