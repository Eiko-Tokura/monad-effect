{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.System.EventLoop where

import Control.System
import Control.Monad.Effect
import Control.Concurrent.STM

eventLoop :: forall mods es. (ConsFDataList FData mods, EventLoopSystem FData mods es ) => Eff mods es ()
eventLoop = do
  beforeSystem @_ @_ @_
  listenToEvents @_ @_ @_ >>= liftIO . atomically >>= handleEvents @_ @mods @es
  afterSystem @_ @_ @_
  eventLoop @mods @es
