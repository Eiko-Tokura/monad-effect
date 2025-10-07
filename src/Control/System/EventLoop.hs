module Control.System.EventLoop where

import Control.System
import Control.Monad.Effect
import Control.Concurrent.STM

eventLoop :: forall mods es. (ConsFDataList FData mods, System FData mods es) => Eff mods es ()
eventLoop = do
  beforeSystem
  listenToEvents >>= liftIO . atomically >>= handleEvents @_ @mods
  afterSystem
  eventLoop
