module Control.System.EventLoop where

import Control.System
import Control.Monad.Effect
import Control.Concurrent.STM

eventLoop :: forall mods . (ConsFDataList FData mods, System FData mods) => Eff mods NoError ()
eventLoop = do
  beforeSystem
  listenToEvents >>= liftIO . atomically >>= handleEvents @_ @mods
  afterSystem
  eventLoop

-- | Avoid runtime error (IO) though, make sure every loop breaking error is in SystemError
eventLoopWithRelease :: forall mods. (ConsFDataList FData mods, System FData mods) => Eff mods NoError ()
eventLoopWithRelease = do
  eventLoop
  releaseSystem

-- | When restart, the state is reset to the initial state
eventLoopWithReleaseRestartIO :: forall mods. (ConsFDataList FData mods, System FData mods) => SystemInitData FData mods -> IO ()
eventLoopWithReleaseRestartIO initData = do
  _ <- runSystemWithInitData @mods initData eventLoopWithRelease
  eventLoopWithReleaseRestartIO initData
