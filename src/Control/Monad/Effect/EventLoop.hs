module Control.Monad.Effect.EventLoop where

import Control.System
import Control.Monad.Effect
import Control.Concurrent.STM

eventLoop :: forall mods . (ConsFDataList FData mods, System mods) => Eff mods NoError ()
eventLoop = do
  beforeSystem
  listenToEvents >>= liftIO . atomically >>= handleEvents @mods
  afterSystem
  eventLoop

-- | Avoid runtime error (IO) though, make sure every loop breaking error is in SystemError
eventLoopWithRelease :: forall mods. (ConsFDataList FData mods, System mods) => Eff mods NoError ()
eventLoopWithRelease = EffT $ \rs ss -> do
  (e, ss') <- unEffT @_ @mods eventLoop rs ss
  _ <- runEffT rs ss' $ releaseSystem @mods
  return (e, ss')

-- | When restart, the state is reset to the initial state
eventLoopWithReleaseRestartIO :: forall mods. (ConsFDataList FData mods, System mods) => SystemInitData FData mods -> IO () -- Eff mods NoError ()
eventLoopWithReleaseRestartIO initData = do
  _ <- runSystemWithInitData @mods initData eventLoopWithRelease
  eventLoopWithReleaseRestartIO initData
