module Control.Monad.Effect.EventLoop where

import Control.Monad.Effect
import Control.Concurrent.STM

eventLoop :: forall mods . (System mods) => Eff mods NoError ()
eventLoop = do
  beforeSystem
  listenToEvents >>= liftIO . atomically >>= handleEvents @mods
  afterSystem
  eventLoop

-- | Avoid runtime error (IO) though, make sure every loop breaking error is in SystemError
eventLoopWithRelease :: forall mods . (System mods) => Eff mods NoError ()
eventLoopWithRelease = Eff $ \rs ss -> do
  (e, ss') <- unEff @mods eventLoop rs ss
  _ <- runEff rs ss' $ releaseSystem @mods
  return (e, ss')

-- | When restart, the state is reset to the initial state
eventLoopWithReleaseRestartIO :: forall mods . (System mods) => SystemInitData mods -> IO () -- Eff mods NoError ()
eventLoopWithReleaseRestartIO initData = do
  _ <- runEffWithInitData @mods initData eventLoopWithRelease
  eventLoopWithReleaseRestartIO initData
