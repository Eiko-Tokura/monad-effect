module Control.Monad.Effect.EventLoop where

import Control.Monad.Effect
import Control.Monad.RST
import Control.Concurrent.STM
import Data.HList

eventLoop :: forall mods . (System mods) => Eff mods NoError ()
eventLoop = do
  beforeSystem
  listenToEvents >>= liftIO . atomically >>= handleEvents @mods
  afterSystem
  eventLoop

-- | Avoid runtime error (IO) though, make sure every loop breaking error is in SystemError
eventLoopWithRelease :: forall mods . (System mods) => Eff mods NoError (Maybe SystemError)
eventLoopWithRelease = Eff $ RSE $ \rs ss -> do
  (e, ss') <- runRSE (unEff @mods eventLoop) rs ss
  _ <- runEff rs ss' $ releaseSystem @mods
  case e of
    Left (SHead sysE)  -> return (Right (Just sysE), ss')
    _                  -> return (Right Nothing, ss')

-- | When restart, the state is reset to the initial state
eventLoopWithReleaseRestartIO :: forall mods . (System mods) => SystemInitData mods -> IO () -- Eff mods NoError ()
eventLoopWithReleaseRestartIO initData = do
  _ <- runEffWithInitData @mods initData eventLoopWithRelease
  eventLoopWithReleaseRestartIO initData

