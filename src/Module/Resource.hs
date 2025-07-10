{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Provides ResourceT functionality for managing resources
module Module.Resource where

import Control.Monad.Effect
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource.Internal
import Control.System
import Data.Default
import Data.IORef
import GHC.Generics (Generic)

data Resource

instance Module Resource where
  newtype ModuleRead Resource = ResourceRead { resourceRead :: IORef (ReleaseMap) }
  data    ModuleState Resource = ResourceState

instance SystemModule Resource where
  data ModuleInitData Resource = ResourceInitData deriving (Generic, Default)
  data ModuleEvent    Resource = ResourceEvent

-- | Not really orphan because it is EffT'
instance (In' c Resource mods, MonadIO m) => MonadResource (EffT' c mods es m) where
  liftResourceT = \ResourceT{unResourceT} -> do
    rMap <- asksModule resourceRead
    liftIO $ unResourceT rMap

instance Loadable c Resource mods where
  initModule _ = do
    istate <- createInternalState
    return (ResourceRead istate, ResourceState)
