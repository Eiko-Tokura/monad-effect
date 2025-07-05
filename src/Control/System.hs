{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
module Control.System
  (
  -- * Module and System
    Module(..), System(..), Loadable(..)
  , runSystemWithInitData

  , queryModule, queriesModule
  , localModule
  , getModule, getsModule
  , putModule, modifyModule

  -- * Loadable
  , ModuleInitDataHardCode
  , LoadableEnv(..), LoadableArgs(..), SystemEnv(..), SystemArgs(..)

  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Effect
import Data.Kind
import Data.TypeList

-- class Module mod => SystemModule mod where

type SystemInitDataHardCode c mods = c ModuleInitDataHardCode mods

-- | Run a System of EffT given initData
--
-- If error happens at initialization, it will return Left SystemError
-- If error happens during normal flow, it will return Right (Left (SList (SystemError : es)))
runSystemWithInitData :: forall mods es m c a. (ConsFDataList c mods, System mods, MonadIO m)
  => SystemInitData c mods
  -> EffT c mods es m a
  -> m
      (Either SystemError -- ^ if error happens at initialization
        ( Result es a     -- ^ if error happens during event loop
        , SystemState c mods)
      )
runSystemWithInitData initData eff = do
  liftIO (runEffT0 (initAllModules @mods initData)) >>= \case
    RSuccess (rs, ss)  -> Right <$> runEffT rs ss eff
    RFailure (EHead e) -> return $ Left e

-- | Specifies that the module can load after mods are loaded
-- in practice we could use
-- instance SomeModuleWeNeed `In` mods => Loadable mods SomeModuleToLoad
class Loadable mod mods where
  initModule  :: ModuleInitData mod -> EffT c mods '[SystemError] IO (ModuleRead mod, ModuleState mod)

  beforeEvent :: EffT c (mod : mods) NoError IO ()
  beforeEvent = return ()
  {-# INLINE beforeEvent #-}

  afterEvent  :: EffT c (mod : mods) NoError IO ()
  afterEvent = return ()
  {-# INLINE afterEvent #-}

  moduleEvent :: EffT c (mod : mods) NoError IO (STM (ModuleEvent mod))
  moduleEvent = return empty
  {-# INLINE moduleEvent #-}

  handleEvent :: ModuleEvent mod -> EffT c (mod : mods) NoError IO ()
  handleEvent _ = return ()
  {-# INLINE handleEvent #-}

  releaseModule :: EffT c (mod : mods) NoError IO () -- ^ release resources, quit module
  releaseModule = return ()
  {-# INLINE releaseModule #-}

data family ModuleInitDataHardCode mod :: Type

class Loadable mod mods => LoadableEnv mod mods where
  readInitDataFromEnv :: ModuleInitDataHardCode mod -> EffT c '[] '[SystemError] IO (ModuleInitData mod)
  -- ^ Read module init data from environment, or other means
  -- one can change this to SystemInitData mods -> IO (ModuleInitData mod)

class Loadable mod mods => LoadableArgs mod mods where
  readInitDataFromArgs :: ModuleInitDataHardCode mod -> [String] -> EffT c '[] '[SystemError] IO (ModuleInitData mod)
  -- ^ Read module init data from command line arguments, or using comand line arguments to read other things
  -- one can change this to SystemInitData mods -> [String] -> IO (ModuleInitData mod)

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
--
-- the last module in the list is the first to be loaded
-- and also the first to execute beforeEvent and afterEvent
class System mods where
  initAllModules :: ConsFDataList c mods => SystemInitData c mods -> EffT c '[] '[SystemError] IO (SystemRead c mods, SystemState c mods)

  listenToEvents :: ConsFDataList c mods => EffT c mods '[] IO (STM (SystemEvent mods))

  handleEvents :: ConsFDataList c mods => SystemEvent mods -> EffT c mods '[] IO ()

  beforeSystem :: ConsFDataList c mods => EffT c mods '[] IO ()

  afterSystem  :: ConsFDataList c mods => EffT c mods '[] IO ()

  releaseSystem :: ConsFDataList c mods => EffT c mods '[] IO ()
  -- ^ safely release all resources system acquired
  -- Warning: releaseSystem is done in reverse order of initAllModules
  -- i.e. the head of the list is the first to be released

class System mods => SystemEnv mods where
  readSystemInitDataFromEnv :: ConsFDataList c mods => SystemInitDataHardCode c mods -> EffT c '[] '[SystemError] IO (SystemInitData c mods)

class System mods => SystemArgs mods where
  readSystemInitDataFromArgs :: ConsFDataList c mods => SystemInitDataHardCode c mods -> [String] -> EffT c '[] '[SystemError] IO (SystemInitData c mods)

-- | base case for system
instance System '[] where
  initAllModules _ = return (fNil, fNil)
  {-# INLINE initAllModules #-}

  listenToEvents = return empty
  {-# INLINE listenToEvents #-}

  handleEvents _ = return ()
  {-# INLINE handleEvents #-}

  beforeSystem = return ()
  {-# INLINE beforeSystem #-}

  afterSystem = return ()
  {-# INLINE afterSystem #-}

  releaseSystem = return ()
  {-# INLINE releaseSystem #-}

instance SystemEnv '[] where
  readSystemInitDataFromEnv _ = return fNil
  {-# INLINE readSystemInitDataFromEnv #-}

instance SystemArgs '[] where
  readSystemInitDataFromArgs _ _ = do
    return fNil
  {-# INLINE readSystemInitDataFromArgs #-}

-- | Inductive instance for system
instance (Module mod, System mods, Loadable mod mods) => System (mod ': mods) where
  initAllModules (x :*** xs) = do
    (rs, ss)  <- initAllModules xs -- >>= _
    (er, ss') <- liftIO $ runEffT rs ss $ initModule @mod x
    case er of
      RSuccess (r', s') -> return (r' :*** rs, s' :*** ss')
      RFailure (EHead e) -> effThrowIn e
  {-# INLINE initAllModules #-}

  beforeSystem = do
    embedEffT $ beforeSystem @mods
    beforeEvent @mod
  {-# INLINE beforeSystem #-}

  afterSystem = do
    embedEffT $ afterSystem @mods
    afterEvent @mod
  {-# INLINE afterSystem #-}

  listenToEvents = do
    tailEvents <- embedEffT $ listenToEvents @mods
    headEvent  <- moduleEvent @mod
    return $ UHead <$> headEvent <|> UTail <$> tailEvents
  {-# INLINE listenToEvents #-}

  handleEvents (UHead x) = handleEvent @mod x
  handleEvents (UTail xs) = embedEffT $ handleEvents @mods xs
  {-# INLINE handleEvents #-}

  releaseSystem = do
    releaseModule @mod
    embedEffT $ releaseSystem @mods
  {-# INLINE releaseSystem #-}

instance (SubList c mods (mod:mods), Module mod, SystemEnv mods, Loadable mod mods, LoadableEnv mod mods) => SystemEnv (mod ': mods) where
  readSystemInitDataFromEnv (im :*** ims) = do
    xs <- readSystemInitDataFromEnv @mods ims
    x  <- readInitDataFromEnv @mod @mods im
    return $ x :*** xs
  {-# INLINE readSystemInitDataFromEnv #-}

instance (SubList c mods (mod:mods), Module mod, SystemArgs mods, Loadable mod mods, LoadableArgs mod mods) => SystemArgs (mod ': mods) where
  readSystemInitDataFromArgs (im :*** ims) args = do
    xs <- readSystemInitDataFromArgs @mods ims args
    x  <- readInitDataFromArgs @mod @mods im args
    return $ x :*** xs
  {-# INLINE readSystemInitDataFromArgs #-}
