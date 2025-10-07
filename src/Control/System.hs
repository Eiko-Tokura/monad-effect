{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
module Control.System
  (
  -- * Module and System
    Module(..)

  , System(..), Loadable(..)

  -- , runSystemWithInitData

  , askModule, asksModule
  , queryModule, queriesModule
  , localModule
  , getModule, getsModule
  , putModule, modifyModule

  -- * Loadable
  , ModuleInitDataHardCode
  
  , LoadableEnv(..), LoadableArgs(..), SystemEnv(..), SystemArgs(..)
  , SystemInitDataHardCode'
  , SystemInitDataHardCode
  , SystemInitDataHardCodeL
  , Dependency, Dependency'

  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Effect
import Data.Kind
import Data.TypeList

type family DependencyW (mod :: Type) (deps :: [Type]) (mods :: [Type]) :: Constraint where
  DependencyW mod '[] mods = mod `In` (mod : mods)
  DependencyW mod (dep ': deps) mods = (dep `In` mods, dep `In` (mod : mods), DependencyW mod deps mods)

type family DependencyW' c (mod :: Type) (deps :: [Type]) (mods :: [Type]) :: Constraint where
  DependencyW' c mod '[] mods = (In' c mod (mod : mods))
  DependencyW' c mod (dep ': deps) mods = (In' c dep mods, In' c dep (mod : mods), DependencyW' c mod deps mods)

-- | A type family that can be used to generate the constraints
-- to make specifying module dependencies easier
type family Dependency (mod :: Type) (deps :: [Type]) (mods :: [Type]) :: Constraint where
  Dependency mod deps mods = (ConsFDataList FData (mod : mods), DependencyW mod deps mods)

type family Dependency' c (mod :: Type) (deps :: [Type]) (mods :: [Type]) :: Constraint where
  Dependency' c mod deps mods = (ConsFDataList c (mod : mods), DependencyW' c mod deps mods)

type SystemInitDataHardCode' c mods = c ModuleInitDataHardCode mods
type SystemInitDataHardCode    mods = SystemInitDataHardCode' FData mods
type SystemInitDataHardCodeL   mods = SystemInitDataHardCode' FList mods

-- | Run a System of EffT' given initData
--
-- If error happens at initialization, it will return Left SystemError
-- If error happens during normal flow, it will return Right (RFailure SystemError)
-- runSystemWithInitData :: forall mods es m c a. (ConsFDataList c mods, System c mods es, MonadIO m)
--   => SystemInitData c mods
--   -> EffT' c mods es m a
--   -> m
--       (Either SystemError -- ^ if error happens at initialization
--         ( Result es a     -- ^ if error happens during event loop
--         , SystemState c mods)
--       )
-- runSystemWithInitData initData eff = do
--   liftIO (runEffT0 (initAllModules @c @mods initData)) >>= \case
--     RSuccess (rs, ss)  -> Right <$> runEffT rs ss eff
--     RFailure (EHead e) -> return $ Left e

-- | Specifies that the module can load after mods are loaded
-- in practice we could use
-- instance SomeModuleWeNeed `In` mods => Loadable mods SomeModuleToLoad
class Loadable c mod mods es where
  {-# MINIMAL withModule #-}
  withModule :: ModuleInitData mod -> EffT' c (mod : mods) es IO a -> EffT' c mods es IO a

  beforeEvent :: EffT' c (mod : mods) es IO ()
  beforeEvent = return ()
  {-# INLINE beforeEvent #-}

  afterEvent  :: EffT' c (mod : mods) es IO ()
  afterEvent = return ()
  {-# INLINE afterEvent #-}

  moduleEvent :: EffT' c (mod : mods) es IO (STM (ModuleEvent mod))
  moduleEvent = return empty
  {-# INLINE moduleEvent #-}

  handleEvent :: ModuleEvent mod -> EffT' c (mod : mods) es IO ()
  handleEvent _ = return ()
  {-# INLINE handleEvent #-}

  -- releaseModule :: EffT' c (mod : mods) es IO () -- ^ release resources, quit module
  -- releaseModule = return ()
  -- {-# INLINE releaseModule #-}

data family ModuleInitDataHardCode mod :: Type

class Loadable c mod mods es => LoadableEnv c mod mods es where
  readInitDataFromEnv :: ModuleInitDataHardCode mod -> EffT' c '[] es IO (ModuleInitData mod)
  -- ^ Read module init data from environment, or other means
  -- one can change this to SystemInitData mods -> IO (ModuleInitData mod)

class Loadable c mod mods es => LoadableArgs c mod mods es where
  readInitDataFromArgs :: ModuleInitDataHardCode mod -> [String] -> EffT' c '[] es IO (ModuleInitData mod)
  -- ^ Read module init data from command line arguments, or using comand line arguments to read other things
  -- one can change this to SystemInitData mods -> [String] -> IO (ModuleInitData mod)

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
--
-- the last module in the list is the first to be loaded
-- and also the first to execute beforeEvent and afterEvent
class System c mods es where
  withAllModules :: ConsFDataList c mods => SystemInitData c mods -> EffT' c mods es IO a -> EffT' c '[] es IO a

  listenToEvents :: ConsFDataList c mods => EffT' c mods es IO (STM (SystemEvent mods))

  handleEvents :: ConsFDataList c mods => SystemEvent mods -> EffT' c mods es IO ()

  beforeSystem :: ConsFDataList c mods => EffT' c mods es IO ()

  afterSystem  :: ConsFDataList c mods => EffT' c mods es IO ()


class System c mods es => SystemEnv c mods es where
  readSystemInitDataFromEnv :: ConsFDataList c mods => SystemInitDataHardCode' c mods -> EffT' c '[] es IO (SystemInitData c mods)

class System c mods es => SystemArgs c mods es where
  readSystemInitDataFromArgs :: ConsFDataList c mods => SystemInitDataHardCode' c mods -> [String] -> EffT' c '[] es IO (SystemInitData c mods)

-- | base case for system
instance System c '[] es where
  withAllModules _ = id
  -- return (fNil, fNil)
  {-# INLINE withAllModules #-}

  listenToEvents = return empty
  {-# INLINE listenToEvents #-}

  handleEvents _ = return ()
  {-# INLINE handleEvents #-}

  beforeSystem = return ()
  {-# INLINE beforeSystem #-}

  afterSystem = return ()
  {-# INLINE afterSystem #-}

instance SystemEnv c '[] es where
  readSystemInitDataFromEnv _ = return fNil
  {-# INLINE readSystemInitDataFromEnv #-}

instance SystemArgs c '[] es where
  readSystemInitDataFromArgs _ _ = do
    return fNil
  {-# INLINE readSystemInitDataFromArgs #-}

-- | Inductive instance for system
instance (SystemModule mod, System c mods es, Loadable c mod mods es) => System c (mod ': mods) es where
  withAllModules (x :*** xs) = withAllModules xs . withModule @c @mod x
  {-# INLINE withAllModules #-}

  beforeSystem = do
    embedMods $ beforeSystem @c @mods
    beforeEvent @c @mod
  {-# INLINE beforeSystem #-}

  afterSystem = do
    embedMods $ afterSystem @c @mods
    afterEvent @c @mod
  {-# INLINE afterSystem #-}

  listenToEvents = do
    tailEvents <- embedMods $ listenToEvents @c @mods
    headEvent  <- moduleEvent @c @mod
    return $ UHead <$> headEvent <|> UTail <$> tailEvents
  {-# INLINE listenToEvents #-}

  handleEvents (UHead x) = handleEvent @c @mod x
  handleEvents (UTail xs) = embedMods $ handleEvents @_ @mods xs
  {-# INLINE handleEvents #-}

instance (SubList c mods (mod:mods), SystemModule mod, SystemEnv c mods es, Loadable c mod mods es, LoadableEnv c mod mods es) => SystemEnv c (mod ': mods) es where
  readSystemInitDataFromEnv (im :*** ims) = do
    xs <- readSystemInitDataFromEnv @c @mods ims
    x  <- readInitDataFromEnv @c @mod @mods im
    return $ x :*** xs
  {-# INLINE readSystemInitDataFromEnv #-}

instance (SubList c mods (mod:mods), SystemModule mod, SystemArgs c mods es, Loadable c mod mods es, LoadableArgs c mod mods es) => SystemArgs c (mod ': mods) es where
  readSystemInitDataFromArgs (im :*** ims) args = do
    xs <- readSystemInitDataFromArgs @c @mods ims args
    x  <- readInitDataFromArgs @c @mod @mods im args
    return $ x :*** xs
  {-# INLINE readSystemInitDataFromArgs #-}
