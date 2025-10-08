{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, UndecidableSuperClasses #-}
module Control.System
  (
  -- * Module and System
    Module(..)
  , WithSystem(..)

  , EventLoop(..)
  , EventLoopSystem(..)

  -- * read from module / get state
  , askModule, asksModule
  , queryModule, queriesModule
  , localModule
  , getModule, getsModule
  , putModule, modifyModule

  -- * Loadable
  , Loadable(..)
  , Dependency, Dependency'

  -- * Small utils
  , detectFlag

  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Effect
import Data.Kind
import Data.TypeList
import Data.Text (Text)

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

-- | Run a System of EffT' given initData
--
class Loadable c mod mods ies where
  {-# MINIMAL withModule #-}
  withModule :: ConsFDataList c (mod : mods) => ModuleInitData mod -> EffT' c (mod : mods) ies IO a -> EffT' c mods ies IO a

class EventLoop c mod mods es where
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

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
--
-- the last module in the list is the first to be loaded
-- and also the first to execute beforeEvent and afterEvent
class WithSystem c mods initEs where
  withSystem :: ConsFDataList c mods => SystemInitData c mods -> EffT' c mods initEs IO a -> EffT' c '[] initEs IO a

class EventLoopSystem c mods es where
  listenToEvents :: ConsFDataList c mods => EffT' c mods es IO (STM (SystemEvent mods))

  handleEvents :: ConsFDataList c mods => SystemEvent mods -> EffT' c mods es IO ()

  beforeSystem :: ConsFDataList c mods => EffT' c mods es IO ()

  afterSystem  :: ConsFDataList c mods => EffT' c mods es IO ()

-- | base case for system
instance WithSystem c '[] ies where
  withSystem _ = id
  -- return (fNil, fNil)
  {-# INLINE withSystem #-}

instance EventLoopSystem c '[] es where
  listenToEvents = return empty
  {-# INLINE listenToEvents #-}

  handleEvents _ = return ()
  {-# INLINE handleEvents #-}

  beforeSystem = return ()
  {-# INLINE beforeSystem #-}

  afterSystem = return ()
  {-# INLINE afterSystem #-}

-- | Inductive instance for system
instance (SystemModule mod, WithSystem c mods ies, Loadable c mod mods ies) => WithSystem c (mod ': mods) ies where
  withSystem (x :*** xs) = withSystem @c @mods @ies xs . withModule @c @mod @mods @ies x
  {-# INLINE withSystem #-}

instance (SystemModule mod, EventLoop c mod mods es, EventLoopSystem c mods es) => EventLoopSystem c (mod ': mods) es where
  beforeSystem = do
    embedMods $ beforeSystem @c @mods @es
    beforeEvent @c @mod @mods @es
  {-# INLINE beforeSystem #-}

  afterSystem = do
    embedMods $ afterSystem @c @mods @es
    afterEvent @c @mod @mods @es
  {-# INLINE afterSystem #-}

  listenToEvents = do
    tailEvents <- embedMods $ listenToEvents @c @mods @es
    headEvent  <- moduleEvent @c @mod @mods @es
    return $ UHead <$> headEvent <|> UTail <$> tailEvents
  {-# INLINE listenToEvents #-}

  handleEvents (UHead x) = handleEvent @c @mod @mods @es x
  handleEvents (UTail xs) = embedMods $ handleEvents @_ @mods @es xs
  {-# INLINE handleEvents #-}

detectFlag :: String -> (String -> Either Text a) -> [String] -> Maybe (Either Text a)
detectFlag _ _ [] = Nothing
detectFlag flag parser list = go list
  where
    go (x:y:xs) | x == flag = Just (parser y)
                | otherwise = go (y:xs)
    go _ = Nothing
