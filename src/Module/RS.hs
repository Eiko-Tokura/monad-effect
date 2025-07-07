-- | This module defines two modules (unit of effect) that provides reader and state functionality.
--
-- it can be used in the EffT monad transformer
module Module.RS where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Control.Monad.Effect
import Data.Kind
import Data.TypeList
import Data.Bifunctor (second)
import GHC.TypeLits
import qualified Control.Monad.State as S

-- | A module that provides reader functionality
data RModule (r :: Type)

instance Module (RModule r) where
  newtype ModuleInitData (RModule r) = RInitData { rInitRead :: r }
  newtype ModuleRead  (RModule r)    = RRead { rRead :: r }
  data    ModuleState (RModule r)    = RState deriving (Generic, NFData)
  data    ModuleEvent (RModule r)    = REvent

-- | A reader module that has a name
data RNamed (name :: Symbol) (r :: Type)

instance Module (RNamed name r) where
  newtype ModuleInitData (RNamed name r) = RNamedInitData { rNamedInitRead :: r }
  newtype ModuleRead  (RNamed name r)    = RNamedRead { rNamedRead :: r }
  data    ModuleState (RNamed name r)    = RNamedState deriving (Generic, NFData)
  data    ModuleEvent (RNamed name r)    = RNamedEvent

-- | A module that provides state functionality
data SModule (s :: Type)

instance Module (SModule s) where
  newtype ModuleInitData (SModule s) = SInitData { sInitState :: s }
  data    ModuleRead  (SModule s)    = SRead
  newtype ModuleState (SModule s)    = SState { sState :: s } deriving newtype (Generic, NFData)
  data    ModuleEvent (SModule s)    = SEvent

-- | A state module that has a name
data SNamed (name :: Symbol) (s :: Type)
instance Module (SNamed name s) where
  newtype ModuleInitData (SNamed name s) = SNamedInitData { sNamedInitState :: s }
  data    ModuleRead  (SNamed name s)    = SNamedRead
  newtype ModuleState (SNamed name s)    = SNamedState { sNamedState :: s } deriving newtype (Generic, NFData)
  data    ModuleEvent (SNamed name s)    = SNamedEvent

embedStateT :: forall s mods errs m c a. (Monad m, In' c (SModule s) mods) => S.StateT s (EffT' c mods errs m) a -> EffT' c mods errs m a
embedStateT action = do
  SState s <- getModule @(SModule s)
  (a, s') <- S.runStateT action s
  putModule @(SModule s) (SState s')
  return a
{-# INLINE embedStateT #-}

addStateT :: forall s mods errs m c a.
  ( SubList c mods (SModule s : mods)
  , (SModule s) `NotIn` mods
  , In' c (SModule s) (SModule s : mods)
  , Monad m
  )
  => S.StateT s (EffT' c mods errs m) a -> EffT' c (SModule s : mods) errs m a
addStateT action = do
  SState s <- getModule @(SModule s)
  (a, s') <- embedEffT $ S.runStateT action s
  putModule (SState s')
  return a
{-# INLINE addStateT #-}

asStateT :: forall s mods errs m c a.
  ( In' c (SModule s) mods
  , SModule s `UniqueIn` mods
  , SubList c (Remove (FirstIndex (SModule s) mods) mods) mods
  , Monad m
  )
  => S.StateT s (EffT' c (Remove (FirstIndex (SModule s) mods) mods) errs m) a -> EffT' c mods errs m a
asStateT action = do
  SState s <- getModule
  (a, s') <- embedEffT $ S.runStateT action s
  putModule (SState s')
  return a
{-# INLINE asStateT #-}

runRModule :: (ConsFDataList c (RModule r : mods), Monad m) => r -> EffT' c (RModule r : mods) errs m a -> EffT' c mods errs m a
runRModule r = runEffTOuter_ (RRead r) RState
{-# INLINE runRModule #-}

runRModuleIn :: (ConsFDataList c mods, RemoveElem c mods, Monad m, In' c (RModule r) mods) => r -> EffT' c mods es m a -> EffT' c (Remove (FirstIndex (RModule r) mods) mods) es m a
runRModuleIn r = runEffTIn_ (RRead r) RState
{-# INLINE runRModuleIn #-}

-- | Warning: state will lose when you have an error
runSModule :: (ConsFDataList c (SModule s : mods), Monad m) => s -> EffT' c (SModule s : mods) errs m a -> EffT' c mods errs m (a, s)
runSModule s
  = fmap (second $ \(SState s') -> s')
  . runEffTOuter SRead (SState s)
{-# INLINE runSModule #-}

runSModuleIn :: (ConsFDataList c mods, RemoveElem c mods, Monad m, In' c (SModule s) mods) => s -> EffT' c mods es m a -> EffT' c (Remove (FirstIndex (SModule s) mods) mods) es m (a, s)
runSModuleIn s
  = fmap (second (\(SState s') -> s'))
  . runEffTIn SRead (SState s)
{-# INLINE runSModuleIn #-}

runSModule_ :: (ConsFDataList c (SModule s : mods), Monad m) => s -> EffT' c (SModule s : mods) errs m a -> EffT' c mods errs m a
runSModule_ s = runEffTOuter_ SRead (SState s)
{-# INLINE runSModule_ #-}

askR :: forall r mods errs m c. (Monad m) => (In' c (RModule r) mods) => EffT' c mods errs m r
askR = do
  RRead r <- askModule @(RModule r)
  return r
{-# INLINE askR #-}

asksR :: forall r mods errs m c a. (Monad m) => (In' c (RModule r) mods) => (r -> a) -> EffT' c mods errs m a
asksR f = do
  RRead r <- askModule @(RModule r)
  return (f r)
{-# INLINE asksR #-}

localR :: forall r mods errs m c a. (Monad m, In' c (RModule r) mods) => (r -> r) -> EffT' c mods errs m a -> EffT' c mods errs m a
localR f action = localModule (\(RRead r) -> RRead (f r)) $ action
{-# INLINE localR #-}

getS :: forall s mods errs m c. (Monad m, In' c (SModule s) mods) => EffT' c mods errs m s
getS = do
  SState s <- getModule @(SModule s)
  return s
{-# INLINE getS #-}

getsS :: forall s mods errs m c a. (Monad m, In' c (SModule s) mods) => (s -> a) -> EffT' c mods errs m a
getsS f = do
  SState s <- getModule @(SModule s)
  return (f s)
{-# INLINE getsS #-}

putS :: forall s mods errs c m. (Monad m, In' c (SModule s) mods) => s -> EffT' c mods errs m ()
putS s = do
  SState _ <- getModule @(SModule s)
  putModule @(SModule s) (SState s)
{-# INLINE putS #-}

modifyS :: forall s mods errs c m. (Monad m, In' c (SModule s) mods) => (s -> s) -> EffT' c mods errs m ()
modifyS f = do
  s <- getS
  putS (f s)
{-# INLINE modifyS #-}
