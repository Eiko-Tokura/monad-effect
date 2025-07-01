module Module.RS where

import Control.Monad.Effect
import Data.Kind
import Data.HList ( In, NotIn, SubList )
import Data.Bifunctor (first)
import qualified Control.Monad.State as S

-- | A module that provides reader functionality
data RModule (r :: Type)

instance Module (RModule r) where
  newtype ModuleInitData (RModule r) = RInitData { rInitRead :: r }
  data    ModuleRead  (RModule r)    = RRead
  newtype ModuleState (RModule r)    = RState { rState :: r }
  data    ModuleEvent (RModule r)    = REvent

-- | A module that provides state functionality
data SModule (s :: Type)

instance Module (SModule s) where
  newtype ModuleInitData (SModule s) = SInitData { sInitState :: s }
  data    ModuleRead  (SModule s)    = SRead
  newtype ModuleState (SModule s)    = SState { sState :: s }
  data    ModuleEvent (SModule s)    = SEvent

embedStateT :: forall s mods errs m a. (Monad m, (SModule s) `In` mods) => S.StateT s (EffT mods errs m) a -> EffT mods errs m a
embedStateT action = do
  SState s <- getModule @(SModule s)
  (a, s') <- S.runStateT action s
  putModule @(SModule s) (SState s')
  return a
{-# INLINE embedStateT #-}

addStateT :: forall s mods errs m a.
  ( mods `SubList` (SModule s : mods)
  , errs `SubList` errs
  , NotIn SystemError errs
  , (SModule s) `NotIn` mods
  , Monad m
  )
  => S.StateT s (EffT mods errs m) a -> EffT (SModule s : mods) errs m a
addStateT action = do
  SState s <- getModule @(SModule s)
  (a, s') <- embedEffT $ S.runStateT action s
  putModule (SState s')
  return a
{-# INLINE addStateT #-}

runRModule :: Monad m => r -> EffT (RModule r : mods) errs m a -> EffT mods errs m a
runRModule r = runEffTOuter_ RRead (RState r)
{-# INLINE runRModule #-}

-- | Warning: state will lose when you have an error
runSModule :: Monad m => s -> EffT (SModule s : mods) errs m a -> EffT mods errs m (s, a)
runSModule s
  = fmap (first $ \(SState s') -> s')
  . runEffTOuter SRead (SState s)
{-# INLINE runSModule #-}

runSModule_ :: Monad m => s -> EffT (SModule s : mods) errs m a -> EffT mods errs m a
runSModule_ s = runEffTOuter_ SRead (SState s)
{-# INLINE runSModule_ #-}

askR :: forall r mods errs m. Monad m => ((RModule r) `In` mods) => EffT mods errs m r
askR = do
  RState r <- getModule @(RModule r)
  return r
{-# INLINE askR #-}

asksR :: forall r mods errs m a. Monad m => ((RModule r) `In` mods) => (r -> a) -> EffT mods errs m a
asksR f = do
  RState r <- getModule @(RModule r)
  return (f r)
{-# INLINE asksR #-}

localR :: forall r mods errs m a. (Monad m, (RModule r) `In` mods) => (r -> r) -> EffT mods errs m a -> EffT mods errs m a
localR f action = do
  RState r <- getModule @(RModule r)
  let r' = f r
  putModule @(RModule r) (RState r')
  result <- action
  putModule @(RModule r) (RState r)
  return result
{-# INLINE localR #-}

getS :: forall s mods errs m. (Monad m, (SModule s) `In` mods) => EffT mods errs m s
getS = do
  SState s <- getModule @(SModule s)
  return s
{-# INLINE getS #-}

getsS :: forall s mods errs m a. (Monad m, (SModule s) `In` mods) => (s -> a) -> EffT mods errs m a
getsS f = do
  SState s <- getModule @(SModule s)
  return (f s)
{-# INLINE getsS #-}

putS :: forall s mods errs m. (Monad m, (SModule s) `In` mods) => s -> EffT mods errs m ()
putS s = do
  SState _ <- getModule @(SModule s)
  putModule @(SModule s) (SState s)
{-# INLINE putS #-}

modifyS :: forall s mods errs m. (Monad m, (SModule s) `In` mods) => (s -> s) -> EffT mods errs m ()
modifyS f = do
  s <- getS
  putS (f s)
{-# INLINE modifyS #-}
