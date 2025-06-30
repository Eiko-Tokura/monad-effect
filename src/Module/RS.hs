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

embedStateT :: forall s mods errs a. ((SModule s) `In` mods) => S.StateT s (Eff mods errs) a -> Eff mods errs a
embedStateT action = do
  SState s <- getModule @(SModule s)
  (a, s') <- S.runStateT action s
  putModule @(SModule s) (SState s')
  return a
{-# INLINE embedStateT #-}

addStateT :: forall s mods errs a.
  ( mods `SubList` (SModule s : mods)
  , errs `SubList` errs
  , NotIn SystemError errs
  , (SModule s) `NotIn` mods
  )
  => S.StateT s (Eff mods errs) a -> Eff (SModule s : mods) errs a
addStateT action = do
  SState s <- getModule @(SModule s)
  (a, s') <- embedEff $ S.runStateT action s
  putModule (SState s')
  return a
{-# INLINE addStateT #-}

runRModule :: r -> Eff (RModule r : mods) errs a -> Eff mods errs a
runRModule r = runEffOuter_ RRead (RState r)
{-# INLINE runRModule #-}

-- | Warning: state will lose when you have an error
runSModule :: s -> Eff (SModule s : mods) errs a -> Eff mods errs (s, a)
runSModule s
  = fmap (first $ \(SState s') -> s')
  . runEffOuter SRead (SState s)

runSModule_ :: s -> Eff (SModule s : mods) errs a -> Eff mods errs a
runSModule_ s = runEffOuter_ SRead (SState s)
{-# INLINE runSModule_ #-}

askR :: forall r mods errs. ((RModule r) `In` mods) => Eff mods errs r
askR = do
  RState r <- getModule @(RModule r)
  return r
{-# INLINE askR #-}

asksR :: forall r mods errs a. ((RModule r) `In` mods) => (r -> a) -> Eff mods errs a
asksR f = do
  RState r <- getModule @(RModule r)
  return (f r)
{-# INLINE asksR #-}

localR :: forall r mods errs a. ((RModule r) `In` mods) => (r -> r) -> Eff mods errs a -> Eff mods errs a
localR f action = do
  RState r <- getModule @(RModule r)
  let r' = f r
  putModule @(RModule r) (RState r')
  result <- action
  putModule @(RModule r) (RState r)
  return result
{-# INLINE localR #-}

getS :: forall s mods errs. ((SModule s) `In` mods) => Eff mods errs s
getS = do
  SState s <- getModule @(SModule s)
  return s
{-# INLINE getS #-}

getsS :: forall s mods errs a. ((SModule s) `In` mods) => (s -> a) -> Eff mods errs a
getsS f = do
  SState s <- getModule @(SModule s)
  return (f s)
{-# INLINE getsS #-}

putS :: forall s mods errs. ((SModule s) `In` mods) => s -> Eff mods errs ()
putS s = do
  SState _ <- getModule @(SModule s)
  putModule @(SModule s) (SState s)
{-# INLINE putS #-}

modifyS :: forall s mods errs. ((SModule s) `In` mods) => (s -> s) -> Eff mods errs ()
modifyS f = do
  s <- getS
  putS (f s)
{-# INLINE modifyS #-}
