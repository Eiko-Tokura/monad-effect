module Module.RS where

import Control.Monad.Effect
import Data.Kind
import Data.HList ( In )
import Data.Bifunctor (first)

data RModule (r :: Type)

instance Module (RModule r) where
  newtype ModuleInitData (RModule r) = RInitData { rInitRead :: r }
  data    ModuleRead  (RModule r)    = RRead
  newtype ModuleState (RModule r)    = RState { rState :: r }
  data    ModuleEvent (RModule r)    = REvent

data SModule (s :: Type)

instance Module (SModule s) where
  newtype ModuleInitData (SModule s) = SInitData { sInitState :: s }
  data    ModuleRead  (SModule s)    = SRead
  newtype ModuleState (SModule s)    = SState { sState :: s }
  data    ModuleEvent (SModule s)    = SEvent

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

ask :: forall r mods errs. ((RModule r) `In` mods) => Eff mods errs r
ask = do
  RState r <- getModule @(RModule r)
  return r
{-# INLINE ask #-}

asks :: forall r mods errs a. ((RModule r) `In` mods) => (r -> a) -> Eff mods errs a
asks f = do
  RState r <- getModule @(RModule r)
  return (f r)
{-# INLINE asks #-}

local :: forall r mods errs a. ((RModule r) `In` mods) => (r -> r) -> Eff mods errs a -> Eff mods errs a
local f action = do
  RState r <- getModule @(RModule r)
  let r' = f r
  putModule @(RModule r) (RState r')
  result <- action
  putModule @(RModule r) (RState r)
  return result
{-# INLINE local #-}

get :: forall s mods errs. ((SModule s) `In` mods) => Eff mods errs s
get = do
  SState s <- getModule @(SModule s)
  return s
{-# INLINE get #-}

gets :: forall s mods errs a. ((SModule s) `In` mods) => (s -> a) -> Eff mods errs a
gets f = do
  SState s <- getModule @(SModule s)
  return (f s)
{-# INLINE gets #-}

put :: forall s mods errs. ((SModule s) `In` mods) => s -> Eff mods errs ()
put s = do
  SState _ <- getModule @(SModule s)
  putModule @(SModule s) (SState s)
{-# INLINE put #-}

modify :: forall s mods errs. ((SModule s) `In` mods) => (s -> s) -> Eff mods errs ()
modify f = do
  s <- get
  put (f s)
{-# INLINE modify #-}
