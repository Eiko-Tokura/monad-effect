module Module.RS where

import Control.Monad.Effect
import Data.Kind
import Data.HList ( In )

-- data RModule (r :: Type) (s :: Type)
-- 
-- instance Module (RSModule r s) where
--   data ModuleInitData (RSModule r s) = RSInitData { rsInitRead :: r, rsInitState :: s }
--   data ModuleRead  (RSModule r s)    = RSRead  { rsRead :: r }
--   data ModuleState (RSModule r s)    = RSState { rsState :: s }
--   data ModuleEvent (RSModule r s)    = RSEvent

data RModule (r :: Type)

instance Module (RModule r) where
  data ModuleInitData (RModule r) = RInitData { rInitRead :: r }
  data ModuleRead  (RModule r)    = RRead
  data ModuleState (RModule r)    = RState { rState :: r }
  data ModuleEvent (RModule r)    = REvent

data SModule (s :: Type)

instance Module (SModule s) where
  data ModuleInitData (SModule s) = SInitData { sInitState :: s }
  data ModuleRead  (SModule s)    = SRead
  data ModuleState (SModule s)    = SState { sState :: s }
  data ModuleEvent (SModule s)    = SEvent

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
  s <- get @s
  put (f s)
{-# INLINE modify #-}

