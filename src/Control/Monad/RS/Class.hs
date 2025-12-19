-- This module provides two interface MonadReadable and MonadStateful, unlike MonadReader
-- and MonadState, these interfaces do not have functional dependencies, allowing for
-- simple and extensible state and read effects.
module Control.Monad.RS.Class where

import Control.Monad.Trans
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S

-- | A class for monads that can read a value of type 'r'.
-- without the functional dependencies, so you can read different types of values
-- This ReadOnly has no 'local' method
class Monad m => MonadReadOnly r m where
  -- | Query the monad for a value of type 'r'.
  query :: m r

  -- | Transform the result of a query using a function.
  queries :: (r -> r') -> m r'
  queries f = f <$> query
  {-# INLINE queries #-}

-- | A class for monads that can read a value of type 'r'.
-- without the functional dependencies, so you can read different types of values
class MonadReadOnly r m => MonadReadable r m where
  local :: (r -> r) -> m a -> m a

-- | A class for monads that can maintain a state of type 's'.
-- without the functional dependencies, so you can have different types of states
class Monad m => MonadStateful s m where
  {-# MINIMAL get, put #-}
  -- | Get the current state.
  get :: m s

  gets :: (s -> a) -> m a
  gets f = f <$> get
  {-# INLINE gets #-}

  -- | Set the state to a new value.
  put :: s -> m ()

  -- | Modify the current state using a function.
  modify :: (s -> s) -> m ()
  modify f = do
    s <- get
    put (f s)
  {-# INLINE modify #-}

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadReadOnly r m) => MonadReadOnly r (t m) where
  query = lift query
  {-# INLINE query #-}
instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadStateful s m) => MonadStateful s (t m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
  modify = lift . modify
  {-# INLINE modify #-}

-- | @since 0.2.4.0
instance Monad m => MonadReadOnly r (R.ReaderT r m) where
  query = R.ask
  {-# INLINE query #-}

-- | @since 0.2.4.0
instance Monad m => MonadReadable r (R.ReaderT r m) where
  local = R.local
  {-# INLINE local #-}

-- | @since 0.2.4.0
instance Monad m => MonadStateful s (S.StateT s m) where
  get = S.get
  {-# INLINE get #-}
  put = S.put
  {-# INLINE put #-}
  modify = S.modify
  {-# INLINE modify #-}
