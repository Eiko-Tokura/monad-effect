module Control.Monad.Class.Except where

import Control.Exception
import Control.Monad.Except

-- | Similar to MonadError, but with out the functional dependency so a monad can throw multiple exceptions.
--
-- @since 0.2.2.0
class Monad m => MonadExcept e m where
  throwExcept :: e -> m a

instance Exception e => MonadExcept e IO where
  throwExcept = throwIO

instance MonadExcept e (Either e) where
  throwExcept = Left

instance Monad m => MonadExcept e (ExceptT e m) where
  throwExcept = throwError
