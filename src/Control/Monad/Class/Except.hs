module Control.Monad.Class.Except where

import Control.Exception as E
import Control.Monad.Except
import Data.Kind (Type)
import Data.Proxy
import Data.String (IsString)
import Data.Text (Text, unpack, pack)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Control.Monad
import Control.Monad.Base

-- | a newtype wrapper ErrorText that wraps a Text with a name (for example Symbol type)
-- useful for creating ad-hoc error type
--
-- @
-- ErrorText @_ @"FileNotFound" "information : file not found"
-- @
newtype ErrorText (s :: k) = ErrorText Text
  deriving newtype (IsString)

-- | Can be used to construct an ErrorText value, use type application to give the name
--
-- @
-- errorText @"FileNotFound" "file not found"
-- @
errorText :: forall s. Text -> ErrorText s
errorText = ErrorText
{-# INLINE errorText #-}

-- | a newtype wrapper ErrorValue that wraps a custom value type v with a name (for example Symbol type)
-- useful for creating ad-hoc error type
newtype ErrorValue (a :: k) (v :: Type) = ErrorValue v

-- | Can be used to construct an ErrorValue value, use type application to give the name
errorValue :: forall s v. v -> ErrorValue s v
errorValue = ErrorValue
{-# INLINE errorValue #-}

-- | A wrapper dedicated for errors living in MonadThrow and MonadCatch
newtype MonadThrowError = MonadThrowError SomeException
  deriving Show

instance KnownSymbol s => Show (ErrorText s) where
  show (ErrorText t) = "ErrorText of type " ++ symbolVal (Proxy @s) ++ ": " ++ unpack t

instance (KnownSymbol s, Show v) => Show (ErrorValue s v) where
  show (ErrorValue v) = "ErrorValue of type " <> symbolVal (Proxy @s) <> ": " <> show v

-- | @since 0.2.2.0
instance KnownSymbol s                       => Exception (ErrorText s)

-- | @since 0.2.2.0
instance (KnownSymbol s, Typeable v, Show v) => Exception (ErrorValue s v)

-- | Similar to MonadError, but with out the functional dependency so a monad can throw multiple exceptions.
--
-- @since 0.2.2.0
class Monad m => MonadExcept e m where
  throwExcept :: e -> m a

-- | @since 0.2.2.0
instance Exception e => MonadExcept e IO where
  throwExcept = throwIO

-- | @since 0.2.2.0
instance MonadExcept e (Either e) where
  throwExcept = Left

-- | This instance allows throwing named textual errors in the Either monad.
--
-- @since 0.2.2.0
instance KnownSymbol s => MonadExcept (ErrorText s) (Either (Text, Text)) where
  throwExcept (ErrorText t) = Left (pack $ symbolVal (Proxy @s), t)

-- | @since 0.2.2.0
instance Monad m => MonadExcept e (ExceptT e m) where
  throwExcept = throwError

-- | This instance allows throwing named textual errors in the ExceptT monad transformer.
--
-- @since 0.2.2.0
instance (Monad m, KnownSymbol s) => MonadExcept (ErrorText s) (ExceptT (Text, Text) m) where
  throwExcept (ErrorText t) = throwError (pack $ symbolVal (Proxy @s), t)

------------ Some convertion functions ------------

-- | Lift from ExceptT to any MonadExcept (e.g. EffT)
--
-- @since 0.2.3.0
liftExceptT :: (MonadBase n m, MonadExcept e m) => ExceptT e n a -> m a
liftExceptT = either throwExcept pure <=< liftBase . runExceptT
{-# INLINE liftExceptT #-}
