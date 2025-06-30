{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
-- | This module provides a monadic structure for handling state and
-- reading from a shared environment. It defines the 'RS' and 'RST'
-- types, which allow for a combination of stateful computations and
-- read operations in a flexible manner.
--
-- It also provides two interface MonadReadable and MonadStateful for
-- simple and extensible state and read effects.
module Control.Monad.RST
  ( RS(..), RST(..), runRST
  , MonadReadable(..), MonadStateful(..)
  , getModify, modifyGet
  , module Control.Monad.Trans
  , module Control.Monad.IO.Class
  , getsE, putsE, queriesE, Elem(..), HList(..)
  , embedRST, getAll, queryAll
  , fillR, fillS, fillS'

  -- * Error handling capable
  , RSE(..), RSET(..), runRSET
  , embedRSET, catchE, catchAll, throwE
  ) where

import Data.Bifunctor
import Data.HList
import Control.Monad.Trans
import Control.Monad.IO.Class

-- | A newtype representing a computation that reads from an environment
-- of type 'r' and maintains a state of type 's', producing a result of
-- type 'a' in a monadic context 'm'.
newtype RS  r s m a = RS  { runRS  :: r -> s -> m (a, s) }

-- | A newtype representing a computation that reads from an environment
-- of type 'r' and maintains a state of type 's', producing a result of
-- type 'a' in a monadic context 'm', with the possibility of an error
-- of type 'e'.
newtype RSE r s e m a = RSE { runRSE :: r -> s -> m (Either e a, s) }

-- | A newtype representing a stateful computation that uses a heterogeneous
-- list of read values of type 'HList r' and a heterogeneous list of state
-- values of type 'HList s', producing a result of type 'a' in a monadic
-- context 'm'.
newtype RST r s m a = RST { unRST :: RS (HList r) (HList s) m a }
  deriving ( Functor, Applicative, Monad ) via (RS (HList r) (HList s) m)

newtype RSET r s e m a = RSET { unRSET :: RSE (HList r) (HList s) (SList e) m a }
  deriving ( Functor, Applicative, Monad ) via (RSE (HList r) (HList s) (SList e) m)

-- | Deriving instance for 'MonadIO' to allow lifting of IO actions
-- into the 'RST' monad.
deriving instance MonadIO m => MonadIO (RST r s m)
deriving instance MonadIO m => MonadIO (RSET r s e m)

-- | Runs a 'RST' computation with the given environment and state,
-- returning the result and the updated state.
runRST :: RST r s m a -> HList r -> HList s -> m (a, HList s)
runRST = runRS . unRST
{-# INLINE runRST #-}

-- | Runs a 'RSET' computation with the given environment and state,
runRSET :: RSET r s e m a -> HList r -> HList s -> m (Either (SList e) a, HList s)
runRSET = runRSE . unRSET
{-# INLINE runRSET #-}

-- | Functor instance for 'RS', allowing the application of a function
-- to the result of the computation.
instance Functor m => Functor (RS r s m) where
  fmap f (RS g) = RS $ \r s -> fmap (\(a, s') -> (f a, s')) (g r s)
  {-# INLINE fmap #-}

instance Functor m => Functor (RSE r s e m) where
  fmap f (RSE g) = RSE $ \r s -> fmap (first (fmap f)) (g r s)
  {-# INLINE fmap #-}

-- | Applicative instance for 'RS', allowing the use of pure values
-- and the application of functions within the context.
instance Monad m => Applicative (RS r s m) where
  pure a = RS $ \_ s -> return (a, s)
  {-# INLINE pure #-}
  RS f <*> RS g = RS $ \r s -> do
    (a, s')  <- f r s
    (b, s'') <- g r s'
    return (a b, s'')
  {-# INLINE (<*>) #-}

instance Monad m => Applicative (RSE r s e m) where
  pure a = RSE $ \_ s -> return (Right a, s)
  {-# INLINE pure #-}
  RSE f <*> RSE g = RSE $ \r s -> do
    (ea, s')  <- f r s
    (eb, s'') <- g r s'
    return (ea <*> eb, s'')
  {-# INLINE (<*>) #-}

-- | Monad instance for 'RS', allowing for sequential composition of
-- computations.
instance Monad m => Monad (RS r s m) where
  return = pure
  {-# INLINE return #-}
  RS f >>= g = RS $ \r s -> do
    (a, s') <- f r s
    runRS (g a) r s'
  {-# INLINE (>>=) #-}

instance Monad m => Monad (RSE r s e m) where
  return = pure
  {-# INLINE return #-}
  RSE f >>= g = RSE $ \r s -> do
    (ea, s') <- f r s
    case ea of
      Left e  -> return (Left e, s') -- short-circuit on error
      Right a -> runRSE (g a) r s'
  {-# INLINE (>>=) #-}

-- | MonadIO instance for 'RS', allowing IO actions to be lifted
-- into the 'RS' monad.
instance MonadIO m => MonadIO (RS r s m) where
  liftIO m = RS $ \_ s -> do
    a <- liftIO m
    return (a, s)
  {-# INLINE liftIO #-}

-- | MonadIO instance for 'RSE', allowing IO actions to be lifted
instance MonadIO m => MonadIO (RSE r s e m) where
  liftIO m = RSE $ \_ s -> do
    a <- liftIO m
    return (Right a, s)
  {-# INLINE liftIO #-}

-- | Monad transformer instance for 'RS', allowing the lifting of
-- monadic actions into the 'RS' context.
instance MonadTrans (RS r s) where
  lift m = RS $ \_ s -> do
    a <- m
    return (a, s)
  {-# INLINE lift #-}

instance MonadTrans (RSE r s e) where
  lift m = RSE $ \_ s -> do
    a <- m
    return (Right a, s)
  {-# INLINE lift #-}

--------------------------------------------------------------------------------

-- | A class for monads that can read a value of type 'r'.
-- without the functional dependencies, so you can read different types of values
class Monad m => MonadReadable r m where
  {-# MINIMAL query, local #-}
  -- | Query the monad for a value of type 'r'.
  query :: m r

  -- | Transform the result of a query using a function.
  queries :: (r -> r') -> m r'
  queries f = f <$> query
  {-# INLINE queries #-}

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

--------------------------------------------------------------------------------

instance Monad m => MonadReadable r (RSE r s e m) where
  query = RSE $ \r s -> return (Right r, s)
  {-# INLINE query #-}
  local f (RSE g) = RSE $ \r s -> g (f r) s
  {-# INLINE local #-}

instance Monad m => MonadStateful s (RSE r s e m) where
  get = RSE $ \_ s ->   return (Right s, s)
  {-# INLINE get #-}
  put s = RSE $ \_ _ -> return (Right (), s)
  {-# INLINE put #-}

-- | Instance of 'MonadReadable' for 'RST', allowing queries to be made
-- on the read environment.
instance (In a hr, Monad m) => MonadReadable a (RST hr hs m) where
  query = RST $ RS $ \hr hs -> return (getH hr, hs)
  {-# INLINE query #-}
  local f (RST (RS g)) = RST $ RS $ \hr hs -> g (modifyH f hr) hs >>= \(a, hs') -> return (a, hs')
  {-# INLINE local #-}

-- | Instance of 'MonadStateful' for 'RST', allowing state management
-- within the monad.
instance (In s hs, Monad m) => MonadStateful s (RST hr hs m) where
  get   = RST $ RS $ \_ hs -> return (getH hs, hs)
  {-# INLINE get #-}
  put s = RST $ RS $ \_ hs -> return ((), modifyH (const s) hs)
  {-# INLINE put #-}

-- | Get the current state, modify it using a function, and return the
-- original state.
getModify :: (In s hs, Monad m) => (s -> s) -> RST hr hs m s
getModify f = do
  s <- get
  put (f s)
  return s
{-# INLINE getModify #-}

-- | Get the current state, modify it using a function, and return the
-- modified state.
modifyGet :: (In s hs, Monad m) => (s -> s) -> RST hr hs m s
modifyGet f = do
  s <- get
  put (f s)
  return (f s)
{-# INLINE modifyGet #-}

-- | Embed a smaller 'RST' computation into a larger one, allowing for
-- a subset of the read and state environments to be used.
embedRST :: (Monad m, SubList r' r, SubList s' s) => RST r' s' m a -> RST r s m a
embedRST = RST . RS
  . (\mrs' hr hs -> do
        (a, s') <- mrs' (getSubList hr) (getSubList hs)
        return (a, subListUpdate hs s')
    )
  . runRS . unRST
{-# INLINE embedRST #-}

embedRSET :: (Monad m, SubList r' r, SubList s' s, SubList e' e) => RSET r' s' e' m a -> RSET r s e m a
embedRSET = RSET . RSE
  . (\mrs' hr hs -> do
        (a, s') <- mrs' (getSubList hr) (getSubList hs)
        return (first subListEmbed a, subListUpdate hs s')
    )
  . runRSE . unRSET
{-# INLINE embedRSET #-}

-- | Get all state values as a heterogeneous list.
getAll :: Monad m => RST hr hs m (HList hs)
getAll = RST $ RS $ \_ hs -> return (hs, hs)
{-# INLINE getAll #-}

-- | Query all read values as a heterogeneous list.
queryAll :: Monad m => RST hr hs m (HList hr)
queryAll = RST $ RS $ curry return
{-# INLINE queryAll #-}

-- | Get a value from the state using an 'Elem' reference.
getsE :: Monad m => Elem e hs -> RST hr hs m e
getsE e = RST $ RS $ \_ hs -> return (getE e hs, hs)
{-# INLINE getsE #-}

-- | Set a value in the state using an 'Elem' reference.
putsE :: Monad m => Elem e hs -> e -> RST hr hs m ()
putsE e v = RST $ RS $ \_ hs -> return ((), putE e v hs)
{-# INLINE putsE #-}

-- | Query a value from the read environment using an 'Elem' reference.
queriesE :: Monad m => Elem e hr -> RST hr hs m e
queriesE e = RST $ RS $ \hr hs -> return (getE e hr, hs)
{-# INLINE queriesE #-}

--------------------------------------------------------------------------------
-- Filling and reduce

-- | Fill the reader environment with a value, reducing one element from
-- the read environment.
fillR :: r0 -> RST (r0 ': rs) s m a -> RST rs s m a
fillR r0 (RST (RS f)) = RST $ RS $ \rs s -> f (r0 :* rs) s
{-# INLINE fillR #-}

-- | Fill the state with a value, reducing one element from the state,
-- and return an extra state.
fillS :: (Functor m) => s0 -> RST r (s0 ': ss) m a -> RST r ss m (a, s0)
fillS s0 (RST (RS f)) = RST $ RS $ \r ss -> (\(a, b0 :* bs) -> ((a, b0), bs)) <$> f r (s0 :* ss)
{-# INLINE fillS #-}

-- | Fill the state with a value, reducing one element from the state,
-- and discard the extra state.
fillS' :: (Functor m) => s0 -> RST r (s0 ': ss) m a -> RST r ss m a
fillS' s0 = fmap fst . fillS s0
{-# INLINE fillS' #-}

-- | This function elliminates the error type from the RSET monad by catching!
catchE :: forall e es r s m a. Monad m => RSET r s (e ': es) m a -> (e -> RSET r s es m a) -> RSET r s es m a
catchE (RSET m) h = RSET $ RSE $ \r s -> do
  (ea, s') <- runRSE m r s
  case ea of
    Left (SHead e)  -> runRSET (h e) r s'
    Left (STail es) -> return (Left es, s')
    Left SEmpty     -> return (Left SEmpty, s') -- error without type and information, should not happen
    Right a         -> return (Right a, s')
{-# INLINE catchE #-}

-- | This function elliminates ALL the error type from the RSET monad by catching
-- (and pattern matching on error type)!
catchAll :: Monad m => RSET r s (e ': es) m a -> (SList (e ': es) -> RSET r s '[] m a) -> RSET r s '[] m a
catchAll (RSET m) h = RSET $ RSE $ \r s -> do
  (ea, s') <- runRSE m r s
  case ea of
    Left es -> runRSET (h es) r s'
    Right a -> return (Right a, s')
{-# INLINE catchAll #-}

throwE :: Applicative m => In e es => e -> RSET r s es m a
throwE e = RSET $ RSE $ \_ s -> pure (Left (embedS e), s)
{-# INLINE throwE #-}
