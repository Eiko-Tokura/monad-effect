{-# LANGUAGE DerivingVia, AllowAmbiguousTypes #-}
-- | The module you should import to use for effectful computation
module Control.Monad.Effect
  ( -- * EffTectful computation
    EffT', Eff, Pure, EffT, EffL, PureL, EffLT
  , ErrorText(..), ErrorValue(..), MonadThrowError(..)
  , embedEffT, embedMods, embedError
  , runEffT, runEffT_, runEffT0, runEffT01, runEffT00
  , runEffTNoError
  , runEffTOuter, runEffTOuter', runEffTOuter_
  , runEffTIn, runEffIn', runEffTIn_
  , effCatch, effCatchAll, effCatchSystem
  , effCatchIn, effCatchIn'
  , effThrow, effThrowIn
  , effTryIO, effTryIOIn, effTryIOWith, effTryIOInWith
  , effEither, effEitherWith
  , effEitherIn, effEitherInWith
  , effMaybeWith, effMaybeInWith
  , pureMaybeInWith, pureEitherInWith
  , baseEitherIn, baseEitherInWith, baseMaybeInWith
  , errorToEither, errorToEitherAll, eitherAllToEffect
  , liftIOException, liftIOAt, liftIOSafeWith, liftIOText, liftIOPrepend
  , effEitherSystemException

  -- , proofEmbedEffT
  , declareNoError
  , checkNoError

  , SystemError(..), NoError
  , SystemState, SystemRead, SystemEvent, SystemInitData

  -- * Modules
  , Module(..), SystemModule(..)
  , queryModule, queriesModule, askModule, asksModule
  , localModule
  , getModule, getsModule
  , putModule, modifyModule

  -- * Re-exports
  , MonadIO(..)
  , ConsFDataList, FList, FData
  , Identity(..)
  , InList, In'
  , In, InL
  ) where

import Control.Exception as E hiding (TypeError)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RST
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.String (IsString)
import Data.Text (Text, unpack, pack)
import Data.Type.Equality
import Data.TypeList
import Data.TypeList.FData
import GHC.TypeError
import GHC.TypeLits

-- | EffTectful computation, using modules as units of effect
-- the tick is used to indicate the polymorphic type c which is the data structure used to store the modules.
--
-- The idea is that most user should just use EffT or Eff by default, and does not need this polymorphic type c.
newtype EffT' (c :: (Type -> Type) -> [Type] -> Type) (mods :: [Type]) (es :: [Type]) m a
  = EffT' { unEffT' :: SystemRead c mods -> SystemState c mods -> m (Result es a, SystemState c mods) }

-- | Short hand monads, recommended, uses FData under the hood
type Eff   mods es  = EffT' FData mods es IO
type EffT  mods es  = EffT' FData mods es
type Pure  mods es  = EffT' FData mods es Identity
type In    mods es  = In'   FData mods es

-- | Short hand monads which uses FList instead of FData as the data structure
type EffL  mods es = EffT' FList mods es IO
type EffLT mods es = EffT' FList mods es
type PureL mods es = EffT' FList mods es Identity
type InL   mods es = In'   FList mods es

-- | A constraint that checks the error list is empty in EffT
type family MonadNoError m :: Constraint where
  MonadNoError (EffT' c mods NoError m) = ()
  MonadNoError (EffT' c mods es      m) = TypeError ('Text "MonadNoError: the effect has error, catch them")
  MonadNoError _ = TypeError ('Text "MonadNoError: not an EffT' type")

-- | identity function that checks a MonadNoError constraint
checkNoError :: MonadNoError m => m a -> m a
checkNoError = id
{-# INLINE checkNoError #-}

-- | a newtype wrapper ErrorText that wraps a Text with a name (symbol type)
-- useful for creating ad-hoc error type
newtype ErrorText (s :: Symbol) = ErrorText Text
  deriving newtype (IsString)

-- | a newtype wrapper ErrorValue that wraps a custom value type v with a name (symbol type)
-- useful for creating ad-hoc error type
newtype ErrorValue (a :: Symbol) (v :: Type) = ErrorValue v

-- | A wrapper dedicated for errors living in MonadThrow and MonadCatch
newtype MonadThrowError = MonadThrowError SomeException
  deriving Show

instance KnownSymbol s => Show (ErrorText s) where
  show (ErrorText t) = "ErrorText of type " ++ symbolVal (Proxy @s) ++ ": " ++ unpack t

instance (KnownSymbol s, Show v) => Show (ErrorValue s v) where
  show (ErrorValue v) = "ErrorValue of type " <> symbolVal (Proxy @s) <> ": " <> show v

instance Functor m => Functor (EffT' c mods es m) where
  fmap f (EffT' eff) = EffT' $ \rs ss -> first (fmap f) <$> eff rs ss
  {-# INLINE fmap #-}

instance Monad m => Applicative (EffT' c mods es m) where
  pure a = EffT' $ \_ ss -> pure (RSuccess a, ss)
  {-# INLINE pure #-}

  EffT' effF <*> EffT' effA = EffT' $ \rs ss -> do
    (eF, ss1) <- effF rs ss
    case eF of
      RSuccess f -> do
        (eA, ss2) <- effA rs ss1
        case eA of
          RSuccess a  -> return (RSuccess (f a), ss2)
          RFailure es -> return (RFailure es, ss2)
      RFailure es -> return (RFailure es, ss1)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (EffT' c mods es m) where
  EffT' eff >>= f = EffT' $ \rs ss -> do
    (eResult, ss1) <- eff rs ss
    case eResult of
      RSuccess a  -> unEffT' (f a) rs ss1
      RFailure es -> return (RFailure es, ss1)
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (EffT' c mods es m) where
  liftIO io = EffT' $ \_ ss -> do
    a <- liftIO io
    return (RSuccess a, ss)
  {-# INLINE liftIO #-}

instance Monad m => MonadReadable (SystemRead c mods) (EffT' c mods es m) where
  query = EffT' $ \rs ss -> return (RSuccess rs, ss)
  {-# INLINE query #-}
  local f (EffT' eff) = EffT' $ \rs ss -> eff (f rs) ss
  {-# INLINE local #-}

instance Monad m => MonadStateful (SystemState c mods) (EffT' c mods es m) where
  get = EffT' $ \_ ss -> return (RSuccess ss, ss)
  {-# INLINE get #-}
  put ss = EffT' $ \_ _ -> return (RSuccess (), ss)
  {-# INLINE put #-}
  modify f = EffT' $ \_ ss -> return (RSuccess (), f ss)
  {-# INLINE modify #-}

instance MonadTrans (EffT' c mods es) where
  lift ma = EffT' $ \_ ss -> do
    a <- ma
    return (RSuccess a, ss)
  {-# INLINE lift #-}

instance MonadTransControl (EffT' c mods es) where
  type StT (EffT' c mods es) a = (Result es a, SystemState c mods)
  liftWith f = EffT' $ \rs ss -> fmap (\a -> (RSuccess a, ss)) $ f $ runEffT rs ss
  {-# INLINE liftWith #-}

  restoreT mRes = EffT' $ \_ _ -> mRes
  {-# INLINE restoreT #-}

-- | The error in throwM is thrown as MonadThrowError, which is a wrapper for SomeException.
instance (Monad m, ConsFDataList c mods, InList MonadThrowError es) => MonadThrow (EffT' c mods es m) where
  throwM = effThrowIn . MonadThrowError . toException
  {-# INLINE throwM #-}

-- | this can only catch MonadThrowError, other errors are algebraic and should be caught by effCatch, effCatchIn, effCatchAll
instance (Monad m, ConsFDataList c mods, InList MonadThrowError es) => MonadCatch (EffT' c mods es m) where
  catch ma handler = effCatchIn' ma $ \(MonadThrowError e) ->
    case fromException e of
      Just e' -> handler e'
      Nothing -> effThrowIn $ MonadThrowError e
  {-# INLINE catch #-}

-- | embed smaller effect into larger effect
embedEffT :: forall mods mods' m c es es' a. (SubList c mods mods', SubListEmbed es es', Monad m)
  => EffT' c mods es m a -> EffT' c mods' es' m a
embedEffT eff = EffT' $ \rs' ss' -> do
  let rs = getSubListF rs'
      ss = getSubListF @_ @mods ss'
      modsEffT' = unEffT' eff
  (emods, ss1) <- modsEffT' rs ss
  return (subListResultEmbed emods, subListUpdateF ss' ss1)
{-# INLINE embedEffT #-}

-- | embed effect, but only change the mods list, the error list remains the same. The (inner) mods type variable is the first type parameter, suitable for type application.
embedMods :: forall mods mods' c es m a. (Monad m, ConsFDataList c mods', SubListEmbed es es, SubList c mods mods') => EffT' c mods es m a -> EffT' c mods' es m a
embedMods = embedEffT
{-# INLINE embedMods #-}

-- | embed effect, but only change the error list, the mods list remains the same. The (inner) es type variable is the first type parameter, suitable for type application.
embedError :: forall es es' c mods m a. (Monad m, SubList c mods mods, SubListEmbed es es') => EffT' c mods es m a -> EffT' c mods es' m a
embedError = embedEffT
{-# INLINE embedError #-}

-- | Run the EffT' computation with data needed, returns the potential error result and the new state in the base monad.
runEffT :: forall mods es m c a. Monad m => SystemRead c mods -> SystemState c mods -> EffT' c mods es m a -> m (Result es a, SystemState c mods)
runEffT rs ss = \eff -> unEffT' eff rs ss
{-# INLINE runEffT #-}

-- | Same as runEff but ignores the new state
runEffT_ :: forall mods es m c a
  .  Monad m
  => SystemRead c mods
  -> SystemState c mods
  -> EffT' c mods es m a
  -> m (Result es a)
runEffT_ rs ss = fmap fst . runEffT rs ss
{-# INLINE runEffT_ #-}

-- | same as runEff, but get rid of the Result wrapper when you have empty error list
runEffTNoError :: forall mods m c a
  .  Monad m
  => SystemRead c mods
  -> SystemState c mods
  -> EffT' c mods NoError m a
  -> m (a, SystemState c mods)
runEffTNoError rs ss = fmap (first resultNoError) . runEffT rs ss
{-# INLINE runEffTNoError #-}

-- | runs the EffT' with no modules
runEffT0 :: (Monad m, ConsFNil c) => EffT' c '[] es m a -> m (Result es a)
runEffT0 = fmap fst . runEffT fNil fNil
{-# INLINE runEffT0 #-}

-- | runs the EffT' with no modules and a single possible error type, return as classic Either type
runEffT01 :: (Monad m, ConsFNil c) => EffT' c '[] '[e] m a -> m (Either e a)
runEffT01 = fmap (first fromElistSingleton . resultToEither) . runEffT0
{-# INLINE runEffT01 #-}

-- | runs the EffT' with no modules and no error
runEffT00 :: (Monad m, ConsFNil c) => EffT' c '[] NoError m a -> m a
runEffT00 = fmap resultNoError . runEffT0
{-# INLINE runEffT00 #-}

-- | Runs a EffT' computation and eliminate the most outer effect with its input given
--
-- Note: `ModuleState mod` will be lost (because nothing will be returned) when the outer EffT' monad returns an exception. This should not be a problem and is expected from the type signature. But if you want to avoid it, you can catch the exceptions or use `errorToEitherAll` or `runEffTOuter'`
runEffTOuter :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods es m (a, ModuleState mod)
runEffTOuter mread mstate eff = EffT' $ \modsRead modsState ->
    (\(ea, (s :*** ss)) -> ((,s) <$> ea, ss)) <$> (unEffT' @_ @(mod:mods) eff) (mread `consF1` modsRead) (mstate `consF1` modsState)
{-# INLINE runEffTOuter #-}

-- | Runs a EffT' computation and eliminate the most outer effect with its input given, returning the result as Result type.
--
-- This makes sure the `ModuleState mod` up until exception or completion is always returned.
runEffTOuter' :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods NoError m (Result es a, ModuleState mod)
runEffTOuter' r s = runEffTOuter r s . errorToResult
{-# INLINE runEffTOuter' #-}

-- | the same as `runEffTOuter`, but discards the state
runEffTOuter_ :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods es m a
runEffTOuter_ mread mstate eff = fst <$> runEffTOuter @mod @mods mread mstate eff
{-# INLINE runEffTOuter_ #-}

-- | Running an inner module of EffT, eliminates it
runEffTIn :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) es m (a, ModuleState mod)
runEffTIn mread mstate eff = EffT' $ \modsRead modsState -> do
  let rs = unRemoveElem (singFirstIndex @mod @mods) mread modsRead
      ss = unRemoveElem (singFirstIndex @mod @mods) mstate modsState
  (ea, ss') <- unEffT' eff rs ss
  case ea of
    RSuccess a  -> pure (RSuccess (a, getIn ss'), removeElem (singFirstIndex @mod @mods) ss')
    RFailure es -> pure (RFailure es, removeElem (singFirstIndex @mod @mods) ss')
{-# INLINE runEffTIn #-}

-- | Runs an inner EffT' module and eliminate it, returning the result as Result type.
--
-- This makes sure the `ModuleState mod` up until exception or completion is always returned.
runEffIn' :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) NoError m (Result es a, ModuleState mod)
runEffIn' mread mstate = runEffTIn mread mstate . errorToResult
{-# INLINE runEffIn' #-}

-- | The same as runEffTIn, but discards the state
runEffTIn_ :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) es m a
runEffTIn_ mread mstate eff = fst <$> runEffTIn @mod @mods mread mstate eff
{-# INLINE runEffTIn_ #-}
-------------------------------------- instances --------------------------------------

-- | The unit of Effect, a module is a type with certain associated data family types
class Module mod where
  data ModuleRead     mod :: Type
  data ModuleState    mod :: Type

-- | A module that can be placed into a system, has some init data required to initialize it, and can have some events
class Module mod => SystemModule mod where
  data ModuleEvent    mod :: Type
  data ModuleInitData mod :: Type

type SystemState    c mods = c     ModuleState    mods
type SystemRead     c mods = c     ModuleRead     mods

type SystemEvent      mods = UList ModuleEvent    mods
type SystemInitData c mods = c     ModuleInitData mods

data SystemError
  = SystemErrorException SomeException
  | SystemErrorText      Text
  deriving Show

-- | NoError is just a synonym for empty list
type NoError = '[]

-- | Queries the module read inside the EffT' monad
queryModule :: forall mod mods c m es. (Monad m, In' c mod mods, Module mod) => EffT' c mods es m (ModuleRead mod)
queryModule = queries @(SystemRead c mods) (getIn @c @mod)
{-# INLINE queryModule #-}

-- | The same as qeuryModule, just a synonym
askModule :: forall mod mods c m es. (Monad m, In' c mod mods, Module mod) => EffT' c mods es m (ModuleRead mod)
askModule = queryModule
{-# INLINE askModule #-}

-- | Queries the module read inside the EffT' monad, using a function to extract the value
queriesModule :: forall mod mods es m c a. (Monad m, In' c mod mods, Module mod) => (ModuleRead mod -> a) -> EffT' c mods es m a
queriesModule f = f <$> queryModule @mod @mods @c
{-# INLINE queriesModule #-}

-- | The same as queriesModule, just a synonym
asksModule :: forall mod mods es m c a. (Monad m, In' c mod mods, Module mod) => (ModuleRead mod -> a) -> EffT' c mods es m a
asksModule = queriesModule
{-# INLINE asksModule #-}

-- | Run the EffT' computation with a modified module read
localModule :: forall mod mods es m c a. (Monad m, In' c mod mods, Module mod) => (ModuleRead mod -> ModuleRead mod) -> EffT' c mods es m a -> EffT' c mods es m a
localModule f eff = EffT' $ \rs ss -> do
  let rs' = modifyIn @c @mod f rs
  unEffT' eff rs' ss
{-# INLINE localModule #-}

-- | Get the module state inside the EffT' monad
getModule :: forall mod mods es m c. (Monad m, In' c mod mods, Module mod) => EffT' c mods es m (ModuleState mod)
getModule = gets @(SystemState c mods) (getIn @c @mod)
{-# INLINE getModule #-}

-- | Get the module state inside the EffT' monad, using a function to extract the value
getsModule :: forall mod mods es m c a. (Monad m, In' c mod mods, Module mod) => (ModuleState mod -> a) -> EffT' c mods es m a
getsModule f = f <$> getModule @mod
{-# INLINE getsModule #-}

-- | Put the module state inside the EffT' monad
putModule :: forall mod mods es m c. (Monad m, In' c mod mods, Module mod) => ModuleState mod -> EffT' c mods es m ()
putModule x = modify @(SystemState c mods) (modifyIn $ const x)
{-# INLINE putModule #-}

-- | Modify the module state inside the EffT' monad
modifyModule :: forall mod mods es m c. (Monad m, In' c mod mods, Module mod) => (ModuleState mod -> ModuleState mod) -> EffT' c mods es m ()
modifyModule f = modify @(SystemState c mods) (modifyIn f)
{-# INLINE modifyModule #-}

-- | Declare that the computation has no error, it just discards the error types. When the error actually happen it will be runtime exception.
declareNoError :: Monad m => EffT' c mods es m a -> EffT' c mods NoError m a
declareNoError eff = eff `effCatchAll` \_es -> error "declareNoError: declared NoError, but got errors"
{-# INLINE declareNoError #-}

-- | lift IO action into EffT, catch IOException and return as Left, synonym for effIOSafe
liftIOException :: IO a -> EffT' c mods '[IOException] IO a
liftIOException = liftIOAt
{-# INLINE liftIOException #-}

-- | lift IO and catch a specific type of exception
liftIOAt :: Exception e => IO a -> EffT' c mods '[e] IO a
liftIOAt = liftIOSafeWith id
{-# INLINE liftIOAt #-}

-- | Capture SomeException in IO and turn it into a ErrorText
liftIOText :: forall s mods c a. (Text -> Text) -> IO a -> EffT' c mods '[ErrorText s] IO a
liftIOText err = liftIOSafeWith (\(e :: SomeException) -> ErrorText $ err $ pack $ show e)
{-# INLINE liftIOText #-}

-- | lift IO action into EffT, catch SomeException, turn it into Text
-- and prepend error message into ErrorText s. The type `s` is a type level string and at the first type parameter, suitable for type application.
--
-- example: `liftIOPrepend @"File" "File error:" $ readFile' "file.txt"`
liftIOPrepend :: forall s mods c a. Text -> IO a -> EffT' c mods '[ErrorText s] IO a
liftIOPrepend err = liftIOText (err <>)
{-# INLINE liftIOPrepend #-}

-- | lift IO action into EffT, catch specific type of exception e' into a custom error e
liftIOSafeWith :: Exception e' => (e' -> e) -> IO a -> EffT' c mods '[e] IO a
liftIOSafeWith f io = EffT' $ \_ s -> do
  a :: Either e' a <- E.try io
  case a of
    Right a' -> return (RSuccess a', s)
    Left e'  -> return (RFailure $ EHead $ f e', s)
{-# INLINE liftIOSafeWith #-}

-- | @try@ on the Base monad IO, adding as the first error in the error list.
-- It is recommended that you wrap low-level routines into algebraic error in the first place instead of using this function.
effTryIO :: Exception e => EffT' c mods es IO a -> EffT' c mods (e : es) IO a
effTryIO = effTryIOWith id
{-# INLINE effTryIO #-}

-- | @try@ on the Base monad IO, put into the error list.
-- It is recommended that you wrap low-level routines into algebraic error in the first place instead of using this function.
effTryIOIn :: forall e es c mods a. (Exception e, InList e es) => EffT' c mods es IO a -> EffT' c mods es IO a
effTryIOIn = effTryIOInWith @e id
{-# INLINE effTryIOIn #-}

-- | @try@ on the Base monad IO with the given function, adding as the first error in the error list.
-- It is recommended that you wrap low-level routines into algebraic error in the first place instead of using this function.
effTryIOWith :: Exception e => (e -> e') -> EffT' c mods es IO a -> EffT' c mods (e' : es) IO a
effTryIOWith f eff = EffT' $ \rs ss -> do
  ePair <- E.try (unEffT' eff rs ss)
  case ePair of
    Left e -> return (RFailure $ EHead $ f e, ss)
    Right (eResult, stateMods) -> return (resultMapErrors ETail eResult, stateMods)
{-# INLINE effTryIOWith #-}

-- | @try@ on the Base monad IO with the given function, adding as the first error in the error list.
-- It is recommended that you wrap low-level routines into algebraic error in the first place instead of using this function.
effTryIOInWith :: (Exception e, InList e' es) => (e -> e') -> EffT' c mods es IO a -> EffT' c mods es IO a
effTryIOInWith f eff = EffT' $ \rs ss -> do
  ePair <- E.try (unEffT' eff rs ss)
  case ePair of
    Left e                     -> return (RFailure $ embedE $ f e, ss)
    Right (eResult, stateMods) -> return (eResult, stateMods)
{-# INLINE effTryIOInWith #-}

-- | Convert the first error in the effect to Either
errorToEither :: Monad m => EffT' c mods (e : es) m a -> EffT' c mods es m (Either e a)
errorToEither eff = EffT' $ \rs ss -> do
  (eResult, stateMods) <- unEffT' eff rs ss
  case eResult of
    RSuccess a          -> return (RSuccess (Right a), stateMods)
    RFailure (EHead e)  -> return (RSuccess (Left e), stateMods)
    RFailure (ETail es) -> return (RFailure es, stateMods)
{-# INLINE errorToEither #-}

-- | Convert all errors to Either
errorToEitherAll :: Monad m => EffT' c mods es m a -> EffT' c mods NoError m (Either (EList es) a)
errorToEitherAll eff = EffT' $ \rs ss -> do
  (eResult, stateMods) <- unEffT' eff rs ss
  case eResult of
    RSuccess a    -> return (RSuccess (Right a), stateMods)
    RFailure es   -> return (RSuccess (Left es), stateMods)
{-# INLINE errorToEitherAll #-}

-- | Convert all errors to Result
errorToResult :: Monad m => EffT' c mods es m a -> EffT' c mods NoError m (Result es a)
errorToResult eff = EffT' $ \rs ss -> do
  (eResult, stateMods) <- unEffT' eff rs ss
  case eResult of
    RSuccess a    -> return (pure $ RSuccess a, stateMods)
    RFailure es   -> return (pure $ RFailure es, stateMods)
{-# INLINE errorToResult #-}

-- | The reverse of errorToEither, convert Either (EList es) into the error list.
eitherAllToEffect :: Monad m => EffT' c mods NoError m (Either (EList es) a) -> EffT' c mods es m a
eitherAllToEffect eff = EffT' $ \rs ss -> do
  (eResult, stateMods) <- unEffT' eff rs ss
  case eResult of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left es) -> return (RFailure es, stateMods)
{-# INLINE eitherAllToEffect #-}

-- | Catch SystemError
effCatchSystem :: (Monad m, In' c SystemError es) => EffT' c mods es m a -> (SystemError -> EffT' c mods es m a) -> EffT' c mods es m a
effCatchSystem eff h = EffT' $ \rs ss -> do
  (eResult, stateMods) <- (unEffT' eff) rs ss
  case eResult of
    RSuccess a  -> return (RSuccess a, stateMods)
    RFailure es | Just e@(SystemErrorException _) <- getEMaybe es -> do
      unEffT' (h e) rs ss
    RFailure es -> return (RFailure es, stateMods)
{-# INLINE effCatchSystem #-}

-- | Catch the first error in the error list, and handle it with a handler function
effCatch :: Monad m => EffT' c mods (e : es) m a -> (e -> EffT' c mods es m a) -> EffT' c mods es m a
effCatch eff h = EffT' $ \rs ss -> do
  (eResult, stateMods) <- (unEffT' eff) rs ss
  case eResult of
    RSuccess a          -> return (RSuccess a, stateMods)
    RFailure (EHead e)  -> (unEffT' $ h e) rs ss
    RFailure (ETail es) -> return (RFailure $ es, stateMods)
{-# INLINE effCatch #-}

-- | Catch a specific error type in the error list, and handle it with a handler function.
-- This will remove the error type from the error list.
--
-- the error type is the first type parameter, suitable for type application.
effCatchIn:: forall e es mods m c a es'. (Monad m, InList e es, es' ~ Remove (FirstIndex e es) es)
  => EffT' c mods es m a -> (e -> EffT' c mods es' m a) -> EffT' c mods es' m a
effCatchIn eff h = EffT' $ \rs ss -> do
  (eResult, stateMods) <- (unEffT' eff) rs ss
  case getElemRemoveResult (singIndex @e @es) eResult of
    Left e -> case proofIndex @e @es of
      Refl -> unEffT' (h e) rs ss
    Right eResult' -> return (eResult', stateMods)
{-# INLINE effCatchIn #-}

-- | Same as effCatchIn, but Does Not remove the error type
effCatchIn' :: forall e es mods m c a. (Monad m, InList e es)
  => EffT' c mods es m a -> (e -> EffT' c mods es m a) -> EffT' c mods es m a
effCatchIn' eff h = EffT' $ \rs ss -> do
  r@(eResult, _) <- (unEffT' eff) rs ss
  case eResult of
    RSuccess _ -> return r
    RFailure es  -> case getEMaybe @e @es es of
      Just e' -> unEffT' (h e') rs ss
      Nothing -> return r
{-# INLINE effCatchIn' #-}

-- | Catch all errors in the error list, and handle it with a handler function. You can pattern match on `EList es` to handle the errors.
-- Removes all errors from the error list.
effCatchAll :: Monad m => EffT' c mods es m a -> (EList es -> EffT' c mods NoError m a) -> EffT' c mods NoError m a
effCatchAll eff h = EffT' $ \rs ss -> do
  (er, stateMods) <- (unEffT' eff) rs ss
  case er of
    RSuccess a    -> return (RSuccess a, stateMods)
    RFailure es   -> (unEffT' $ h es) rs ss
{-# INLINE effCatchAll #-}

-- | Throw into the error list
effThrowIn :: (Monad m, InList e es) => e -> EffT' c mods es m a
effThrowIn e = EffT' $ \_ s -> pure (RFailure $ embedE e, s)
{-# INLINE effThrowIn #-}

-- | Throw into the error list
effThrow :: (Monad m, InList e es) => e -> EffT' c mods es m a
effThrow = effThrowIn
{-# INLINE effThrow #-}

-- | Turn an Either return type into the error list with a function, adding the error type if it is not already in the error list.
-- The inner monad type needs to be precise due to the way type inference works.
effEitherWith :: forall e e' es mods c m a. (CheckIfElem e' es, Monad m)
  => (e -> e') -> EffT' c mods es m (Either e a) -> EffT' c mods (AddIfNotElem e' es) m a
effEitherWith f eff = case singIfElem @e' @es of
  Left Refl           -> EffT' $ \rs ss -> do
    (eResult, stateMods) <- unEffT' eff rs ss
    case eResult of
      RSuccess (Right a) -> return (RSuccess a, stateMods)
      RSuccess (Left e)  -> return (RFailure $ EHead $ f e, stateMods)
      RFailure es        -> return (RFailure $ ETail es, stateMods)
  Right (Refl, index) -> EffT' $ \rs ss -> do
    (eResult, stateMods) <- unEffT' eff rs ss
    case eResult of
      RSuccess (Right a) -> return (RSuccess a, stateMods)
      RSuccess (Left e)  -> return (RFailure $ embedES index $ f e, stateMods)
      RFailure es        -> return (RFailure $ es, stateMods)
{-# INLINE effEitherWith #-}

-- | Turn an Either return type into the error list, adding the error type if it is not already in the error list.
-- The inner monad type needs to be precise due to the way type inference works.
effEither :: (CheckIfElem e es, Monad m) => EffT' c mods es m (Either e a) -> EffT' c mods (AddIfNotElem e es) m a
effEither = effEitherWith id
{-# INLINE effEither #-}

-- | Lift an Either return type in the base monad into EffT
baseEitherIn :: (Monad m, InList e es) => m (Either e a) -> EffT' c mods es m a
baseEitherIn = effEitherIn . lift
{-# INLINE baseEitherIn #-}

-- | Lift an Either return type in the base monad into EffT with the given function
baseEitherInWith :: (Monad m, InList e' es) => (e -> e') -> m (Either e a) -> EffT' c mods es m a
baseEitherInWith f = effEitherInWith f . lift
{-# INLINE baseEitherInWith #-}

-- | Turn an Either return type into the error list with a function
effEitherInWith :: (Monad m, InList e' es) => (e -> e') -> EffT' c mods es m (Either e a) -> EffT' c mods es m a
effEitherInWith f eff = EffT' $ \rs ss -> do
  (eResult, stateMods) <- unEffT' eff rs ss
  case eResult of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left e)  -> return (RFailure $ embedE $ f e, stateMods)
    RFailure sysE      -> return (RFailure sysE, stateMods)
{-# INLINE effEitherInWith #-}

-- | Turn an Either return type into the error list
effEitherIn :: (Monad m, InList e es) => EffT' c mods es m (Either e a) -> EffT' c mods es m a
effEitherIn = effEitherInWith id
{-# INLINE effEitherIn #-}

-- | Turn a pure Maybe value into error with the given error type.
pureMaybeInWith :: forall e es m mods c a. (In' c e es, Monad m) => e -> Maybe a -> EffT' c mods es m a 
pureMaybeInWith e = effMaybeInWith e . lift . pure
{-# INLINE pureMaybeInWith #-}

-- | Lift a Maybe return type in the base monad into EffT with the given error type.
baseMaybeInWith :: forall e es m mods c a. (In' c e es, Monad m) => e -> m (Maybe a) -> EffT' c mods es m a
baseMaybeInWith f = effMaybeInWith f . lift
{-# INLINE baseMaybeInWith #-}

-- | Turn a pure Either value into error with the given error type.
pureEitherInWith :: forall e' e es m mods c a. (In' c e' es, Monad m) => (e -> e') -> Either e a -> EffT' c mods es m a
pureEitherInWith f = effEitherInWith f . lift . pure
{-# INLINE pureEitherInWith #-}

-- | Turn an Either return type into the error list, adding the error type if it is not already in the error list.
-- The inner monad type needs to be precise due to the way type inference works.
effMaybeWith :: forall e es m mods c a. (CheckIfElem e es, Monad m) => e -> EffT' c mods es m (Maybe a) -> EffT' c mods (AddIfNotElem e es) m a
effMaybeWith e eff = case singIfElem @e @es of
  Left Refl           -> EffT' $ \rs ss -> do
   (eResult, stateMods) <- unEffT' eff rs ss
   case eResult of
     RSuccess (Just a) -> return (RSuccess a, stateMods)
     RSuccess Nothing  -> return (RFailure $ EHead e, stateMods)
     RFailure sysE     -> return (RFailure $ ETail sysE, stateMods)
  Right (Refl, index) -> EffT' $ \rs ss -> do
    (eResult, stateMods) <- unEffT' eff rs ss
    case eResult of
      RSuccess (Just a) -> return (RSuccess a, stateMods)
      RSuccess Nothing  -> return (RFailure $ embedES index e, stateMods)
      RFailure es       -> return (RFailure $ es, stateMods)
{-# INLINE effMaybeWith #-}

-- | Turn an Maybe return type into the error list
effMaybeInWith :: forall e es m mods c a. (In' c e es, Monad m) => e -> EffT' c mods es m (Maybe a) -> EffT' c mods es m a 
effMaybeInWith e eff = EffT' $ \rs ss -> do
  (eResult, stateMods) <- unEffT' eff rs ss
  case eResult of
    RSuccess (Just a)  -> return (RSuccess a, stateMods)
    RSuccess (Nothing) -> return (RFailure $ embedE $ e, stateMods)
    RFailure sysE      -> return (RFailure sysE, stateMods)
{-# INLINE effMaybeInWith #-}

effEitherSystemException :: (Monad m, Exception e, InList SystemError es) => EffT' c mods es m (Either e a) -> EffT' c mods es m a
effEitherSystemException = effEitherInWith (\e -> SystemErrorException $ toException e)
