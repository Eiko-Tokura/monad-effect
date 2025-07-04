{-# LANGUAGE DerivingVia, UndecidableInstances, AllowAmbiguousTypes, LinearTypes #-}
module Control.Monad.Effect
  ( -- * EffTectful computation
    Eff, Pure, EffT(..)
  , EffL, PureL
  , ErrorText(..)
  , embedEffT--, embedEffT
  , embedMods, embedError
  , runEffT--, runEffT
  , runEffT_, runEffT0, runEffT01, runEffT00
  , runEffTWithInitData, runEffTNoError
  , runEffTOuter, runEffTOuter_
  , runEffTIn, runEffTIn_
  , effCatch, effCatchAll, effCatchSystem
  , effCatchIn
  , effThrow, effThrowSystem
  , effEither, effEitherWith
  , effEitherIn, effEitherInWith
  , effEitherSystemWith, effEitherSystemException
  , effMaybeWith, effMaybeInWith
  , errorToEither, errorToEitherAll
  , liftIOException, liftIOAt, liftIOSafeWith, liftIOText, liftIOPrepend

  -- , proofEmbedEffT
  , declareNoError
  , checkNoError

  -- * Module and System
  , Module(..), System(..), Loadable(..)
  , queryModule, queriesModule
  , localModule
  , getModule, getsModule
  , putModule, modifyModule

  , SystemError(..), NoError
  , SystemInitData, SystemState, SystemRead, SystemEvent
  , SystemInitDataHardCode

  -- * Loadable
  , ModuleInitDataHardCode
  , LoadableEnv(..), LoadableArgs(..), SystemEnv(..), SystemArgs(..)

  -- * Re-exports
  , MonadIO(..)
  , ConsFDataList, FList, FData
  ) where

import Data.TypeList
import Data.TypeList.FData
import Data.Bifunctor
import Data.Kind
import Data.Text (Text, unpack, pack)
import Data.Type.Equality
import Data.Proxy
import Data.String (IsString)
import Data.Functor.Identity
import GHC.TypeError
import GHC.TypeLits
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.RST
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Exception as E hiding (TypeError)

-- | EffTectful computation, using modules as units of effect
-- newtype EffT mods es a = EffT { unEffT :: SystemRead mods -> SystemState mods -> IO (Result es a, SystemState mods) }
newtype EffT (c :: (Type -> Type) -> [Type] -> Type) mods es m a = EffT { unEffT :: SystemRead c mods -> SystemState c mods -> m (Result es a, SystemState c mods) }

type Eff  mods es  = EffT FData mods es IO
type Pure mods es  = EffT FData mods es Identity

type EffL  mods es = EffT FList mods es IO
type PureL mods es = EffT FList mods es Identity

type family MonadNoError m :: Constraint where
  MonadNoError (EffT c mods NoError m) = ()
  MonadNoError (EffT c mods es      m) = TypeError ('Text "MonadNoError: the effect has error, catch them")
  MonadNoError _ = TypeError ('Text "MonadNoError: not an EffT type")

checkNoError :: MonadNoError m => m a -> m a
checkNoError = id
{-# INLINE checkNoError #-}

newtype ErrorText (s :: Symbol) = ErrorText Text
  deriving newtype (IsString)

instance KnownSymbol s => Show (ErrorText s) where
  show (ErrorText t) = "Error of type " ++ symbolVal (Proxy @s) ++ ": " ++ unpack t

instance Functor m => Functor (EffT c mods es m) where
  fmap f (EffT eff) = EffT $ \rs ss -> first (fmap f) <$> eff rs ss
  {-# INLINE fmap #-}

instance Monad m => Applicative (EffT c mods es m) where
  pure a = EffT $ \_ ss -> pure (RSuccess a, ss)
  {-# INLINE pure #-}

  EffT effF <*> EffT effA = EffT $ \rs ss -> do
    (eF, ss1) <- effF rs ss
    case eF of
      RSuccess f -> do
        (eA, ss2) <- effA rs ss1
        case eA of
          RSuccess a  -> return (RSuccess (f a), ss2)
          RFailure es -> return (RFailure es, ss2)
      RFailure es -> return (RFailure es, ss1)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (EffT c mods es m) where
  EffT eff >>= f = EffT $ \rs ss -> do
    (eResult, ss1) <- eff rs ss
    case eResult of
      RSuccess a  -> unEffT (f a) rs ss1
      RFailure es -> return (RFailure es, ss1)
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (EffT c mods es m) where
  liftIO io = EffT $ \_ ss -> do
    a <- liftIO io
    return (RSuccess a, ss)
  {-# INLINE liftIO #-}

instance Monad m => MonadReadable (SystemRead c mods) (EffT c mods es m) where
  query = EffT $ \rs ss -> return (RSuccess rs, ss)
  {-# INLINE query #-}
  local f (EffT eff) = EffT $ \rs ss -> eff (f rs) ss
  {-# INLINE local #-}

instance Monad m => MonadStateful (SystemState c mods) (EffT c mods es m) where
  get = EffT $ \_ ss -> return (RSuccess ss, ss)
  {-# INLINE get #-}
  put ss = EffT $ \_ _ -> return (RSuccess (), ss)
  {-# INLINE put #-}
  modify f = EffT $ \_ ss -> return (RSuccess (), f ss)
  {-# INLINE modify #-}

instance MonadTrans (EffT c mods es) where
  lift ma = EffT $ \_ ss -> do
    a <- ma
    return (RSuccess a, ss)
  {-# INLINE lift #-}

instance MonadTransControl (EffT c mods es) where
  type StT (EffT c mods es) a = (Result es a, SystemState c mods)
  liftWith f = EffT $ \rs ss -> fmap (\a -> (RSuccess a, ss)) $ f $ runEffT rs ss
  {-# INLINE liftWith #-}

  restoreT mRes = EffT $ \_ _ -> mRes
  {-# INLINE restoreT #-}

-- | The error in throwM is thrown to the top level as SystemErrorException SomeException
instance (Monad m, ConsFDataList c mods, SubList c mods mods, SubList c es es, In SystemError es) => MonadThrow (EffT c mods es m) where
  throwM = embedEffT @mods @mods @_ @_ @es @es . effThrowSystem @es . SystemErrorException . toException
  {-# INLINE throwM #-}

-- | this can only catch SystemErrorException SomeException, other errors are algebraic
instance (Monad m, ConsFDataList c mods, SubList c mods mods, SubList c es es, In' c SystemError es) => MonadCatch (EffT c mods es m) where
  catch ma handler = effCatchSystem ma $ \case
    SystemErrorException e -> case fromException e of
      Just e' -> handler e'
      Nothing -> embedEffT @mods @mods . effThrowSystem @es $ SystemErrorException e
    e                      -> effThrow e
  {-# INLINE catch #-}

-- | embed smaller effect into larger effect
embedEffT :: forall mods mods' m c es es' a. (SubList c mods mods', SubListEmbed es es', Monad m)
  => EffT c mods es m a -> EffT c mods' es' m a
embedEffT eff = EffT $ \rs' ss' -> do
  let rs = getSubListF rs'
      ss = getSubListF @_ @mods ss'
      modsEffT = unEffT eff
  (emods, ss1) <- modsEffT rs ss
  return (subListResultEmbed emods, subListUpdateF ss' ss1)
{-# INLINE embedEffT #-}

-- embedEffT :: forall mods mods' es es' a. (SubList mods mods', SubList es es')
--   => EffT mods es a -> EffT mods' es' a
-- embedEffT effT = coerce $ embedEffT effT
-- {-# INLINE embedEffT #-}

-- proofEmbedEffT :: Monad m => ProofSubList mods mods' -> ProofSubList es es' -> EffT c mods es m a -> EffT c mods' es' m a
-- proofEmbedEffT pm pe eff = EffT $ \rs' ss' -> do
--   let rs = proofGetSubListF pm rs'
--       ss = proofGetSubListF pm ss'
--       modsEffT = unEffT eff
--   (emods, ss1) <- modsEffT rs ss
--   return (proofSubListErrorEmbed pe emods, proofSubListUpdateF pm ss' ss1)
-- {-# INLINE proofEmbedEffT #-}

embedMods :: forall mods mods' c es m a. (Monad m, ConsFDataList c mods', SubList c mods mods', SubListEmbed es es) => EffT c mods es m a -> EffT c mods' es m a
embedMods = embedEffT
{-# INLINE embedMods #-}

embedError :: forall es es' c mods m a. (Monad m, SubList c mods mods, SubListEmbed es es') => EffT c mods es m a -> EffT c mods es' m a
embedError = embedEffT
{-# INLINE embedError #-}

runEffT :: forall mods es m c a. Monad m => SystemRead c mods -> SystemState c mods -> EffT c mods es m a -> m (Result es a, SystemState c mods)
runEffT rs ss eff = unEffT eff rs ss
{-# INLINE runEffT #-}

runEffT_ :: forall mods es m c a
  .  Monad m
  => SystemRead c mods
  -> SystemState c mods
  -> EffT c mods es m a
  -> m (Result es a)
runEffT_ rs ss eff = fst <$> runEffT rs ss eff
{-# INLINE runEffT_ #-}

-- | If error happens at initialization, it will return Left SystemError
-- If error happens during event loop, it will return Right (Left (SList (SystemError : es)))
runEffTWithInitData :: forall mods es m c a. (ConsFDataList c mods, System mods, MonadIO m)
  => SystemInitData c mods
  -> EffT c mods es m a
  -> m
      (Either SystemError -- ^ if error happens at initialization
        ( Result es a -- ^ if error happens during event loop
        , SystemState c mods)
      )
runEffTWithInitData initData eff = do
  liftIO (runEffT0 (initAllModules @mods initData)) >>= \case
    RSuccess (rs, ss)  -> Right <$> runEffT rs ss eff
    RFailure (EHead e) -> return $ Left e

runEffTNoError :: forall mods m c a
  .  Monad m
  => SystemRead c mods
  -> SystemState c mods
  -> EffT c mods NoError m a
  -> m (a, SystemState c mods)
runEffTNoError rs ss eff = first resultNoError <$> unEffT eff rs ss
{-# INLINE runEffTNoError #-}

runEffT0 :: ConsFNil c => EffT c '[] es IO a -> IO (Result es a)
runEffT0 = fmap fst . runEffT fNil fNil
{-# INLINE runEffT0 #-}

runEffT01 :: ConsFNil c => EffT c '[] '[e] IO a -> IO (Either e a)
runEffT01 = fmap (first fromElistSingleton . resultToEither) . runEffT0

runEffT00 :: ConsFNil c => EffT c '[] NoError IO a -> IO a
runEffT00 = fmap resultNoError . runEffT0
{-# INLINE runEffT00 #-}

-- | Warning: state will lose when you have an error
runEffTOuter :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m) => ModuleRead mod -> ModuleState mod -> EffT c (mod : mods) es m a -> EffT c mods es m (ModuleState mod, a)
runEffTOuter mread mstate eff = EffT
  $ \modsRead modsState ->
    (\(ea, (s :*** ss)) -> ((s,) <$> ea, ss)) <$> (unEffT @_ @(mod:mods) eff) (mread `consF1` modsRead) (mstate `consF1` modsState)
{-# INLINE runEffTOuter #-}

runEffTOuter_ :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m) => ModuleRead mod -> ModuleState mod -> EffT c (mod : mods) es m a -> EffT c mods es m a
runEffTOuter_ mread mstate eff = EffT
  $ \modsRead modsState ->
    (\(ea, (_ :*** ss)) -> (ea, ss)) <$> (unEffT @_ @(mod:mods) eff) (mread `consF1` modsRead) (mstate `consF1` modsState)
{-# INLINE runEffTOuter_ #-}

runEffTIn :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT c mods es m a
  -> EffT c (Remove (FirstIndex mod mods) mods) es m (a, ModuleState mod)
runEffTIn mread mstate eff = EffT $ \modsRead modsState -> do
  let rs = unRemoveElem (singFirstIndex @mod @mods) mread modsRead
      ss = unRemoveElem (singFirstIndex @mod @mods) mstate modsState
  (ea, ss') <- unEffT eff rs ss
  case ea of
    RSuccess a  -> pure (RSuccess (a, getIn ss'), removeElem (singFirstIndex @mod @mods) ss')
    RFailure es -> pure (RFailure es, removeElem (singFirstIndex @mod @mods) ss')
{-# INLINE runEffTIn #-}

runEffTIn_ :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT c mods es m a
  -> EffT c (Remove (FirstIndex mod mods) mods) es m a
runEffTIn_ mread mstate eff = fst <$> runEffTIn @mod @mods mread mstate eff
{-# INLINE runEffTIn_ #-}
-------------------------------------- instances --------------------------------------

class Module mod where
  data ModuleInitData mod :: Type
  data ModuleRead     mod :: Type
  data ModuleState    mod :: Type
  data ModuleEvent    mod :: Type

type SystemInitDataHardCode c mods = c ModuleInitDataHardCode mods
type SystemInitData c mods = c     ModuleInitData mods
type SystemState    c mods = c     ModuleState    mods
type SystemRead     c mods = c     ModuleRead     mods
type SystemEvent      mods = UList ModuleEvent    mods
data SystemError
  = SystemErrorException SomeException
  | SystemErrorText      Text
  deriving Show

type NoError = '[]

queryModule :: forall mod mods c m es. (Monad m, In' c mod mods, Module mod) => EffT c mods es m (ModuleRead mod)
queryModule = queries @(SystemRead c mods) (getIn @c @mod)
{-# INLINE queryModule #-}

queriesModule :: forall mod mods es m c a. (Monad m, In' c mod mods, Module mod) => (ModuleRead mod -> a) -> EffT c mods es m a
queriesModule f = f <$> queryModule @mod @mods @c
{-# INLINE queriesModule #-}

localModule :: forall mod mods es m c a. (Monad m, In' c mod mods, Module mod) => (ModuleRead mod -> ModuleRead mod) -> EffT c mods es m a -> EffT c mods es m a
localModule f eff = EffT $ \rs ss -> do
  let rs' = modifyIn @c @mod f rs
  unEffT eff rs' ss
{-# INLINE localModule #-}

getModule :: forall mod mods es m c. (Monad m, In' c mod mods, Module mod) => EffT c mods es m (ModuleState mod)
getModule = gets @(SystemState c mods) (getIn @c @mod)
{-# INLINE getModule #-}

getsModule :: forall mod mods es m c a. (Monad m, In' c mod mods, Module mod) => (ModuleState mod -> a) -> EffT c mods es m a
getsModule f = f <$> getModule @mod
{-# INLINE getsModule #-}

putModule :: forall mod mods es m c. (Monad m, In' c mod mods, Module mod) => ModuleState mod -> EffT c mods es m ()
putModule x = modify @(SystemState c mods) (modifyIn $ const x)
{-# INLINE putModule #-}

modifyModule :: forall mod mods es m c. (Monad m, In' c mod mods, Module mod) => (ModuleState mod -> ModuleState mod) -> EffT c mods es m ()
modifyModule f = modify @(SystemState c mods) (modifyIn f)
{-# INLINE modifyModule #-}

-- | Specifies that the module can load after mods are loaded
-- in practice we could use
-- instance SomeModuleWeNeed `In` mods => Loadable mods SomeModuleToLoad
class Loadable mod mods where
  initModule  :: ModuleInitData mod -> EffT c mods '[SystemError] IO (ModuleRead mod, ModuleState mod)

  beforeEvent :: EffT c (mod : mods) NoError IO ()
  beforeEvent = return ()
  {-# INLINE beforeEvent #-}

  afterEvent  :: EffT c (mod : mods) NoError IO ()
  afterEvent = return ()
  {-# INLINE afterEvent #-}

  moduleEvent :: EffT c (mod : mods) NoError IO (STM (ModuleEvent mod))
  moduleEvent = return empty
  {-# INLINE moduleEvent #-}

  handleEvent :: ModuleEvent mod -> EffT c (mod : mods) NoError IO ()
  handleEvent _ = return ()
  {-# INLINE handleEvent #-}

  releaseModule :: EffT c (mod : mods) NoError IO () -- ^ release resources, quit module
  releaseModule = return ()
  {-# INLINE releaseModule #-}

data family ModuleInitDataHardCode mod :: Type

class Loadable mod mods => LoadableEnv mod mods where
  readInitDataFromEnv :: ModuleInitDataHardCode mod -> EffT c '[] '[SystemError] IO (ModuleInitData mod)
  -- ^ Read module init data from environment, or other means
  -- one can change this to SystemInitData mods -> IO (ModuleInitData mod)

class Loadable mod mods => LoadableArgs mod mods where
  readInitDataFromArgs :: ModuleInitDataHardCode mod -> [String] -> EffT c '[] '[SystemError] IO (ModuleInitData mod)
  -- ^ Read module init data from command line arguments, or using comand line arguments to read other things
  -- one can change this to SystemInitData mods -> [String] -> IO (ModuleInitData mod)

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
--
-- the last module in the list is the first to be loaded
-- and also the first to execute beforeEvent and afterEvent
class System mods where
  initAllModules :: ConsFDataList c mods => SystemInitData c mods -> EffT c '[] '[SystemError] IO (SystemRead c mods, SystemState c mods)

  listenToEvents :: ConsFDataList c mods => EffT c mods '[] IO (STM (SystemEvent mods))

  handleEvents :: ConsFDataList c mods => SystemEvent mods -> EffT c mods '[] IO ()

  beforeSystem :: ConsFDataList c mods => EffT c mods '[] IO ()

  afterSystem  :: ConsFDataList c mods => EffT c mods '[] IO ()

  releaseSystem :: ConsFDataList c mods => EffT c mods '[] IO ()
  -- ^ safely release all resources system acquired
  -- Warning: releaseSystem is done in reverse order of initAllModules
  -- i.e. the head of the list is the first to be released

class System mods => SystemEnv mods where
  readSystemInitDataFromEnv :: ConsFDataList c mods => SystemInitDataHardCode c mods -> EffT c '[] '[SystemError] IO (SystemInitData c mods)

class System mods => SystemArgs mods where
  readSystemInitDataFromArgs :: ConsFDataList c mods => SystemInitDataHardCode c mods -> [String] -> EffT c '[] '[SystemError] IO (SystemInitData c mods)

-- | base case for system
instance System '[] where
  initAllModules _ = return (fNil, fNil)
  {-# INLINE initAllModules #-}

  listenToEvents = return empty
  {-# INLINE listenToEvents #-}

  handleEvents _ = return ()
  {-# INLINE handleEvents #-}

  beforeSystem = return ()
  {-# INLINE beforeSystem #-}

  afterSystem = return ()
  {-# INLINE afterSystem #-}

  releaseSystem = return ()
  {-# INLINE releaseSystem #-}

instance SystemEnv '[] where
  readSystemInitDataFromEnv _ = return fNil
  {-# INLINE readSystemInitDataFromEnv #-}

instance SystemArgs '[] where
  readSystemInitDataFromArgs _ _ = do
    return fNil
  {-# INLINE readSystemInitDataFromArgs #-}

-- | Inductive instance for system
instance (Module mod, System mods, Loadable mod mods) => System (mod ': mods) where
  initAllModules (x :*** xs) = do
    (rs, ss)  <- initAllModules xs -- >>= _
    (er, ss') <- liftIO $ runEffT rs ss $ initModule @mod x
    case er of
      RSuccess (r', s') -> return (r' :*** rs, s' :*** ss')
      RFailure (EHead e) -> effThrowSystem e
  {-# INLINE initAllModules #-}

  beforeSystem = do
    embedEffT $ beforeSystem @mods
    beforeEvent @mod
  {-# INLINE beforeSystem #-}

  afterSystem = do
    embedEffT $ afterSystem @mods
    afterEvent @mod
  {-# INLINE afterSystem #-}

  listenToEvents = do
    tailEvents <- embedEffT $ listenToEvents @mods
    headEvent  <- moduleEvent @mod
    return $ UHead <$> headEvent <|> UTail <$> tailEvents
  {-# INLINE listenToEvents #-}

  handleEvents (UHead x) = handleEvent @mod x
  handleEvents (UTail xs) = embedEffT $ handleEvents @mods xs
  {-# INLINE handleEvents #-}

  releaseSystem = do
    releaseModule @mod
    embedEffT $ releaseSystem @mods
  {-# INLINE releaseSystem #-}

instance (SubList c mods (mod:mods), Module mod, SystemEnv mods, Loadable mod mods, LoadableEnv mod mods) => SystemEnv (mod ': mods) where
  readSystemInitDataFromEnv (im :*** ims) = do
    xs <- readSystemInitDataFromEnv @mods ims
    x  <- readInitDataFromEnv @mod @mods im
    return $ x :*** xs
  {-# INLINE readSystemInitDataFromEnv #-}

instance (SubList c mods (mod:mods), Module mod, SystemArgs mods, Loadable mod mods, LoadableArgs mod mods) => SystemArgs (mod ': mods) where
  readSystemInitDataFromArgs (im :*** ims) args = do
    xs <- readSystemInitDataFromArgs @mods ims args
    x  <- readInitDataFromArgs @mod @mods im args
    return $ x :*** xs
  {-# INLINE readSystemInitDataFromArgs #-}

-- | Declare that the computation has no error, i.e. it is safe to run
declareNoError :: Monad m => EffT c mods es m a -> EffT c mods NoError m a
declareNoError eff = eff `effCatchAll` \_es -> error "unsafeDeclareNoError: declared NoError, but got errors"
{-# INLINE declareNoError #-}

-- | lift IO action into EffT, catch IOException and return as Left, synonym for effIOSafe
liftIOException :: IO a -> EffT c mods '[IOException] IO a
liftIOException = liftIOAt
{-# INLINE liftIOException #-}

liftIOAt :: Exception e => IO a -> EffT c mods '[e] IO a
liftIOAt = liftIOSafeWith id
{-# INLINE liftIOAt #-}

liftIOText :: forall s mods c a. (Text -> Text) -> IO a -> EffT c mods '[ErrorText s] IO a
liftIOText err = liftIOSafeWith (\(e :: SomeException) -> ErrorText $ err $ pack $ show e)
{-# INLINE liftIOText #-}

-- | lift IO action into EffT, catch SomeException, turn it into Text
-- and prepend error message into ErrorText s
--
-- example: `liftIOPrepend @"File" "File error:" $ readFile "file.txt"`
liftIOPrepend :: forall s mods c a. Text -> IO a -> EffT c mods '[ErrorText s] IO a
liftIOPrepend err = liftIOText (err <>)
{-# INLINE liftIOPrepend #-}

-- | lift IO action into EffT, catch IOException into a custom error and return as Left
liftIOSafeWith :: Exception e' => (e' -> e) -> IO a -> EffT c mods '[e] IO a
liftIOSafeWith f io = EffT $ \_ s -> do
  a :: Either e' a <- E.try io
  case a of
    Right a' -> return (RSuccess a', s)
    Left e'  -> return (RFailure $ EHead $ f e', s)
{-# INLINE liftIOSafeWith #-}

-- | Convert the first error in the effect to Either
errorToEither :: Monad m => EffT c mods (e : es) m a -> EffT c mods es m (Either e a)
errorToEither eff = EffT $ \rs ss -> do
  (eResult, stateMods) <- unEffT eff rs ss
  case eResult of
    RSuccess a          -> return (RSuccess (Right a), stateMods)
    RFailure (EHead e)  -> return (RSuccess (Left e), stateMods)
    RFailure (ETail es) -> return (RFailure es, stateMods)
{-# INLINE errorToEither #-}

-- | Convert all errors to Either
errorToEitherAll :: Monad m => EffT c mods es m a -> EffT c mods NoError m (Either (EList es) a)
errorToEitherAll eff = EffT $ \rs ss -> do
  (eResult, stateMods) <- unEffT eff rs ss
  case eResult of
    RSuccess a    -> return (RSuccess (Right a), stateMods)
    RFailure es   -> return (RSuccess (Left es), stateMods)
{-# INLINE errorToEitherAll #-}

effCatchSystem :: (Monad m, In' c SystemError es) => EffT c mods es m a -> (SystemError -> EffT c mods es m a) -> EffT c mods es m a
effCatchSystem eff h = EffT $ \rs ss -> do
  (eResult, stateMods) <- (unEffT eff) rs ss
  case eResult of
    RSuccess a  -> return (RSuccess a, stateMods)
    RFailure es | Just e@(SystemErrorException _) <- getEMaybe es -> do
      unEffT (h e) rs ss
    RFailure es -> return (RFailure es, stateMods)
{-# INLINE effCatchSystem #-}

effCatch :: Monad m => EffT c mods (e : es) m a -> (e -> EffT c mods es m a) -> EffT c mods es m a
effCatch eff h = EffT $ \rs ss -> do
  (eResult, stateMods) <- (unEffT eff) rs ss
  case eResult of
    RSuccess a          -> return (RSuccess a, stateMods)
    RFailure (EHead e)  -> (unEffT $ h e) rs ss
    RFailure (ETail es) -> return (RFailure $ es, stateMods)
{-# INLINE effCatch #-}

effCatchIn
  :: forall e es mods m c a es'. (Monad m, In e es, es' ~ Remove (FirstIndex e es) es)
  => EffT c mods es m a -> (e -> EffT c mods es' m a) -> EffT c mods es' m a
effCatchIn eff h = EffT $ \rs ss -> do
  (eResult, stateMods) <- (unEffT eff) rs ss
  case getElemRemoveResult (singIndex @e @es) eResult of
    Left e -> case proofIndex @e @es of
      Refl -> unEffT (h e) rs ss
    Right eResult' -> return (eResult', stateMods)
{-# INLINE effCatchIn #-}

effCatchAll :: Monad m => EffT c mods es m a -> (EList es -> EffT c mods NoError m a) -> EffT c mods NoError m a
effCatchAll eff h = EffT $ \rs ss -> do
  (er, stateMods) <- (unEffT eff) rs ss
  case er of
    RSuccess a    -> return (RSuccess a, stateMods)
    RFailure es   -> (unEffT $ h es) rs ss
{-# INLINE effCatchAll #-}

effThrow :: Monad m => In e es => e -> EffT c mods es m a
effThrow e = EffT $ \_ s -> pure (RFailure $ embedE e, s)
{-# INLINE effThrow #-}

effThrowSystem :: forall es mods m c a. Monad m => In SystemError es => SystemError -> EffT c mods es m a
effThrowSystem = effThrow
{-# INLINE effThrowSystem #-}

effEitherWith :: (Monad m) => (e -> e') -> EffT c mods es m (Either e a) -> EffT c mods (e' : es) m a
effEitherWith f eff = EffT $ \rs ss -> do
  (eResult, stateMods) <- unEffT eff rs ss
  case eResult of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left e)  -> return (RFailure $ EHead $ f e, stateMods)
    RFailure sysE      -> return (RFailure $ ETail sysE, stateMods)
{-# INLINE effEitherWith #-}

effEither :: Monad m => EffT c mods es m (Either e a) -> EffT c mods (e : es) m a
effEither = effEitherWith id
{-# INLINE effEither #-}

effMaybeWith :: forall e es m mods c a. Monad m => e -> EffT c mods es m (Maybe a) -> EffT c mods (e : es) m a
effMaybeWith e eff = EffT $ \rs ss -> do
  (eResult, stateMods) <- unEffT eff rs ss
  case eResult of
    RSuccess (Just a) -> return (RSuccess a, stateMods)
    RSuccess Nothing  -> return (RFailure $ EHead e, stateMods)
    RFailure sysE     -> return (RFailure $ ETail sysE, stateMods)
{-# INLINE effMaybeWith #-}

effMaybeInWith :: (In e es, Monad m) => e -> EffT c mods es m (Maybe a) -> EffT c mods es m a
effMaybeInWith e eff = EffT $ \rs ss -> do
  (eResult, stateMods) <- unEffT eff rs ss
  case eResult of
    RSuccess (Just a) -> return (RSuccess a, stateMods)
    RSuccess Nothing  -> return (RFailure $ embedE e, stateMods)
    RFailure sysE     -> return (RFailure sysE, stateMods)
{-# INLINE effMaybeInWith #-}

effEitherSystemWith :: (Monad m, In SystemError es) => (e -> SystemError) -> EffT c mods es m (Either e a) -> EffT c mods es m a
effEitherSystemWith f eff = EffT $ \rs ss -> do
  (eResult, stateMods) <- unEffT eff rs ss
  case eResult of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left e)  -> return (RFailure $ embedE $ f e, stateMods)
    RFailure sysE      -> return (RFailure sysE, stateMods)
{-# INLINE effEitherSystemWith #-}

effEitherSystemException :: (Monad m, In SystemError es, Exception e) => EffT c mods es m (Either e a) -> EffT c mods es m a
effEitherSystemException = effEitherSystemWith (SystemErrorException . toException)
{-# INLINE effEitherSystemException #-}

effEitherInWith :: (Monad m, In e' es) => (e -> e') -> EffT c mods es m (Either e a) -> EffT c mods es m a
effEitherInWith f eff = EffT $ \rs ss -> do
  (eResult, stateMods) <- unEffT eff rs ss
  case eResult of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left e)  -> return (RFailure $ embedE $ f e, stateMods)
    RFailure sysE      -> return (RFailure sysE, stateMods)
{-# INLINE effEitherInWith #-}

effEitherIn :: (Monad m, In e es) => EffT c mods es m (Either e a) -> EffT c mods es m a
effEitherIn = effEitherInWith id
{-# INLINE effEitherIn #-}
