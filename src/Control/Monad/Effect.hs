{-# LANGUAGE DerivingVia, UndecidableInstances, AllowAmbiguousTypes, LinearTypes #-}
module Control.Monad.Effect
  ( -- * Effectful computation
    Eff
  , embedEff, embedMods, embedError
  , runEff, runEff_, runEff0, runEff01, runEff00
  , runEffWithInitData, runEffNoError
  , runEffOuter, runEffOuter_
  , effCatch, effCatchAll, effCatchSystem
  , effThrow, effThrowSystem
  , effEither, effEitherWith
  , effEitherIn, effEitherInWith
  , effEitherSystemWith, effEitherSystemException
  , effMaybeWith, effMaybeInWith
  , liftIOSafe, effIOSafe
  , liftIOSafeAt, effIOSafeAt
  , liftIOSafeWith, effIOSafeWith

  -- * Module and System
  , Module(..), System(..), Loadable(..)
  , queryModule, queriesModule
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
  ) where

import Data.HList
import Data.Bifunctor
import Data.Kind
import Data.Text (Text)
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.RST
import Control.Monad.Catch
import Control.Exception as E

-- | Effectful computation, using modules as units of effect
newtype Eff mods es a = Eff { unEff :: SystemRead mods -> SystemState mods -> IO (Result es a, SystemState mods) }

instance Functor (Eff mods es) where
  fmap f (Eff eff) = Eff $ \rs ss -> first (fmap f) <$> eff rs ss
  {-# INLINE fmap #-}

instance Applicative (Eff mods es) where
  pure a = Eff $ \_ ss -> return (RSuccess a, ss)
  {-# INLINE pure #-}

  Eff effF <*> Eff effA = Eff $ \rs ss -> do
    (eF, ss1) <- effF rs ss
    case eF of
      RSuccess f -> do
        (eA, ss2) <- effA rs ss1
        case eA of
          RSuccess a -> return (RSuccess (f a), ss2)
          RFailure es -> return (RFailure $ es, ss2)
      RFailure es -> return (RFailure es, ss1)
  {-# INLINE (<*>) #-}

instance Monad (Eff mods es) where
  Eff eff >>= f = Eff $ \rs ss -> do
    (eResult, ss1) <- eff rs ss
    case eResult of
      RSuccess a  -> unEff (f a) rs ss1
      RFailure es -> return (RFailure es, ss1)
  {-# INLINE (>>=) #-}

instance MonadIO (Eff mods es) where
  liftIO io = Eff $ \_ ss -> do
    a <- io
    return (RSuccess a, ss)
  {-# INLINE liftIO #-}

instance MonadReadable (SystemRead mods) (Eff mods es) where
  query = Eff $ \rs ss -> return (RSuccess rs, ss)
  {-# INLINE query #-}
  local f (Eff eff) = Eff $ \rs ss -> eff (f rs) ss
  {-# INLINE local #-}

instance MonadStateful (SystemState mods) (Eff mods es) where
  get = Eff $ \_ ss -> return (RSuccess ss, ss)
  {-# INLINE get #-}
  put ss = Eff $ \_ _ -> return (RSuccess (), ss)
  {-# INLINE put #-}
  modify f = Eff $ \_ ss -> return (RSuccess (), f ss)
  {-# INLINE modify #-}

-- | The error in throwM is thrown to the top level as SystemErrorException SomeException
instance (SubList mods mods, SubList es es, SubList es (SystemError : es), In SystemError es) => MonadThrow (Eff mods es) where
  throwM = embedEff @mods @mods @es @es . effThrowSystem @es . SystemErrorException . toException
  {-# INLINE throwM #-}

-- | this can only catch SystemErrorException SomeException, other errors are algebraic
instance (SubList mods mods, SubList es es, SubList es (SystemError : es), In SystemError es) => MonadCatch (Eff mods es) where
  catch ma handler = effCatchSystem ma $ \case
    SystemErrorException e -> case fromException e of
      Just e' -> handler e'
      Nothing -> embedEff @mods @mods . effThrowSystem @es $ SystemErrorException e
    e                      -> effThrow e
  {-# INLINE catch #-}

-- | embed smaller effect into larger effect
embedEff :: forall mods mods' es es' a. (SubList mods mods', SubList es es')
  => Eff mods es a -> Eff mods' es' a
embedEff eff = Eff $ \rs' ss' -> do
  let rs = getSubListF rs'
      ss = getSubListF @mods ss'
      modsEff = unEff eff
  (emods, ss1) <- modsEff rs ss
  return (subListErrorEmbed emods, subListUpdateF ss' ss1)
{-# INLINE embedEff #-}

embedMods :: (SubList mods mods', SubList es es, NotIn SystemError es)
  => Eff mods es a -> Eff mods' es a
embedMods = embedEff
{-# INLINE embedMods #-}

embedError :: (SubList mods mods, SubList es es', NotIn SystemError es')
  => Eff mods es a -> Eff mods es' a
embedError = embedEff
{-# INLINE embedError #-}

runEff :: forall mods es a
  .  SystemRead mods
  -> SystemState mods
  -> Eff mods es a
  -> IO (Result es a, SystemState mods)
runEff rs ss eff = unEff eff rs ss
{-# INLINE runEff #-}

runEff_ :: forall mods es a
  .  SystemRead mods
  -> SystemState mods
  -> Eff mods es a
  -> IO (Result es a)
runEff_ rs ss eff = fst <$> runEff rs ss eff
{-# INLINE runEff_ #-}

-- | If error happens at initialization, it will return Left SystemError
-- If error happens during event loop, it will return Right (Left (SList (SystemError : es)))
runEffWithInitData :: forall mods es a. System mods
  => SystemInitData mods
  -> Eff mods es a
  -> IO 
      (Either SystemError -- ^ if error happens at initialization
        ( Result es a -- ^ if error happens during event loop
        , SystemState mods)
      )
runEffWithInitData initData eff = do
  runEff0 (initAllModules @mods initData) >>= \case
    RSuccess (rs, ss)  -> Right <$> runEff rs ss eff
    RFailure (EHead e) -> return $ Left e

runEffNoError :: forall mods a
  .  SystemRead mods
  -> SystemState mods
  -> Eff mods NoError a
  -> IO (a, SystemState mods)
runEffNoError rs ss eff = first resultNoError <$> unEff eff rs ss
{-# INLINE runEffNoError #-}

runEff0 :: Eff '[] es a -> IO (Result es a)
runEff0 = fmap fst . runEff FNil FNil
{-# INLINE runEff0 #-}

runEff01 :: Eff '[] '[e] a -> IO (Either e a)
runEff01 = fmap (first fromElistSingleton . resultToEither) . runEff0

runEff00 :: Eff '[] NoError a -> IO a
runEff00 = fmap resultNoError . runEff0
{-# INLINE runEff00 #-}

-- | Warning: state will lose when you have an error
runEffOuter :: forall mod mods es a. ModuleRead mod -> ModuleState mod -> Eff (mod : mods) es a -> Eff mods es (ModuleState mod, a)
runEffOuter mread mstate eff = Eff
  $ \modsRead modsState ->
    (\(ea, (s :** ss)) -> ((s,) <$> ea, ss)) <$> (unEff @(mod:mods) eff) (mread :** modsRead) (mstate :** modsState)
{-# INLINE runEffOuter #-}

runEffOuter_ :: forall mod mods es a. ModuleRead mod -> ModuleState mod -> Eff (mod : mods) es a -> Eff mods es a
runEffOuter_ mread mstate eff = Eff
  $ \modsRead modsState ->
    (\(ea, (_ :** ss)) -> (ea, ss)) <$> (unEff @(mod:mods) eff) (mread :** modsRead) (mstate :** modsState)
{-# INLINE runEffOuter_ #-}

-------------------------------------- instances --------------------------------------

class Module mod where
  data ModuleInitData mod :: Type
  data ModuleRead     mod :: Type
  data ModuleState    mod :: Type
  data ModuleEvent    mod :: Type

type SystemInitDataHardCode mods = FList ModuleInitDataHardCode mods
type SystemInitData mods = FList ModuleInitData mods
type SystemState    mods = FList ModuleState    mods
type SystemRead     mods = FList ModuleRead     mods
type SystemEvent    mods = UList ModuleEvent    mods
data SystemError
  = SystemErrorException SomeException
  | SystemErrorText      Text
  deriving Show

type NoError = '[]

queryModule :: forall mod mods es. (In mod mods, Module mod) => Eff mods es (ModuleRead mod)
queryModule = queries @(SystemRead mods) (getF @mod)
{-# INLINE queryModule #-}

queriesModule :: forall mod mods es a. (In mod mods, Module mod) => (ModuleRead mod -> a) -> Eff mods es a
queriesModule f = f <$> queryModule @mod
{-# INLINE queriesModule #-}

getModule :: forall mod mods es. (In mod mods, Module mod) => Eff mods es (ModuleState mod)
getModule = gets @(SystemState mods) (getF @mod)
{-# INLINE getModule #-}

getsModule :: forall mod mods es a. (In mod mods, Module mod) => (ModuleState mod -> a) -> Eff mods es a
getsModule f = f <$> getModule @mod
{-# INLINE getsModule #-}

putModule :: forall mod mods es. (In mod mods, Module mod) => ModuleState mod -> Eff mods es ()
putModule x = modify @(SystemState mods) (modifyF $ const x)
{-# INLINE putModule #-}

modifyModule :: forall mod mods es. (In mod mods, Module mod) => (ModuleState mod -> ModuleState mod) -> Eff mods es ()
modifyModule f = modify @(SystemState mods) (modifyF f)
{-# INLINE modifyModule #-}

-- | Specifies that the module can load after mods are loaded
-- in practice we could use
-- instance SomeModuleWeNeed `In` mods => Loadable mods SomeModuleToLoad
class Loadable mod mods where
  initModule  :: ModuleInitData mod -> Eff mods '[SystemError] (ModuleRead mod, ModuleState mod)

  beforeEvent :: Eff (mod : mods) NoError ()
  beforeEvent = return ()
  {-# INLINE beforeEvent #-}

  afterEvent  :: Eff (mod : mods) NoError ()
  afterEvent = return ()
  {-# INLINE afterEvent #-}

  moduleEvent :: Eff (mod : mods) NoError (STM (ModuleEvent mod))
  moduleEvent = return empty
  {-# INLINE moduleEvent #-}

  handleEvent :: ModuleEvent mod -> Eff (mod : mods) NoError ()
  handleEvent _ = return ()
  {-# INLINE handleEvent #-}

  releaseModule :: Eff (mod : mods) NoError () -- ^ release resources, quit module
  releaseModule = return ()
  {-# INLINE releaseModule #-}

data family ModuleInitDataHardCode mod :: Type

class Loadable mod mods => LoadableEnv mod mods where
  readInitDataFromEnv :: ModuleInitDataHardCode mod -> Eff '[] '[SystemError] (ModuleInitData mod)
  -- ^ Read module init data from environment, or other means
  -- one can change this to SystemInitData mods -> IO (ModuleInitData mod)

class Loadable mod mods => LoadableArgs mod mods where
  readInitDataFromArgs :: ModuleInitDataHardCode mod -> [String] -> Eff '[] '[SystemError] (ModuleInitData mod)
  -- ^ Read module init data from command line arguments, or using comand line arguments to read other things
  -- one can change this to SystemInitData mods -> [String] -> IO (ModuleInitData mod)

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
--
-- the last module in the list is the first to be loaded
-- and also the first to execute beforeEvent and afterEvent
class System mods where
  initAllModules :: SystemInitData mods -> Eff '[] '[SystemError] (SystemRead mods, SystemState mods)

  listenToEvents :: Eff mods '[] (STM (SystemEvent mods))

  handleEvents :: SystemEvent mods -> Eff mods '[] ()

  beforeSystem :: Eff mods '[] ()

  afterSystem  :: Eff mods '[] ()

  releaseSystem :: Eff mods '[] ()
  -- ^ safely release all resources system acquired
  -- Warning: releaseSystem is done in reverse order of initAllModules
  -- i.e. the head of the list is the first to be released

class System mods => SystemEnv mods where
  readSystemInitDataFromEnv :: SystemInitDataHardCode mods -> Eff '[] '[SystemError] (SystemInitData mods)

class System mods => SystemArgs mods where
  readSystemInitDataFromArgs :: SystemInitDataHardCode mods -> [String] -> Eff '[] '[SystemError] (SystemInitData mods)

-- | base case for system
instance System '[] where
  initAllModules _ = return (FNil, FNil)
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
  readSystemInitDataFromEnv _ = return FNil
  {-# INLINE readSystemInitDataFromEnv #-}

instance SystemArgs '[] where
  readSystemInitDataFromArgs _ _ = do
    return FNil
  {-# INLINE readSystemInitDataFromArgs #-}

-- | Inductive instance for system
instance (SubList mods (mod:mods), Module mod, System mods, Loadable mod mods) => System (mod ': mods) where
  initAllModules (x :** xs) = do
    (rs, ss)  <- initAllModules xs -- >>= _
    (er, ss') <- liftIO $ runEff rs ss $ initModule @mod x
    case er of
      RSuccess (r', s') -> return (r' :** rs, s' :** ss')
      RFailure (EHead e) -> effThrowSystem e
  {-# INLINE initAllModules #-}

  beforeSystem = do
    embedEff $ beforeSystem @mods
    beforeEvent @mod
  {-# INLINE beforeSystem #-}

  afterSystem = do
    embedEff $ afterSystem @mods
    afterEvent @mod
  {-# INLINE afterSystem #-}

  listenToEvents = do
    tailEvents <- embedEff $ listenToEvents @mods
    headEvent  <- moduleEvent @mod
    return $ UHead <$> headEvent <|> UTail <$> tailEvents
  {-# INLINE listenToEvents #-}

  handleEvents (UHead x) = handleEvent @mod x
  handleEvents (UTail xs) = embedEff $ handleEvents @mods xs
  {-# INLINE handleEvents #-}

  releaseSystem = do
    releaseModule @mod
    embedEff $ releaseSystem @mods
  {-# INLINE releaseSystem #-}

instance (SubList mods (mod:mods), Module mod, SystemEnv mods, Loadable mod mods, LoadableEnv mod mods) => SystemEnv (mod ': mods) where
  readSystemInitDataFromEnv (im :** ims) = do
    xs <- readSystemInitDataFromEnv @mods ims
    x  <- readInitDataFromEnv @mod @mods im
    return $ x :** xs
  {-# INLINE readSystemInitDataFromEnv #-}

instance (SubList mods (mod:mods), Module mod, SystemArgs mods, Loadable mod mods, LoadableArgs mod mods) => SystemArgs (mod ': mods) where
  readSystemInitDataFromArgs (im :** ims) args = do
    xs <- readSystemInitDataFromArgs @mods ims args
    x  <- readInitDataFromArgs @mod @mods im args
    return $ x :** xs
  {-# INLINE readSystemInitDataFromArgs #-}

-- | lift IO action into Eff, catch IOException and return as Left, synonym for effIOSafe
liftIOSafe :: IO a -> Eff mods '[IOException] a
liftIOSafe = effIOSafe
{-# INLINE liftIOSafe #-}

liftIOSafeAt :: Exception e => IO a -> Eff mods '[e] a
liftIOSafeAt = effIOSafeAt
{-# INLINE liftIOSafeAt #-}

-- | lift IO action into Eff, catch IOException and return as Left
effIOSafe :: IO a -> Eff mods '[IOException] a
effIOSafe = effIOSafeWith id
{-# INLINE effIOSafe #-}

effIOSafeAt :: Exception e => IO a -> Eff mods '[e] a
effIOSafeAt = effIOSafeWith id
{-# INLINE effIOSafeAt #-}

-- | lift IO action into Eff, catch IOException into a custom error and return as Left
liftIOSafeWith :: (IOException -> e) -> IO a -> Eff mods '[e] a
liftIOSafeWith = effIOSafeWith
{-# INLINE liftIOSafeWith #-}

-- | lift IO action into Eff, catch IOException into a custom error and return as Left
effIOSafeWith :: Exception e' => (e' -> e) -> IO a -> Eff mods '[e] a
effIOSafeWith f io = Eff $ \_ s -> do
  a :: Either e' a <- E.try io
  case a of
    Right a' -> return (RSuccess a', s)
    Left e'  -> return (RFailure $ EHead $ f e', s)
{-# INLINE effIOSafeWith #-}

effCatchSystem :: (SystemError `In` es) => Eff mods es a -> (SystemError -> Eff mods es a) -> Eff mods es a
effCatchSystem eff h = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- (unEff eff) rs ss
  case eS_E_Es of
    RSuccess a  -> return (RSuccess a, stateMods)
    RFailure es | Just e@(SystemErrorException _) <- getEMaybe es -> do
      unEff (h e) rs ss
    RFailure es -> return (RFailure es, stateMods)
{-# INLINE effCatchSystem #-}

effCatch :: Eff mods (e : es) a -> (e -> Eff mods es a) -> Eff mods es a
effCatch eff h = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- (unEff eff) rs ss
  case eS_E_Es of
    RSuccess a                  -> return (RSuccess a, stateMods)
    RFailure (EHead e)  -> (unEff $ h e) rs ss
    RFailure (ETail es) -> return (RFailure $ es, stateMods)
  -- return (_ , stateMods)
{-# INLINE effCatch #-}

effCatchAll :: Eff mods es a -> (EList es -> Eff mods NoError a) -> Eff mods NoError a
effCatchAll eff h = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- (unEff eff) rs ss
  case eS_E_Es of
    RSuccess a    -> return (RSuccess a, stateMods)
    RFailure es   -> (unEff $ h es) rs ss
{-# INLINE effCatchAll #-}

effThrow :: In e es => e -> Eff mods es a
effThrow e = Eff $ \_ s -> pure (RFailure $ embedE e, s)
{-# INLINE effThrow #-}

effThrowSystem :: In SystemError es => SystemError -> Eff mods es a
effThrowSystem = effThrow
{-# INLINE effThrowSystem #-}

-- proofSubList :: EList (a : cs) -> EList (a : b : cs)
-- proofSubList (EHead x)  = EHead x
-- proofSubList (ETail xs) = ETail $ ETail xs
-- {-# INLINE proofSubList #-}

effEitherWith :: (e -> e') -> Eff mods es (Either e a) -> Eff mods (e' : es) a
effEitherWith f eff = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- unEff eff rs ss
  case eS_E_Es of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left e)  -> return (RFailure $ EHead $ f e, stateMods)
    RFailure sysE      -> return (RFailure $ ETail sysE, stateMods)
{-# INLINE effEitherWith #-}

effEither :: Eff mods es (Either e a) -> Eff mods (e : es) a
effEither = effEitherWith id
{-# INLINE effEither #-}

effMaybeWith :: e -> Eff mods es (Maybe a) -> Eff mods (e : es) a
effMaybeWith e eff = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- unEff eff rs ss
  case eS_E_Es of
    RSuccess (Just a) -> return (RSuccess a, stateMods)
    RSuccess Nothing  -> return (RFailure $ EHead e, stateMods)
    RFailure sysE     -> return (RFailure $ ETail sysE, stateMods)
{-# INLINE effMaybeWith #-}

effMaybeInWith :: (In e es) => e -> Eff mods es (Maybe a) -> Eff mods es a
effMaybeInWith e eff = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- unEff eff rs ss
  case eS_E_Es of
    RSuccess (Just a) -> return (RSuccess a, stateMods)
    RSuccess Nothing  -> return (RFailure $ embedE e, stateMods)
    RFailure sysE     -> return (RFailure sysE, stateMods)
{-# INLINE effMaybeInWith #-}

effEitherSystemWith :: In SystemError es => (e -> SystemError) -> Eff mods es (Either e a) -> Eff mods es a
effEitherSystemWith f eff = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- unEff eff rs ss
  case eS_E_Es of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left e)  -> return (RFailure $ embedE $ f e, stateMods)
    RFailure sysE   -> return (RFailure sysE, stateMods)
{-# INLINE effEitherSystemWith #-}

effEitherSystemException :: (In SystemError es, Exception e) => Eff mods es (Either e a) -> Eff mods es a
effEitherSystemException = effEitherSystemWith (SystemErrorException . toException)
{-# INLINE effEitherSystemException #-}

effEitherInWith :: (In e' es) => (e -> e') -> Eff mods es (Either e a) -> Eff mods es a
effEitherInWith f eff = Eff $ \rs ss -> do
  (eS_E_Es, stateMods) <- unEff eff rs ss
  case eS_E_Es of
    RSuccess (Right a) -> return (RSuccess a, stateMods)
    RSuccess (Left e)  -> return (RFailure $ embedE $ f e, stateMods)
    RFailure sysE      -> return (RFailure sysE, stateMods)
{-# INLINE effEitherInWith #-}

effEitherIn :: (In e es) => Eff mods es (Either e a) -> Eff mods es a
effEitherIn = effEitherInWith id
{-# INLINE effEitherIn #-}
