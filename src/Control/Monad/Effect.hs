{-# LANGUAGE DerivingVia, UndecidableInstances, AllowAmbiguousTypes, LinearTypes #-}
module Control.Monad.Effect
  ( -- * Effectful computation
    Eff(..), embedEff, embedMods, embedError
  , runEff, runEffWithInitData, runEffNoError, runEff0
  , runEffOuter, runEffOuter_
  , effCatch, effCatchAll, effCatchSystem
  , effThrow, effThrowSystem
  , effEither, effEitherWith
  , effEitherIn, effEitherInWith
  , effEitherSystemWith, effEitherSystemException
  , effMaybeWith, effMaybeInWith
  , liftIOSafe, effIOSafe, liftIOSafeWith, effIOSafeWith

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

-- new design idea:
-- Remove current SystemError, creating a new type SystemError that is top level and is 
-- used to break the event loop. Other errors should not be present on top level.
--
-- so there should be an error list field in Eff, on system level they need to be empty
-- forcing the user to handle all errors inside module level, or throw them to the top level
--

-- | Effectful computation, using modules as units of effect
newtype Eff mods es a = Eff { unEff :: RSE (SystemRead mods) (SystemState mods) (SList (SystemError : es)) IO a }
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO
    , MonadReadable (SystemRead mods)
    , MonadStateful (SystemState mods)
    )

-- | The error in throwM is thrown to the top level as SystemErrorException SomeException
instance (SubList mods mods, NotIn SystemError es) => MonadThrow (Eff mods es) where
  throwM = embedEff @mods @mods . effThrowSystem . SystemErrorException . toException
  {-# INLINE throwM #-}

-- | this can only catch SystemErrorException SomeException, other errors are algebraic
instance (SubList mods mods, SystemError `NotIn` es) => MonadCatch (Eff mods es) where
  catch ma handler = effCatchSystem ma $ \case
    SystemErrorException e -> case fromException e of
      Just e' -> handler e'
      Nothing -> embedEff @mods @mods . effThrowSystem $ SystemErrorException e
    e                      -> effThrow e
  {-# INLINE catch #-}

-- | embed smaller effect into larger effect
embedEff :: forall mods mods' es es' a. (SubList mods mods', SubList es (SystemError : es'), NotIn SystemError es')
  => Eff mods es a -> Eff mods' es' a
embedEff eff = Eff $ RSE $ \rs' ss' -> do
  let rs = getSubListF rs'
      ss = getSubListF @mods ss'
      modsEff = runRSE $ unEff eff
  (emods, ss1) <- modsEff rs ss
  return (first subListEmbed emods, subListUpdateF ss' ss1)
{-# INLINE embedEff #-}

embedMods :: (SubList mods mods', SubList es (SystemError : es), NotIn SystemError es)
  => Eff mods es a -> Eff mods' es a
embedMods = embedEff
{-# INLINE embedMods #-}

embedError :: (SubList mods mods, SubList es (SystemError : es'), NotIn SystemError es')
  => Eff mods es a -> Eff mods es' a
embedError = embedEff
{-# INLINE embedError #-}

runEff :: forall mods es a
  .  SystemRead mods
  -> SystemState mods
  -> Eff mods es a
  -> IO (Either (SList (SystemError : es)) a, SystemState mods)
runEff rs ss eff = runRSE (unEff eff) rs ss
{-# INLINE runEff #-}

-- | If error happens at initialization, it will return Left SystemError
-- If error happens during event loop, it will return Right (Left (SList (SystemError : es)))
runEffWithInitData :: forall mods es a. (System mods)
  => SystemInitData mods
  -> Eff mods es a
  -> IO 
      (Either SystemError -- ^ if error happens at initialization
        ( Either (SList (SystemError : es)) a -- ^ if error happens during event loop
        , SystemState mods)
      )
runEffWithInitData initData eff = do
  initAllModules @mods initData >>= \case
    Right (rs, ss) -> Right <$> runEff rs ss eff
    Left e         -> return $ Left e

runEffNoError :: forall mods a
  .  SystemRead mods
  -> SystemState mods
  -> Eff mods NoError a
  -> IO (Either SystemError a, SystemState mods)
runEffNoError rs ss eff = first (first $ \case
    SHead e -> e
    _       -> SystemErrorText "SEmpty error"
  ) <$> runRSE (unEff eff) rs ss

runEff0 :: Eff '[] '[] a -> IO (Either SystemError a)
runEff0 = fmap fst . runEffNoError FNil FNil
{-# INLINE runEff0 #-}

-- | Warning: state will lose when you have an error
runEffOuter :: forall mod mods es a. ModuleRead mod -> ModuleState mod -> Eff (mod : mods) es a -> Eff mods es (ModuleState mod, a)
runEffOuter mread mstate eff = Eff . RSE
  $ \modsRead modsState ->
    (\(ea, (s :** ss)) -> ((s,) <$> ea, ss)) <$> runRSE (unEff @(mod:mods) eff) (mread :** modsRead) (mstate :** modsState)

runEffOuter_ :: forall mod mods es a. ModuleRead mod -> ModuleState mod -> Eff (mod : mods) es a -> Eff mods es a
runEffOuter_ mread mstate eff = Eff . RSE
  $ \modsRead modsState ->
    (\(ea, (_ :** ss)) -> (ea, ss)) <$> runRSE (unEff @(mod:mods) eff) (mread :** modsRead) (mstate :** modsState)

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
  initModule  :: ModuleInitData mod -> Eff mods NoError (ModuleRead mod, ModuleState mod)

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
  readInitDataFromEnv :: ModuleInitDataHardCode mod -> Eff '[] '[] (ModuleInitData mod)
  -- ^ Read module init data from environment, or other means
  -- one can change this to SystemInitData mods -> IO (ModuleInitData mod)

class Loadable mod mods => LoadableArgs mod mods where
  readInitDataFromArgs :: ModuleInitDataHardCode mod -> [String] -> Eff '[] '[] (ModuleInitData mod)
  -- ^ Read module init data from command line arguments, or using comand line arguments to read other things
  -- one can change this to SystemInitData mods -> [String] -> IO (ModuleInitData mod)

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
--
-- the last module in the list is the first to be loaded
-- and also the first to execute beforeEvent and afterEvent
class System mods where
  initAllModules :: SystemInitData mods -> IO (Either SystemError (SystemRead mods, SystemState mods))

  listenToEvents :: Eff mods '[] (STM (SystemEvent mods))

  handleEvents :: SystemEvent mods -> Eff mods '[] ()

  beforeSystem :: Eff mods '[] ()

  afterSystem  :: Eff mods '[] ()

  releaseSystem :: Eff mods '[] ()
  -- ^ safely release all resources system acquired
  -- Warning: releaseSystem is done in reverse order of initAllModules
  -- i.e. the head of the list is the first to be released

class System mods => SystemEnv mods where
  readSystemInitDataFromEnv :: SystemInitDataHardCode mods -> Eff '[] '[] (SystemInitData mods)

class System mods => SystemArgs mods where
  readSystemInitDataFromArgs :: SystemInitDataHardCode mods -> [String] -> Eff '[] '[] (SystemInitData mods)

-- | base case for system
instance System '[] where
  initAllModules _ = return $ Right (FNil, FNil)
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
    initAllModules xs >>= \case
      Right (rs, ss) -> do
        (er, ss') <- runRSE (unEff @mods $ initModule @mod x) rs ss
        case er of
          (Right (r', s'))      -> return $ Right (r' :** rs, s' :** ss')
          (Left (SHead sysE))   -> return $ Left sysE
          (Left SEmpty)         -> error "SEmpty should not be present in error"
          (Left (STail SEmpty)) -> error "SList '[] should not be present in error"
      Left e -> return $ Left e
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

-- | lift IO action into Eff, catch IOException into a custom error and return as Left
liftIOSafeWith :: (IOException -> e) -> IO a -> Eff mods '[e] a
liftIOSafeWith = effIOSafeWith
{-# INLINE liftIOSafeWith #-}

-- | lift IO action into Eff, catch IOException and return as Left
effIOSafe :: IO a -> Eff mods '[IOException] a
effIOSafe = effIOSafeWith id
{-# INLINE effIOSafe #-}

-- | lift IO action into Eff, catch IOException into a custom error and return as Left
effIOSafeWith :: (IOException -> e) -> IO a -> Eff mods '[e] a
effIOSafeWith f io = Eff $ RSE $ \_ s -> do
  a :: Either IOException a <- E.try io
  case a of
    Right a' -> return (Right a', s)
    Left e   -> return (Left $ STail $ SHead $ f e, s)
{-# INLINE effIOSafeWith #-}

effCatchSystem :: Eff mods es a -> (SystemError -> Eff mods es a) -> Eff mods es a
effCatchSystem eff h = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right a                 -> return (Right a, stateMods)
    Left (SHead sysE)       -> runRSE (unEff $ h sysE) rs ss
    Left (STail e)          -> return (Left $ STail e, stateMods)
    Left _                  -> error "SEmpty should not be present in error"
{-# INLINE effCatchSystem #-}

effCatch :: Eff mods (e : es) a -> (e -> Eff mods es a) -> Eff mods es a
effCatch eff h = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right a                 -> return (Right a, stateMods)
    Left (SHead sysE)       -> return (Left $ SHead sysE, stateMods)
    Left (STail (SHead e))  -> runRSE (unEff $ h e) rs ss
    Left (STail (STail es)) -> return (Left $ STail es, stateMods)
    Left _                  -> error "SEmpty should not be present in error"
  -- return (_ , stateMods)
{-# INLINE effCatch #-}

effCatchAll :: Eff mods es a -> (SList es -> Eff mods NoError a) -> Eff mods NoError a
effCatchAll eff h = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right a           -> return (Right a, stateMods)
    Left (STail es)   -> runRSE (unEff $ h es) rs ss
    Left (SHead sysE) -> return (Left $ SHead sysE, stateMods)
    Left _            -> error "SEmpty should not be present in error"
{-# INLINE effCatchAll #-}

effThrow :: In e (SystemError : es) => e -> Eff mods es a
effThrow e = Eff $ RSE $ \_ s -> pure (Left $ embedS e, s)
{-# INLINE effThrow #-}

effThrowSystem :: SystemError -> Eff mods '[] a
effThrowSystem = effThrow
{-# INLINE effThrowSystem #-}

proofSubList :: SList (a : cs) -> SList (a : b : cs)
proofSubList (SHead x) = SHead x
proofSubList (STail xs) = STail $ STail xs
proofSubList SEmpty = SEmpty
{-# INLINE proofSubList #-}

effEitherWith :: (e -> e') -> Eff mods es (Either e a) -> Eff mods (e' : es) a
effEitherWith f eff = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right (Right a) -> return (Right a, stateMods)
    Right (Left e)  -> return (Left $ STail $ SHead $ f e, stateMods)
    Left sysE       -> return (Left $ proofSubList sysE, stateMods)
{-# INLINE effEitherWith #-}

effEither :: Eff mods es (Either e a) -> Eff mods (e : es) a
effEither = effEitherWith id
{-# INLINE effEither #-}

effMaybeWith :: e -> Eff mods es (Maybe a) -> Eff mods (e : es) a
effMaybeWith e eff = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right (Just a) -> return (Right a, stateMods)
    Right Nothing  -> return (Left $ STail $ SHead e, stateMods)
    Left sysE      -> return (Left $ proofSubList sysE, stateMods)
{-# INLINE effMaybeWith #-}

effMaybeInWith :: (In e es) => e -> Eff mods es (Maybe a) -> Eff mods es a
effMaybeInWith e eff = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right (Just a) -> return (Right a, stateMods)
    Right Nothing  -> return (Left $ STail $ embedS e, stateMods)
    Left sysE       -> return (Left sysE, stateMods)
{-# INLINE effMaybeInWith #-}

effEitherSystemWith :: (e -> SystemError) -> Eff mods es (Either e a) -> Eff mods es a
effEitherSystemWith f eff = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right (Right a) -> return (Right a, stateMods)
    Right (Left e)  -> return (Left $ SHead $ f e, stateMods)
    Left sysE       -> return (Left sysE, stateMods)
{-# INLINE effEitherSystemWith #-}

effEitherSystemException :: (Exception e) => Eff mods es (Either e a) -> Eff mods es a
effEitherSystemException = effEitherSystemWith (SystemErrorException . toException)
{-# INLINE effEitherSystemException #-}

effEitherInWith :: (In e' es) => (e -> e') -> Eff mods es (Either e a) -> Eff mods es a
effEitherInWith f eff = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (unEff eff) rs ss
  case eS_E_Es of
    Right (Right a) -> return (Right a, stateMods)
    Right (Left e)  -> return (Left $ STail $ embedS $ f e, stateMods)
    Left sysE       -> return (Left sysE, stateMods)
{-# INLINE effEitherInWith #-}

effEitherIn :: (In e es) => Eff mods es (Either e a) -> Eff mods es a
effEitherIn = effEitherInWith id
{-# INLINE effEitherIn #-}
