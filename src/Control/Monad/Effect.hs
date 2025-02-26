{-# LANGUAGE DerivingVia, UndecidableInstances, LinearTypes #-}
module Control.Monad.Effect where

import Data.HList
import Data.Bifunctor
import Data.Kind
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.RST hiding (RSET(..), runRSET)
import Control.Exception

-- new design idea:
-- Remove current SystemError, creating a new type SystemError that is top level and is 
-- used to break the event loop. Other errors should not be present on top level.
--
-- so there should be an error list field in Eff, on system level they need to be empty
-- forcing the user to handle all errors inside module level, or throw them to the top level
--

-- | Effectful computation, using modules as units of effect
newtype Eff mods es a = Eff { runEff :: RSE (SystemRead mods) (SystemState mods) (SList (SystemError : es)) IO a }
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO
    , MonadReadable (SystemRead mods)
    , MonadStateful (SystemState mods)
    )

-- | embed smaller effect into larger effect
embedEff :: forall mods mods' es es' a. (EmbedSubList mods mods', SubList mods mods', SubList es (SystemError : es'))
  => Eff mods es a -> Eff mods' es' a
embedEff eff = Eff $ RSE $ \rs' ss' -> do
  let rs = getSubListF rs'
      ss = getSubListF @mods ss'
      modsEff = runRSE $ runEff eff
  (emods, ss1) <- modsEff rs ss
  return (first subListEmbed emods, subListUpdateF ss' ss1)
{-# INLINE embedEff #-}

-------------------------------------- instances --------------------------------------

class Module mod where
  data ModuleInitData mod :: Type
  data ModuleRead     mod :: Type
  data ModuleState    mod :: Type
  data ModuleEvent    mod :: Type

type SystemInitData mods = FList ModuleInitData mods
type SystemState    mods = FList ModuleState    mods
type SystemRead     mods = FList ModuleRead     mods
type SystemEvent    mods = UList ModuleEvent    mods
newtype SystemError = SystemError SomeException
type NoError = '[]

queryModule :: forall mod mods es. (In mod mods, Module mod) => Eff mods es (ModuleRead mod)
queryModule = queries @(SystemRead mods) (getF @mod)
{-# INLINE queryModule #-}

queriesModule :: forall mod mods es a. (In mod mods, Module mod) => (ModuleRead mod -> a) -> Eff mods es a
queriesModule f = f <$> queryModule @mod
{-# INLINE queriesModule #-}

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

------------------------------------------system : a list of modules------------------------------------------
-- | System is a list of modules loaded in sequence with dependency verification
class System mods where
  initAllModules :: SystemInitData mods -> IO (Either SystemError (SystemRead mods, SystemState mods))

  listenToEvents :: Eff mods '[] (STM (SystemEvent mods))

  handleEvents :: SystemEvent mods -> Eff mods '[] ()

  beforeSystem :: Eff mods '[] ()

  afterSystem  :: Eff mods '[] ()

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

-- | Inductive instance for system
instance (SubList mods (mod:mods), Module mod, System mods, Loadable mod mods) => System (mod ': mods) where
  initAllModules (x :** xs) = do
    initAllModules xs >>= \case
      Right (rs, ss) -> do
        (er, ss') <- runRSE (runEff @mods $ initModule @mod x) rs ss
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

effCatch :: Eff mods (e : es) a -> (e -> Eff mods es a) -> Eff mods es a
effCatch eff h = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (runEff eff) rs ss
  case eS_E_Es of
    Right a                 -> return (Right a, stateMods)
    Left (SHead sysE)       -> return (Left $ SHead sysE, stateMods)
    Left (STail (SHead e))  -> runRSE (runEff $ h e) rs ss
    Left (STail (STail es)) -> return (Left $ STail es, stateMods)
    Left _                  -> error "SEmpty should not be present in error"
  -- return (_ , stateMods)
{-# INLINE effCatch #-}

effCatchAll :: Eff mods es a -> (SList es -> Eff mods NoError a) -> Eff mods NoError a
effCatchAll eff h = Eff $ RSE $ \rs ss -> do
  (eS_E_Es, stateMods) <- runRSE (runEff eff) rs ss
  case eS_E_Es of
    Right a           -> return (Right a, stateMods)
    Left (STail es)   -> runRSE (runEff $ h es) rs ss
    Left (SHead sysE) -> return (Left $ SHead sysE, stateMods)
    Left _            -> error "SEmpty should not be present in error"
{-# INLINE effCatchAll #-}

effThrow :: In e (SystemError : es) => e -> Eff mods es a
effThrow e = Eff $ RSE $ \_ s -> pure (Left $ embedS e, s)
{-# INLINE effThrow #-}

effThrowSystem :: SystemError -> Eff mods '[] a
effThrowSystem e = Eff $ RSE $ \_ s -> pure (Left $ SHead e, s)
{-# INLINE effThrowSystem #-}

--------------------------------------------------------------------------------------------------
-- 
-- --------------------------------------------------------------------------------------------------
-- 
-- data Database = Database
-- instance Module Database where
--   data ModuleInitData Database = DatabaseInitData { dbPath :: Text, dbMigration :: Migration, dbPoolSize :: Int }
--   data ModuleRead     Database = DatabaseRead { dbPool :: Pool SqlBackend }
--   data ModuleState    Database = DatabaseState
--   data ModuleEvent    Database = DatabaseEvent
--   data ModuleError    Database = DatabaseError deriving Show
-- 
-- instance Logger `In` mods => Loadable Database mods where
--   initModule (DatabaseInitData path migration poolSize) = do
--     logger <- askLoggerIO
--     pool <- liftIO $ runLoggingT
--       ( do
--           pool <- createSqlitePool path poolSize
--           runMigration migration `runSqlPool` pool
--           return pool
--       ) logger
--     return $ Right (DatabaseRead pool, DatabaseState)
--   {-# INLINE initModule #-}
-- 
-- class RunDB m where
--   runDB :: ReaderT SqlBackend IO a -> m a
-- 
-- instance Database `In` mods => RunDB (Eff mods) where
--   runDB action = do
--     DatabaseRead pool <- asks (getF @Database)
--     liftIO $ runSqlPool action pool
--   {-# INLINE runDB #-}
