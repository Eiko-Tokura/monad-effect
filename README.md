# monad-effect - a lightweight, fast, algebraic effect system

This project is still in experimental beta and may evolve quickly. Feedback and contributions are very welcome.

`monad-effect` gives you:

- a single, optimisation-friendly monad transformer `EffT` that combines **Reader, State and algebraic errors**;
- **modules** as the unit of effect (e.g. reader, state, logging, database, HTTP, metrics);
- **explicit, composable error lists** instead of using `Text` / `SomeException` ; and
- performant effect stacks, without sacrificing purity.

Most users will work with the `Eff` / `EffT` type aliases and the built-in reader/state modules (`RModule`, `SModule`) and define their own modules around them.

---

- [Project Intuition](#project-intuition)
- [Key Features](#key-features)
  - [Algebraic exceptions](#algebraic-exceptions)
  - [Purity](#purity)
  - [Flexible and modular](#flexible-and-modular)
- [Core Types and Abstractions](#core-types-and-abstractions)
  - [EffT, Eff and EffT'](#efft-eff-and-efft)
  - [Result and EList - algebraic error lists](#result-and-elist---algebraic-error-lists)
  - [Named error types - ErrorText, ErrorValue, MonadExcept](#named-error-types---errortext-errorvalue-monadexcept)
  - [Modules and the system view](#modules-and-the-system-view)
  - [Built-in Reader and State modules - RModule and SModule](#built-in-reader-and-state-modules---rmodule-and-smodule)
  - [RS.Class - `MonadReadOnly`, `MonadReadable`, `MonadStateful`](#rsclass---monadreadonly-monadreadable-monadstateful)
- [Getting Started - Examples](#getting-started---examples)
  - [Quick start - algebraic state and errors](#quick-start---algebraic-state-and-errors)
  - [Embedding and reshaping effects](#embedding-and-reshaping-effects)
  - [Scoped module initialisation](#scoped-module-initialisation)
  - [Large application - a bot with many modules](#large-application---a-bot-with-many-modules)
  - [Example - database access](#example---database-access)
- [Selected API Reference](#selected-api-reference)
  - [Core monad and runners](#core-monad-and-runners)
  - [Error machinery](#error-machinery)

## Project Intuition

At a high level you can think of:

```haskell
newtype EffT mods es m a =
  EffT { unEffT :: SystemRead mods -> SystemState mods -> m (Result es a, SystemState mods) }
```

as:

- **one layer of `Reader`** over a *heterogeneous* environment `SystemRead mods`,
- **one layer of `State`** over a *heterogeneous* state `SystemState mods`, and
- a **typed, algebraic error channel** `Result es a`, where `es :: [Type]` is a *type-level list* of error types.

You explicitly say:

- which **modules** (`mods`) your effect depends on (configuration, mutable state, handles, etc.);
- which **errors** (`es`) it can throw (e.g. `IOException`, `ErrorText "http"`, `MyDomainError`); and
- you get back both a **result** and the **final module state**.

Typical use-cases:

- replace one or more layers of `ReaderT`, `StateT`, `ExceptT` or even more equivalent ones, by a single `EffT` with a small list of modules and error types;
- easily **add or remove error types** (`effCatch`, `effCatchIn`, `errorToEitherAll`, ... );
- run the same effect in **pure monads** (e.g. `Identity`) and in `IO`; and
- pass around **large module stacks** in real applications while keeping the types informative and composable.

---

## Key Features

### Algebraic exceptions

In classic Haskell (and in other languages like Rust), exceptions are often encoded algebraically as `Maybe` or `Either`:

- `Maybe a` is composable but not very informative - you lose any structured information about *why* something failed.
- `Either e a` carries an error payload, but composing multiple distinct `Either e_i a` values across a codebase tends to either:
  - collapse everything to a common super-type like `Text`/`SomeException` (and then you lose the ability to catch specific errors in a principled way, and loses the ability to declare that some of them won't happen); or
  - nest `Either e0 (Either e1 (Either e2 a))`, which is unergonomic.
- `ExceptT e m a` has the same compositional issues, *and* the transformer order matters:
  - `StateT s (ExceptT e m) a ~ s -> m (Either e (a, s))` - once an exception is thrown, both `a` and the intermediate state are lost / rolled-back.
  - `ExceptT e (StateT s m) a ~ s -> m (Either e a, s)` - the state up to the exception point is preserved, which is often what you actually want.

`monad-effect` addresses these issues by:

- using a **type-level list of error types** `es :: [Type]` and a non-empty sum `EList es` to track exactly which error types can occur; and
- using `Result es a` as the algebraic error carrier (see the formal definition below), which behaves like `Either (EList es) a` and collapses to `a` when `es ~ '[]`.

The underlying representation

```haskell
SystemRead mods
-> SystemState mods
-> m (Result es a, SystemState mods)
```

means that module state is preserved when an algebraic exception is thrown (like `ExceptT e (StateT s m) a`), rather than discarded.

You can throw algebraic errors into the list (`effThrowIn` / `effThrow`), catch all errors (`effCatchAll`), or catch and *remove* a specific error type (`effCatchIn`) so that the remaining computation provably no longer produces that error.

### Purity

Instead of reaching for `IORef` / `TVar` for every bit of mutable state, you can also choose to keep states *pure* and model them as part of your self-defined modules. It's a design choice you can make : some effect systems force you into `IO`. While for concurrency programs you need `TVar`s, but we should have the ability to choose pure state where appropriate because we love purity.

In particular the library provides two built-in modules:

- `SModule s` - a module holding a pure state of type `s`;
- `RModule r` - a module holding a read-only value of type `r`.

These integrate with the `MonadStateful` / `MonadReadable` classes and provide the familiar `getS` / `putS` / `modifyS` / `askR` / `localR` APIs, while still participating in the larger module stack.

Using modules, you can:

- keep configuration, pure in-memory state, handles, and effect interpreters in a single typed module stack; and
- run the same code in `Identity` for pure tests, or in `IO` for production, by choosing appropriate runners.
- Use it to run stateful tight computations without `IO` overhead (which GHC can optimize very well).

Template Haskell helpers in `Module.RS.QQ` (`makeRModule`, `makeRSModule`) make it easy to generate simple reader/state modules with minimal boilerplate. However, right now you are expected to write a lot of modules by hand as they are much more flexible.

Besides tight calculation that benefits from pure states, here is another example function that benefits from pure state: the function can be ran as a pure function with pure logging effect (writer / no-logging), or in IO whose logging prints to console/file.

```haskell
eventHandler
  :: (Monad pureMonad)
  => InputEvent
  -> EffT
      '[ Logging pureMonad LogData -- ^ We will use logging to generate diagnostics
      , EventState                -- ^ We need to read and update the state
      ]
      '[ ErrorText "not-allowed"
      ]
      pureMonad
      [OutputCommands]      -- ^ Output commands from the event module
```

### Flexible and modular

Because both modules and errors are tracked at the type level:

- `EffT '[] '[] m a` is isomorphic to `m a`;
- `EffT '[] '[e] m a` is isomorphic to `m (Either e a)`;
- `EffT '[] es  m a` is isomorphic to `m (Result es a)`.

You can:

- **eliminate modules** once you have their inputs, using e.g.:

  ```haskell
  runEffTOuter
    :: (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
    => ModuleRead mod
    -> ModuleState mod
    -> EffT' c (mod : mods) es m a
    -> EffT' c mods es m (a, ModuleState mod)

  runEffTOuter_
    :: ...
    => ModuleRead mod -> ModuleState mod
    -> EffT' c (mod : mods) es m a
    -> EffT' c mods es m a
  ```

  and similarly `runEffTIn`/`runEffTIn_` to drop an inner module.

- **embed smaller effects inside larger ones**, changing only modules (`embedMods`), only errors (`embedError`), or both (`embedEffT`).
- move fluidly between `EffT` and more conventional forms via runners and converters like `runEffT00`, `runEffT01`, `errorToEither`, `errorToEitherAll`, `errorToMaybe`, and the `effEither*` / `effMaybe*` family of helpers.

The result is a small set of primitives that scale well to large applications with many modules such as database access, HTTP clients, metrics, logging, and domain-specific state.

---

## Core Types and Abstractions

This section spells out the main types and how they fit together. All snippets in this section are taken directly from the library, except where explicitly marked as simplified.

### EffT, Eff and EffT'

The real core transformer is `EffT'`; `Eff` and `EffT` are its specialised aliases using the optimised `FData` container.

```haskell
-- | EffTectful computation, using modules as units of effect.
-- The tick indicates the polymorphic type 'c', the data structure
-- used to store the modules (usually 'FData' or 'FList').
newtype EffT' (c    :: (Type -> Type) -> [Type] -> Type)
              (mods :: [Type])
              (es   :: [Type])
              (m    :: Type -> Type) a
  = EffT'
      { unEffT' :: SystemRead  c mods
                -> SystemState c mods
                -> m (Result es a, SystemState c mods)
      }

-- Recommended, specialised aliases (use these in normal code):
type Eff   mods es  = EffT' FData mods es IO
type EffT  mods es  = EffT' FData mods es
type Pure  mods es  = EffT' FData mods es Identity
type In    mods es  = In'   FData mods es

-- Error-enhanced IO and ExceptT-like transformer
type ResultT  es m  = EffT' FData '[] es m
type IO'      es    = EffT' FData '[] es IO
```

Intuitively:

- `mods :: [Type]` - type-level list of **modules** (effects);
- `es   :: [Type]` - type-level list of **error types** that this computation may throw;
- `m` - the **base monad**; and
- `c` - the container type (`FData` by default) used to hold the module environments and states.

The library provides runners such as:

```haskell
runEffT
  :: Monad m
  => SystemRead  c mods
  -> SystemState c mods
  -> EffT' c mods es m a
  -> m (Result es a, SystemState c mods)

-- No modules
runEffT0
  :: (Monad m, ConsFNil c)
  => EffT' c '[] es m a
  -> m (Result es a)

-- No modules, no errors
type NoError = '[]

runEffT00
  :: (Monad m, ConsFNil c)
  => EffT' c '[] NoError m a
  -> m a

-- No modules, single error, exposed as Either
runEffT01
  :: (Monad m, ConsFNil c)
  => EffT' c '[] '[e] m a
  -> m (Either e a)
```

There are much more runners and combinators, see actual haddock documentation.

`EffT` and `Eff` can therefore be specialised to behave like a more flexible `ExceptT`:

```haskell
-- A flexible replacement for 'ExceptT es m'
type ResultT es m = EffT '[] es m
```

### Result and EList - algebraic error lists

Errors are represented by the `Result` and `EList` types:

```haskell
-- | Sum of types, non-empty by construction.
data EList (ts :: [Type]) where
  EHead :: !t          -> EList (t : ts)
  ETail :: !(EList ts) -> EList (t : ts)

-- | Error-aware result.
data Result (es :: [Type]) (a :: Type) where
  RSuccess :: a         -> Result es a
  RFailure :: !(EList es) -> Result es a

resultNoError :: Result '[] a -> a
resultNoError (RSuccess a) = a
```

Important facts:

- `Result es a` behaves like `Either (EList es) a`.  
- When `es ~ '[]`, `Result '[] a` is *effectively just `a`* (`resultNoError` witnesses this).
- `EList es` is a **non-empty** sum type: you cannot construct an `EList '[]`, so a `RFailure` always carries *one of the listed error types*.

### Named error types - ErrorText, ErrorValue, MonadExcept

To avoid defining a new ADT for every small error case, the library provides ad-hoc, named error wrappers:

```haskell
-- | A named textual error.
newtype ErrorText (s :: k) = ErrorText Text
  deriving newtype (IsString)

-- | Use type application 'errorText @"http" "text"'
errorText :: forall s. Text -> ErrorText s
errorText = ErrorText

-- | A named error that wraps an arbitrary value.
newtype ErrorValue (a :: k) (v :: Type) = ErrorValue v

-- | Type application helper
errorValue :: forall s v. v -> ErrorValue s v
errorValue = ErrorValue

-- | MonadExcept without a functional dependency,
-- so a monad can throw multiple error types.
class Monad m => MonadExcept e m where
  throwExcept :: e -> m a
```

Some useful instances:

- `instance Exception e => MonadExcept e IO`
- `instance MonadExcept e (Either e)`
- `instance Monad m => MonadExcept e (ExceptT e m)`
- `instance KnownSymbol s => MonadExcept (ErrorText s) (Either (Text, Text))`

This makes it very convenient to use `ErrorText "http"`, `ErrorText "decode"` etc. in larger codebases.

### Modules and the system view

Modules are the **unit of effect**. A module describes:

- what read-only data it exposes (`ModuleRead`); and
- what mutable state it keeps (`ModuleState`).

```haskell
class Module mod where
  data ModuleRead  mod :: Type
  data ModuleState mod :: Type

-- Modules that may be part of a "system"
class Module mod => SystemModule mod where
  data ModuleEvent    mod :: Type
  data ModuleInitData mod :: Type

-- System-wide containers (usually backed by 'FData')
type SystemRead     c mods = c ModuleRead  mods
type SystemState    c mods = c ModuleState mods
type SystemEvent      mods = UList ModuleEvent    mods
type SystemInitData c mods = c ModuleInitData     mods
```

Within `EffT'` you can access the data families, module reads and states using the provided helpers:

```haskell
-- | Synonyms
queryModule  :: (Monad m, In' c mod mods, Module mod)
             => EffT' c mods es m (ModuleRead  mod)
askModule    :: (Monad m, In' c mod mods, Module mod)
             => EffT' c mods es m (ModuleRead  mod)

queriesModule, asksModule
  :: (Monad m, In' c mod mods, Module mod)
  => (ModuleRead mod -> a)
  -> EffT' c mods es m a

getModule    :: (Monad m, In' c mod mods, Module mod)
             => EffT' c mods es m (ModuleState mod)
getsModule   :: (Monad m, In' c mod mods, Module mod)
             => (ModuleState mod -> a) -> EffT' c mods es m a

putModule    :: (Monad m, In' c mod mods, Module mod)
             => ModuleState mod -> EffT' c mods es m ()
modifyModule :: (Monad m, In' c mod mods, Module mod)
             => (ModuleState mod -> ModuleState mod)
             -> EffT' c mods es m ()
```

The `SystemModule`/`ModuleEvent`/`ModuleInitData` pieces are primarily used by higher-level orchestration helpers (e.g. scoped initialisation via `withModule`).

### Built-in Reader and State modules - RModule and SModule

The `Module.RS` module gives you ready-made reader/state modules and helpers to integrate existing `ReaderT`/`StateT` code.

```haskell
-- Reader module
data RModule (r :: Type)

instance Module (RModule r) where
  newtype ModuleRead  (RModule r) = RRead  { rRead  :: r }
  data    ModuleState (RModule r) = RState deriving (Generic, NFData)

-- State module
data SModule (s :: Type)

instance Module (SModule s) where
  data    ModuleRead  (SModule s) = SRead
  newtype ModuleState (SModule s) = SState { sState :: s }
    deriving newtype (Generic, NFData)
```

Convenience helpers:

```haskell
-- Reader-like interface
askR   :: (Monad m, In' c (RModule r) mods) => EffT' c mods errs m r
asksR  :: (Monad m, In' c (RModule r) mods) => (r -> a) -> EffT' c mods errs m a
localR :: (Monad m, In' c (RModule r) mods)
       => (r -> r) -> EffT' c mods errs m a -> EffT' c mods errs m a

-- State-like interface
getS   :: (Monad m, In' c (SModule s) mods) => EffT' c mods errs m s
getsS  :: (Monad m, In' c (SModule s) mods) => (s -> a) -> EffT' c mods errs m a
putS   :: (Monad m, In' c (SModule s) mods) => s -> EffT' c mods errs m ()
modifyS:: (Monad m, In' c (SModule s) mods) => (s -> s) -> EffT' c mods errs m ()
```

You also get helpers to run and embed modules:

```haskell
runRModule   :: (ConsFDataList c (RModule r : mods), Monad m)
             => r -> EffT' c (RModule r : mods) errs m a
             -> EffT' c mods errs m a

runSModule   :: (ConsFDataList c (SModule s : mods), Monad m)
             => s -> EffT' c (SModule s : mods) errs m a
             -> EffT' c mods errs m (a, s)

runSModule_  :: (ConsFDataList c (SModule s : mods), Monad m)
             => s -> EffT' c (SModule s : mods) errs m a
             -> EffT' c mods errs m a
```

### RS.Class - `MonadReadOnly`, `MonadReadable`, `MonadStateful`

`Control.Monad.RS.Class` defines type-class interfaces similar to `MonadReader`/`MonadState`, but *without* functional dependencies, so a single monad can have many readable and stateful values:

```haskell
class Monad m => MonadReadOnly r m where
  query   :: m r
  queries :: (r -> r') -> m r'

class MonadReadOnly r m => MonadReadable r m where
  local :: (r -> r) -> m a -> m a

class Monad m => MonadStateful s m where
  get    :: m s
  put    :: s -> m ()
  gets   :: (s -> a) -> m a
  modify :: (s -> s) -> m ()
```

Instances are provided for `EffT'` and for monad transformers.

---

## Getting Started - Examples

This section focuses on how to *use* the core abstractions.

### Quick start - algebraic state and errors

A small (made-up) example that:

- stores a `Map` as a state module,
- throws a named `ErrorText "Map.keyNotFound"` when a key is missing,
- and computes an average from a file.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Effect        -- EffT types and combinators
import Module.RS                   -- RModule / SModule helpers
import System.IO
import qualified Data.Map  as M
import qualified Data.Text as T

-- | Wrap a lookup into an algebraic error instead of 'Maybe'.
myLookup
  :: (Show k, Ord k, Monad m)
  => k
  -> EffT '[SModule (M.Map k v)] '[ErrorText "Map.keyNotFound"] m v
myLookup k =
  effMaybeInWith -- this converts a (Maybe v) return value into an algebraic error
    (errorText @"Map.keyNotFound" $
       " where key = " <> T.pack (show k)) -- using this converter function
    (getsS (M.lookup k))   -- :: EffT '[SModule (M.Map k v)] es m (Maybe v)

-- | This effect can run in pure monads like 'Identity' as well as 'IO'.
lookups
  :: forall v m. Monad m
  => EffT '[SModule (M.Map T.Text v)] '[ErrorText "Map.keyNotFound"] m (v, v, v)
lookups = do
  foo <- myLookup "foo"
  bar <- myLookup "bar"
  baz <- myLookup "baz"
  pure (foo, bar, baz)

parse :: String -> Maybe [Double]
parse = undefined  -- parsing logic

computeAverageFromFile
  :: FilePath
  -> Eff
       '[SModule (M.Map T.Text Int)]
       [ IOException
       , ErrorText "empty-file"
       , ErrorText "zero-numbers"
       , ErrorText "Map.keyNotFound"
       ]
       Double
computeAverageFromFile fp = do
  -- Capture 'IOException' from 'readFile' as an algebraic error.
  content <- embedError . liftIOException $ readFile fp

  when (null content) $
    effThrowIn (errorText @"empty-file" "file is empty")

  -- Turn a 'Maybe' into an ad-hoc algebraic error and immediately handle it.
  parsed <- pureMaybeInWith (errorText @"parse-error" "parse error") (parse content)
              `effCatch` \(_ :: ErrorText "parse-error") ->
                pure [0]

  -- Use another effect that requires the same module.
  _ <- embedEffT (lookups @Int)

  when (null parsed) $
    effThrowIn (errorText @"zero-numbers" "zero numbers")

  pure $ sum parsed / fromIntegral (length parsed)
```

You can run such effects in various ways. For example, using the state module runner from `Module.RS` and `runEffT01`:

```haskell
import Control.Monad.Effect
import Module.RS
import qualified Data.Map as M

runCounter
  :: Int
  -> Eff '[SModule Int] '[ErrorText "zero"] IO ()
  -> IO (Either (ErrorText "zero") ((), Int))
runCounter initial action =
  runEffT01 (runSModule initial action)
```

Here:

- `runSModule` eliminates the `SModule` from the module list while threading the state, and
- `runEffT01` eliminates the remaining error list as an `Either`.

### Embedding and reshaping effects

Sometimes you want to run a smaller effect inside a bigger one; or change modules while keeping the error list, or vice versa. The library provides:

```haskell
-- Embed a smaller effect into a larger one (modules and/or errors).
embedEffT
  :: (SubList c mods mods', SubListEmbed es es', Monad m)
  => EffT' c mods es  m a
  -> EffT' c mods' es' m a

-- Only change the module list.
embedMods
  :: (Monad m, ConsFDataList c mods', SubListEmbed es es, SubList c mods mods')
  => EffT' c mods es m a
  -> EffT' c mods' es m a

-- Only change the error list.
embedError
  :: (Monad m, SubList c mods mods, SubListEmbed es es')
  => EffT' c mods es m a
  -> EffT' c mods es' m a
```

This is very useful when:

- you have a reusable component that only needs a subset of modules; or
- you want to **add a new error type** around an existing effect and then project back.

### Scoped module initialisation

For scoped initialisation and teardown, many projects define higher-level wrappers around the `Loadable`/`withModule` pattern. A (simplified) example from a real project:

```haskell
runApp
  :: EffT
       '[ RModule ProxyState
        , RModule ProxySettings
        , PrometheusMan
        , LoggingModuleB
        ]
       NoError
       IO
       ()
  -> IO ()
runApp app = do
  -- Parse options, set up logging and metrics, etc...
  opts :: ProxyOptions Unwrapped <- unwrapRecord "Haskell Proxy Server"
  case optionsToSettings opts of
    Nothing       -> putStrLn "Invalid options provided."
    Just settings -> do
      -- Initialise loggers (implementation-specific).
      baseStdLogger  <- ...
      baseFileLogger <- ...
      let logger = baseStdLogger <> baseFileLogger

      -- Compose initialisation modules inside Eff, then eliminate back to IO.
      runEffT00 $
        withLoggerCleanup logger $
        (if settings.setPrometheus
           then withPrometheus ...
           else withNoPrometheusMan) $
        do
          state <- initializeState settings
          runRModule settings $
          runRModule state    $
          app
```

The exact set of modules (`PrometheusMan`, `LoggingModuleB`, ...) is either project-specific / reusable components you can build between projects, but the pattern is always:

> build an `EffT` stack of modules, run your application logic there, then eliminate modules and errors with the provided runners.

### Large application - a bot with many modules

In a larger system you might have many modules and a domain-specific error list. (Taken from a real-world bot application.)

```haskell
-- The modules loaded into the bot
type Mods =
  [ LogDatabase
  , AsyncModule
  , ProxyWS
  , CronTabTickModule
  , StatusMonitorModule
  , CommandModule
  , RecvSentCQ
  , MeowActionQueue
  , SModule BotConfig
  , SModule WholeChat
  , SModule OtherData
  , MeowConnection
  , BotGlobal
  , ConnectionManagerModule
  , MeowDataDb
  , MeowCoreDb
  , PrometheusMan
  , LoggingModule
  ]

-- Exceptions that require restarting the bot
type MeowErrs =
  '[ ErrorText "recv_connection"
   , ErrorText "send_connection"
   , ErrorText "meowCoreDb"
   ]

type MeowT mods m = EffT mods MeowErrs m
type Meow         = MeowT Mods IO
```

The bot initialisation then becomes a composition of `withX` helpers and `run*Module` runners:

```haskell
runBot
  :: BotInstance -- ^ Initial bot configuration
  -> Meow a -- ^ the bot loop function
  -> EffT
       '[ BotGlobal
        , ConnectionManagerModule
        , MeowDataDb
        , MeowCoreDb
        , PrometheusMan
        , LoggingModule
        ]
       '[ErrorText "meowCoreDb"]
       IO
       ()
runBot bot meow = do
  -- Convert a plain value into a module (implementation-specific).
  botModule <- embedEffT $ botInstanceToModule bot

  -- Build additional configuration/state modules...
  -- initialise counters, status, etc.
  -- not relevant for this example.

  embedNoError -- requires the scope inside to have 'NoError' and embeds it to a larger error context
    $ effAddLogCat' (LogCat botModule.botId) -- adds a logging category to logs within the scope
    $ ( case botRunFlag bot of
          RunClient addr port -> void . withClientConnection addr port
          RunServer addr port ->        withServerConnection addr port
      ) -- running connection
    $ (\app -> do
        AllData wc bc od <- embedEffT $ initAllData botconfig
        runSModule_ od $ runSModule_ wc $ runSModule_ bc $ app
      ) -- domain state
    $ withMeowActionQueue
    $ withRecvSentCQ
    $ withModule CommandModuleInitData
    $ withModule (StatusMonitorModuleInitData meowStat)
    $ maybe id (\init -> withWatchDog init . const) mWatchDogInit
    $ withCronTabTick
    $ withProxyWS (ProxyWSInitData [(add, ip) | ProxyFlag add ip <- bot.botProxyFlags])
    $ withAsyncModule
    $ withLogDatabase
    $ meow
```

The details of `withMeowActionQueue`, `withRecvSentCQ`, `withProxyWS`, etc. are application-specific, but the pattern is always:

- each `withX` introduces one or more modules into the `EffT` stack and arranges their initial `ModuleRead`/`ModuleState`; and
- the final `Meow` computation runs in a rich module environment, with its error list (`MeowErrs`) tracking only the domain errors that matter at that layer.

### Example - database access

You can also use the type system to enforce that certain low-level errors are handled at the call-site. A typical pattern is to wrap a database action so it:

- requires a specific module to be present; and
- requires a specific error to be in the error list:

```haskell
runMeowDataDB
  :: ( In'   c MeowDataDb mods
     , InList (ErrorText "meowDataDb") es
     )
  => ReaderT SqlBackend IO b
  -> EffT' c mods es IO b
```

Because `ErrorText "meowDataDb"` is in the error list, callers must either:

- keep that error in their own `es` (and propagate it upward), or
- explicitly catch and handle it. If they forget, GHC will report a type error.

For example:

```haskell
-- Forgetting to handle 'ErrorText "meowDataDb"' here leads to a type error.
fetchBlockMessages :: BotId -> ChatId -> ChatBlockSpan -> Meow [ChatMessage]
fetchBlockMessages bid cid span = do
  entities <-
    runMeowDataDB (selectList [...conditions based on 'bid', 'cid', 'span'...])
      `effCatch` \(_ :: ErrorText "meowDataDb") ->
        pure []
  pure (map entityVal entities)
```

Here `effCatch` both catches the database error and removes `ErrorText "meowDataDb"` from the error list, so the rest of `fetchBlockMessages` no longer has to account for it.

---

## Selected API Reference

This section is **not** exhaustive. It highlights key exports; for full details, please consult the Haddock documentation.

### Core monad and runners

- `Eff mods es a`, `EffT mods es m a`, `Pure mods es a`, `ResultT es m a`, `IO' es m a`
- `runEffT`, `runEffT_`, `runEffT0`, `runEffT00`, `runEffT01`, `runResultT`
- `runEffTOuter`, `runEffTOuter'`, `runEffTOuter_` - eliminate the *outermost* module while supplying its `ModuleRead`/`ModuleState`
- `runEffTIn`, `runEffTIn'`, `runEffTIn_` - eliminate an *inner* module identified by its type
- `replaceEffTIn` - replace a module with another, using custom conversion functions
- `NoError`, `checkNoError`, `declareNoError`, `embedNoError`
- `applyErrors`, `applyMods` - helpers that expose `es` / `mods` to type applications without changing the value

### Error machinery

- Types: `Result es a`, `EList es`, `SystemError`
- Named wrappers: `ErrorText s`, `ErrorValue s v`, `errorText`, `errorValue`
- Throwing:
  - `effThrowIn`, `effThrow` - throw an error that is already in the list
  - `effThrowEList`, `effThrowEListIn` - throw multiple errors via `EList`
  - `MonadExcept e m` integration (e.g. via `tryAndThrow`, `tryAndThrowText`)
- Catching:
  - `effCatch` - catch the *first* error in the list
  - `effCatchIn` - catch a *specific* error type and remove it from the list
  - `effCatchAll` - catch all algebraic errors as an `EList es`
  - `effCatchSystem` - catch `SystemError`
- Converting errors:
  - `errorToEither`, `errorToEitherAll`, `eitherAllToEffect`
  - `errorInToEither`, `errorToMaybe`, `errorInToMaybe`, `errorToResult`
  - `mapError` - map one error list into another
- Turning `Either` / `Maybe` into errors:
  - `effEitherWith`, `effEither`
  - `effEitherInWith`, `effEitherIn`, `effEitherSystemException`
  - `effMaybeWith`, `effMaybeInWith`
  - `pureMaybeInWith`, `pureEitherInWith`
  - `baseEitherIn`, `baseEitherInWith`, `baseMaybeInWith`

### IO lifting and exception bridging

These functions help you bridge `IO` exceptions into algebraic errors:

- `liftIOException :: MonadIO m => IO a -> EffT' c mods '[IOException] m a`
- `liftIOAt       :: (Exception e, MonadIO m) => IO a -> EffT' c mods '[e] m a`
- `liftIOSafeWith :: (Exception e', MonadIO m) => (e' -> e) -> IO a -> EffT' c mods '[e] m a`
- `liftIOText     :: MonadIO m => (Text -> Text) -> IO a -> EffT' c mods '[ErrorText s] m a`
- `liftIOPrepend  :: Text -> IO a -> EffT' c mods '[ErrorText s] IO a`

Try/catch style:

- `effTry`, `effTryWith` - catch exceptions thrown in the *base monad* and turn them into algebraic errors
- `effTryIO`, `effTryIOWith`, `effTryIOIn`, `effTryIOInWith`
- `effTryUncaught` - catch uncaught exceptions into error lists
- `tryAndThrow`, `tryAndThrowWith`, `tryAndThrowText` - lift `IO` and rethrow via `MonadExcept`

### Modules and module helpers

- Core type classes:
  - `Module` - defines `ModuleRead` and `ModuleState` associated data families
  - `SystemModule` - extends `Module` with `ModuleEvent` and `ModuleInitData`
  - `Loadable c mod mods es` - provides `withModule` for scoped module initialisation
- System-wide aliases:
  - `SystemRead c mods`, `SystemState c mods`
  - `SystemEvent mods`, `SystemInitData c mods`
  - `SystemError`
- Accessors from `Control.Monad.Effect`:
  - `queryModule`, `queriesModule`, `askModule`, `asksModule`
  - `getModule`, `getsModule`, `putModule`, `modifyModule`

`withModule` (from `Control.System`) is particularly useful for implementing custom `withX` helpers that allocate resources, push a module on the stack, run an `EffT` computation, and then clean up.

### Bracket patterns and concurrency

Resource-safe patterns:

- `maskEffT` - `mask` in the base monad while staying in `EffT'`
- `generalBracketEffT`, `generalBracketEffT'`
- `bracketEffT`, `bracketEffT'`
- `bracketOnErrorEffT`, `bracketOnErrorEffT'`

Concurrency:

- `forkEffT` - fork an `EffT` computation onto a new thread
- `forkEffTFinally` - variants with finalisers
- `asyncEffT`, `withAsyncEffT`, `withAsyncEffT'` - integrate `async` with `EffT`
- `restoreAsync`, `restoreAsync_` - restore an `EffT` computation from an `Async` result

### RS modules and interfaces

From `Module.RS`:

- Types:
  - `RModule r`, `RNamed name r`
  - `SModule s`, `SNamed name s`
- Running modules:
  - `runRModule`, `runRModuleIn`
  - `runSModule`, `runSModule_`, `runSModuleIn`
- Embedding existing `ReaderT`/`StateT`:
  - `liftReaderT`, `embedReaderT`, `addReaderT`, `asReaderT`
  - `liftStateT`, `embedStateT`, `addStateT`, `asStateT`
- Convenience:
  - `askR`, `asksR`, `localR`
  - `getS`, `getsS`, `putS`, `modifyS`
  - `readOnly` - treat a state module as a read-only module inside a scope

From `Control.Monad.RS.Class`:

- `MonadReadOnly r m`, `MonadReadable r m`, `MonadStateful s m` - reader/state-like APIs without functional dependencies; `EffT'` has instances for these.

From `Control.Monad.Class.Except`:

- `MonadExcept e m` - multiple error types per monad
- `ErrorText`, `ErrorValue`, `errorText`, `errorValue`

### Template Haskell utilities

From `Module.RS.QQ`:

- `makeRModule`, `makeRModule_`, `makeRModule__`
- `makeRSModule`, `makeRSModule_`

`makeRModule` example (simplified):

```haskell
[makeRModule|MyModule
  myRecord1 :: !MyType1
  myRecord2 ::  MyType2
|]
```

Generates (conceptually):

- `data MyModule`
- `instance Module MyModule` with:
  - `data ModuleRead  MyModule  = MyModuleRead { myRecord1 :: !MyType1, myRecord2 :: MyType2 }`
  - `data ModuleState MyModule  = MyModuleState` (plus derivations)
- `instance SystemModule MyModule` with:
  - `data ModuleEvent    MyModule`
  - `data ModuleInitData MyModule`
- runners `runMyModule`, `runMyModuleIn`, etc., and convenient type synonyms for `ModuleRead` / `ModuleState`.

`makeRSModule` similarly builds a combined reader/state module from a compact specification, including optional lens generation for fields tagged with `Lens`.

---

## Performance, style and benchmarks

The core `EffT` design is **very optimisation-friendly**:

- modules are stored in a specialised data family `FData`, not a linked list;
- `FData` is generated by Template Haskell up to a fixed length (e.g. `FData3`, `FData4`, ...) with strict fields; and
- GHC can often optimise `EffT`-based code down to tight loops, competitive with or better than hand-written `StateT`/`ExceptT` stacks.

The benchmarks in `benchmark/` compare:

- `EffT` with `FList` (heterogeneous list),
- `EffT` with `FData`, and
- `StateT` from `mtl`.

On typical countdown/state benchmarks:

- `EffT` + `FData` is around **25 times faster** than `StateT` without optimisation, and
- about as fast as a properly optimised `StateT` (`-O2 -flate-dmd-anal`).

See the SVG charts under `benchmark/bench-result-*` in the repository for details.

### Some Benchmarks

See the `benchmark` folder for more benchmarks. The benchmarks are copied from `heftia`, another effect system library, with some modifications.

#### Countdown `-O2`
![Countdown-O2](https://raw.githubusercontent.com/Eiko-Tokura/monad-effect/3aeceddb0c7e452b34032b404a5fdc068df322de/benchmark/bench-result-O2/countdown-deep.svg)

#### Countdown `-O0`
![Countdown-O0](https://raw.githubusercontent.com/Eiko-Tokura/monad-effect/3aeceddb0c7e452b34032b404a5fdc068df322de/benchmark/bench-result-O0/countdown-deep.svg)

#### Deep Catch `-O2`
![Catch-O2](https://raw.githubusercontent.com/Eiko-Tokura/monad-effect/3aeceddb0c7e452b34032b404a5fdc068df322de/benchmark/bench-result-O2/catch-deep.svg)

#### Deep Catch `-O0`
![Catch-O0](https://raw.githubusercontent.com/Eiko-Tokura/monad-effect/3aeceddb0c7e452b34032b404a5fdc068df322de/benchmark/bench-result-O0/catch-deep.svg)

#### Local State `-O2`
![Local-O2](https://raw.githubusercontent.com/Eiko-Tokura/monad-effect/3aeceddb0c7e452b34032b404a5fdc068df322de/benchmark/bench-result-O2/local-deep.svg)

#### Local State `-O0`
![Local-O0](https://raw.githubusercontent.com/Eiko-Tokura/monad-effect/3aeceddb0c7e452b34032b404a5fdc068df322de/benchmark/bench-result-O0/local-deep.svg)

### Flags

GHC's type-checker sometimes needs more fuel for large module/error lists. It is recommended to build with:

```bash
-fconstraint-solver-iterations=16
```

or slightly higher when using very deep stacks.

### Style and module design

`monad-effect` does not impose a particular way to structure your modules. You can:

- package a concrete implementation (e.g. Prometheus counter, HTTP manager, database connection pool) directly into a module; or
- use a more “algebraic effects” style, where modules carry *handlers* for an algebraic effect GADT.

An example of the latter is a Prometheus counter module that carries a handler as read-only state

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, RequiredTypeArguments #-}
module Module.Prometheus.Counter where

import Control.Monad.Effect
import System.Metrics.Prometheus.Metric.Counter as C

-- | A prometheus counter module that has a name
data PrometheusCounter (name :: k)

-- | Counter effects written in algebraic effect style
data PrometheusCounterEffect a where
  AddAndSampleCounter :: Int -> PrometheusCounterEffect CounterSample
  AddCounter          :: Int -> PrometheusCounterEffect ()
  IncCounter          ::        PrometheusCounterEffect ()
  SetCounter          :: Int -> PrometheusCounterEffect ()
  SampleCounter       ::        PrometheusCounterEffect CounterSample

-- | The effect handler type for a prometheus counter with given counter name
type PrometheusCounterHandler (name :: k) = forall c mods es m a. (In' c (PrometheusCounter name) mods, MonadIO m) => PrometheusCounterEffect a -> EffT' c mods es m a

-- | The module is declared as a reader module that carries a counter handler
instance Module (PrometheusCounter name) where
  newtype ModuleRead  (PrometheusCounter name) = PrometheusCounterRead { prometheusCounterHandler :: PrometheusCounterHandler name }
  data    ModuleState (PrometheusCounter name) = PrometheusCounterState

-- | Specify / interpret a counter effect with given counter name
runPrometheusCounter
  :: forall name
  -> ( ConsFDataList c (PrometheusCounter name : mods)
     , Monad m
     )
  => PrometheusCounterHandler name -> EffT' c (PrometheusCounter name ': mods) es m a -> EffT' c mods es m a
runPrometheusCounter name handler = runEffTOuter_ (PrometheusCounterRead @_ @name handler) PrometheusCounterState
{-# INLINE runPrometheusCounter #-}

-- | Carry out a counter effect with given counter name
prometheusCounterEffect :: forall name -> (In' c (PrometheusCounter name) mods, MonadIO m) => PrometheusCounterEffect a -> EffT' c mods es m a
prometheusCounterEffect name eff = do
  PrometheusCounterRead handler <- askModule @(PrometheusCounter name)
  handler eff
{-# INLINE prometheusCounterEffect #-}

-- | Use a specific counter to carry out a counter effect
useCounter :: Counter -> PrometheusCounterHandler name
useCounter counter IncCounter              = liftIO $ C.inc counter
useCounter counter (AddCounter n)          = liftIO $ C.add n counter
useCounter counter (SetCounter n)          = liftIO $ C.set n counter
useCounter counter (AddAndSampleCounter n) = liftIO $ C.addAndSample n counter
useCounter counter SampleCounter           = liftIO $ C.sample counter
{-# INLINE useCounter #-}

-- | A counter handler that does nothing
noCounter :: Monad m => PrometheusCounterEffect a -> EffT mods es m a
noCounter IncCounter              = pure ()
noCounter (AddCounter _)          = pure ()
noCounter (SetCounter _)          = pure ()
noCounter (AddAndSampleCounter _) = pure (CounterSample 0)
noCounter SampleCounter           = pure (CounterSample 0)
{-# INLINE noCounter #-}
```

---

## Documentation changes from previous versions

Compared to earlier versions of the README:

- The formal definition of `Result` has been updated to match the code in `Data.Result` (single `RSuccess` constructor, with `Result '[] a` behaving like `a`).

- All examples now use the `errorText` / `errorValue` smart constructors instead of directly constructing using constructors `ErrorText` / `ErrorValue`. This avoids the filling kind type parameter `ErrorText @_ @"..."` noise.

- The `myLookup` / `computeAverageFromFile` example has been synchronised with the code in `test/Examples.hs` and corrected to use `errorText @"Map.keyNotFound"` with a proper `Text` value.

- Experimental system orchestration types (`WithSystem`, `EventLoopSystem`, event loops) and the `Resource` module are intentionally **not** described in detail here, as they may change or be removed in future versions. The focus is on `EffT`, modules, algebraic errors, and the RS/Except helper classes.

- Minor wording updates were made throughout to reflect that `EffT`/`Eff` (with `FData`) are the recommended entry points for most users.

For full API details, please refer to the Haddock documentation generated from the source.
