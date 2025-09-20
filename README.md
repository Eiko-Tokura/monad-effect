# A lightweight, fast, and algebraic effect system that makes sense

This project is in experimental beta, it may change relatively quickly. I will definitely improve it when I use it more in other projects. Feedbacks and contributions are welcome!

## The `EffT` Monad Transformer

The core type of the library is the `EffT` monad transformer, which can be understood as follows:

```haskell
newtype EffT (mods :: [Type]) (es :: [Type]) (m :: Type -> Type) a
  = EffT { SystemRead mods -> SystemState mods -> m (Result es a, SystemState mods) }
```

(This is a simplification of the actual type, but basically the same idea, see the later sections for explanation.)

It is a single layer of reader and state monad together with composable error handling.

* The unit of effect is a `Module`, which just has some custom data families defining its Read and State types.

* `mods` is a list of modules that the effect uses.

* `es` is the list of errors that the effect can throw, which is explicit and algebraic.

* `SystemRead mods` is a data family that holds all the read-only data for the modules in `mods`.

* `SystemState mods` is a data family that holds all the pure-states for the modules in `mods`.

Algebraic exceptions are a key feature of this library, it is easy to throw ad-hoc exception types and the type system will make sure you deal with them or acknowledge their existence.

## Key Features

### **Algebraic Exceptions**

I'm a believer in algebraic data structures and I think exceptions should be made explicit and algebraic. In classic Haskell and other languages like Rust, exceptions are made algebraic using `Maybe` or `Either` types. Haskell provides monadic supports and a `ExceptT` monad transformer for these types, making them joyful to use, I surely love them!

But there are some problems with `Maybe` and `Either`:

* `Maybe` gives you no information about the error, it is composable but not informative. The same problem with `MaybeT`.

* `Either e` gives you information of type `e`, but if you have multiple different `Either e_i` types in your program, there is no obvious way to compose them except by using `Either Text`, `Either SomeException` or `Either (Either e1 (Either e2 e3))`. The former is tempting to use but it gives us no obvious way to catch specific errors (you don't want to parse the Text message to find out what went wrong), and the latter is not ergonomic at all.

* `ExceptT` has the same problem as `Either` and it also has a *small pitfall*, the order of composing monad transformers matters. Think about what `StateT s (ExceptT e m) a` and `ExceptT e (StateT s m) a` mean.

  - `ExceptT e m` is isomorphic to `m (Either e a)`, so `StateT s (m (Either e *)) a` 'desugars' to

    `s -> m (Either e (a, s))`. Depending on what you want the computation to be, this might not be what you want, because once you have an algebraic exception `e`, not only the result `a` is lost, the state during the computation until the exception step is also lost. You will need to start over with an initial state. Maybe this is the behavior you want to have, but it is not obvious what behavior you are using by looking at the type signature.

  - On the other hand, `ExceptT e (StateT s m) a` is isomorphic to `StateT s m (Either e a)`, which desugars to

    `s -> m (Either e a, s)`. This is the more 'correct' behavior, during the computation once you have an exception, the state until the exception steop is preserved.

To solve all these problems, we made the following designs:

* A `Result es a` type that is a sum type of all the exception types in the type level list `es` and return type `a`. This is achieved not by using `Either` but a custom GADT:

  ```haskell
  data Result (es :: [Type]) a where
    RSuccess :: a -> Result '[] a
    RFailure :: !(EList es) -> Result es a

  data EList (es :: [Type]) where
    EHead :: !e -> EList (e ': es)
    ETail :: !(EList es) -> EList (e ': es)
  ```

  Here `EList es` is a sum type that has value in exactly one of the types in `es` and is by construction must be non-empty.

  `Result es a` behaves like `Either (EList es) a`, but better: if `es = '[]`, then `Result '[] a` is just isomorphic to `a`, there is no `RFailure` case!
  
* The type inside `EffT` is `SystemRead mods -> SystemState mods -> m (Result es a, SystemState mods)`, which means that the state is preserved when an algebraic exception is thrown. This is the same as `StateT s m (Either e a)`.

  Note if you have a blowup in the base monad `m`, then you will still lose everything in `(Result es a, SystemState mods)` since blowing up `m` can be thought as branching a `Left` case in `m`. The idea is that you should wrap your low-level routine in algebraic exceptions so that everything goes explicit and algebraic.

### **Purity**

Instead of giving up purity and using `IORef` or `TVar` for every state, we allow the possibility of having pure states in the effect modules. We also provide two built-in modules: `SModule s` is a module that holds a pure state of type `s`, and `RModule r` is a module that holds a read-only value of type `r`. You can use these modules to store pure states and read-only values in the effect system.

Let's see a simple example that combines the use of `SModule` and algebraic exceptions:

```haskell
import Control.Monad.Effect -- the EffT types and useful combinators
import qualified Data.Map  as M
import qualified Data.Text as T

-- | Wraps your effectul routine into EffT monad transformer
myLookup :: (Show k, Ord k, Monad m) => k -> EffT '[SModule (M.Map k v)] '[ErrorText "Map.keyNotFound"] m v
myLookup k
  = effMaybeInWith (ErrorText @"Map.keyNotFound" $ " where key = " <> T.pack (show k)) -- wraps Maybe into an exception
  $ getsS (M.lookup k) -- this just returns a monadic value of type `Maybe v`

-- | This effect can run as a pure function! Put m = Identity for example.
lookups :: forall v m. (Monad m) => EffT '[SModule (M.Map T.Text v)] '[ErrorText "Map.keyNotFound"] m (v, v, v)
lookups = do
  foo <- myLookup "foo"  -- this will throw an exception if "foo" is not found
  bar <- myLookup "bar"  -- instead of Nothing, you get an algebraic exception `ErrorText "Map.keyNotFound"` explaining what went wrong
  baz <- myLookup "baz"  -- just like Maybe and Either, when an exception is thrown, the computation stops and immediately returns
  return (foo, bar, baz)
```

Here `ErrorText (s :: Symbol)` is a newtype wrapper for `Text` is for you to create ad-hoc exception types very easily. We also provided `ErrorValue (s :: Symbol) (v :: Type)` that is a newtype wrapping `v` if you want a more concrete type.

### **Performant**

In fact the library defines a more general `EffT'` type that is also polymorphic in the container that holds the list of types

```haskell
newtype EffT' (c :: (Type -> Type) -> [Type] -> Type) (mods :: [Type]) (es :: [Type]) (m :: Type -> Type) a
  = EffT' { SystemRead c mods -> SystemState c mods -> m (Result es a, SystemState c mods) }

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
```

And we have two containers implemented, a standard heterogeneous list `c = FList`

```haskell
data FList (f :: Type -> Type) (ts :: [Type]) where
  FNil  :: FList f '[]
  FCons :: !(f t) -> !(FList f ts) -> FList f (t : ts)
infixr 5 `FCons`
```

And a more performant data family `c = FData`. The `FData` container is used by default, instead of storing a list as your data structure, it creates a data container that is indexed by the list

```haskell
data family FData (f :: Type -> Type) (ts :: [Type]) :: Type

data instance FData f '[] = FData0
data instance FData f '[t] = FData1
  { fdata1_0 :: !(f t)
  }
data instance FData f '[t1, t2] = FData2
  { fdata2_0 :: !(f t1)
  , fdata2_1 :: !(f t2)
  }
data instance FData f '[t1, t2, t3] = FData3
  { fdata3_0 :: !(f t1)
  , fdata3_1 :: !(f t2)
  , fdata3_2 :: !(f t3)
  }
data instance FData f '[t1, t2, t3, t4] = FData4
  { fdata4_0 :: !(f t1)
  , fdata4_1 :: !(f t2)
  , fdata4_2 :: !(f t3)
  , fdata4_3 :: !(f t4)
  }
data instance FData f '[t1, t2, t3, t4, t5] = FData5
  { fdata5_0 :: !(f t1)
  , fdata5_1 :: !(f t2)
  , fdata5_2 :: !(f t3)
  , fdata5_3 :: !(f t4)
  , fdata5_4 :: !(f t5)
  }
```

This is much more performant than a list (which GHC cannot inline recursive functions operating on it), and GHC optimizes it very well. The performance of `FData` over `FList` is about `5~100` times faster!

Of course we did not write the instances by hand, rather we used Template Haskell to generate all the instances including the methods to extract values from the data structure and to update/compose them. Currently we generated instances up to 15 types in the list, which should be more than enough. (Remark: the error type `es` does not live in `FData` and have no limit).

A count-down benchmark shows that `EffT` is 25 times faster than `StateT` without optimization, and as fast as a `StateT` with correct optimization (`-O2 -flate-dmd-anal`, for which both optimizes to a really fast simple loop!)

```haskell
{-# LANGUAGE DataKinds, PartialTypeSignatures #-}
module Main (main) where

import Control.Monad.Effect
import Criterion.Main
import Data.TypeList
import Data.TypeList.FData
import Module.RS
import qualified Control.Monad.State as S

testEffStateFPoly :: _ => EffT' flist '[RModule (), SModule Int, SModule Bool] NoError IO ()
testEffStateFPoly = do
  x <- getS @Int
  modifyS not
  if x < 1_000_000
    then putS (x + 1) >> testEffStateFPoly
    else return ()

testMtlState :: S.StateT ((), Int, Bool) IO ()
testMtlState = do
  x <- S.gets (\(_, x, _) -> x)
  S.modify (\(_, x', b) -> ((), x', not b))
  if x < 1_000_000
    then do
      S.modify (\(_, _, b) -> ((), x + 1, b))
      testMtlState
    else return ()

main = defaultMain
    [ bgroup "State Effect Eff"
      [ bench "FList" $ whnfIO $ runEffTNoError
          (RRead () `FCons` SRead `FCons` SRead `FCons` FNil)
          (RState `FCons` SState 0 `FCons` SState False `FCons` FNil)
          testEffStateFPoly
      , bench "FData" $ whnfIO $ runEffTNoError
          (FData3 (RRead ()) SRead SRead)
          (FData3 (RState) (SState 0) (SState False))
          testEffStateFPoly
      ]
    , bgroup "Mtl State"
      [ bench "StateT" $ whnfIO $ S.runStateT testMtlState ((), 0, False)
      ]
    ]
```

Tested on my laptop with GHC 9.12.2:

```plain
-------- With -O2 -flate-dmd-anal

benchmarking State Effect Eff/FList
time                 4.971 ms   (4.031 ms .. 5.956 ms)
                     0.887 R²   (0.843 R² .. 0.985 R²)
mean                 5.264 ms   (4.919 ms .. 5.648 ms)
std dev              1.239 ms   (975.3 μs .. 1.412 ms)
variance introduced by outliers: 90% (severely inflated)

benchmarking State Effect Eff/FData
time                 117.6 μs   (117.5 μs .. 117.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 117.5 μs   (117.2 μs .. 117.7 μs)
std dev              865.3 ns   (639.9 ns .. 1.398 μs)

benchmarking Mtl State/StateT
time                 117.1 μs   (116.8 μs .. 117.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 117.3 μs   (117.2 μs .. 117.5 μs)
std dev              463.5 ns   (345.5 ns .. 691.4 ns)
```

The optimization friendly design of the library allows you to use it in performance critical code without sacrificing purity and composability, it can be used as a drop-in replacement (upgrade!) for `StateT`, `ExceptT`, `ReaderT`, or even `IO` monad, which is more performant and composable!

### Flexible

#### Represents Common Monads

The `EffT` monad can be easily transformed into other monads, making it really a more flexible and composable replacement for `StateT`, `ExceptT`, `ReaderT`, or even `IO` monad.

For example,

* the type `EffT '[] '[] m a` is just isomorphic to `m a`

* the type `EffT '[] '[e] m a` is isomorphic to `m (Either e a)`

* the type `EffT '[] es m a` is isomorphic to `m (Result es a)`

```haskell
type NoError = '[] -- just a synonym

-- | runs the EffT' with no modules and no error
runEffT00 :: (Monad m, ConsFNil c) => EffT' c '[] NoError m a -> m a
runEffT00 = fmap resultNoError . runEffT0

-- | runs the EffT' with no modules and a single possible error type, return as classic Either type
runEffT01 :: (Monad m, ConsFNil c) => EffT' c '[] '[e] m a -> m (Either e a)
runEffT01 = fmap (first fromElistSingleton . resultToEither) . runEffT0

-- | runs the EffT' with no modules
runEffT0 :: (Monad m, ConsFNil c) => EffT' c '[] es m a -> m (Result es a)
runEffT0 = fmap fst . runEffT fNil fNil

-- | Convert the first error in the effect to Either
errorToEither :: Monad m => EffT' c mods (e : es) m a -> EffT' c mods es m (Either e a)

-- | Convert all errors to Either
errorToEitherAll :: Monad m => EffT' c mods es m a -> EffT' c mods NoError m (Either (EList es) a)
-- (... more functions to convert EffT between common types ...)
```

#### Eliminate Effects

Effects can be eliminated! Imagine if you have 5 reader modules, you should be able to give a reader value and eliminate it from the effect type. This is achieved by the following functions:

```haskell
-- | Runs a EffT' computation and eliminate the most outer effect with its input given
--
-- Warning: `ModuleState mod` will be lost when the outer EffT' returns an exception
runEffTOuter :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods es m (a, ModuleState mod)

-- | the same as runEffTOuter, but discards the state
runEffTOuter_ :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods es m a

-- | Running an inner module of EffT, eliminates it
runEffTIn :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) es m (a, ModuleState mod)

-- | The same as runEffTIn, but discards the state
runEffTIn_ :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) es m a
```

#### Throw Algebraic and Catch Algebraic Exceptions

You can throw algebraic exceptions in the effect system using `effThrowIn` and catch them using `effCatch`. After they are caught, the error type is removed from the error list.

```haskell
```haskell
-- | Throw into the error list
effThrowIn :: (Monad m, InList e es) => e -> EffT' c mods es m a

-- | Throw into the error list
effThrow :: (Monad m, InList e es) => e -> EffT' c mods es m a
effThrow = effThrowIn

-- | Catch the first error in the error list, and handle it with a handler function
effCatch :: Monad m => EffT' c mods (e : es) m a -> (e -> EffT' c mods es m a) -> EffT' c mods es m a

-- | Catch a specific error type in the error list, and handle it with a handler function.
-- This will remove the error type from the error list.
effCatchIn:: forall e es mods m c a es'. (Monad m, InList e es, es' ~ Remove (FirstIndex e es) es)
  => EffT' c mods es m a -> (e -> EffT' c mods es' m a) -> EffT' c mods es' m a
```

## An Example

```haskell
module Examples where

import Control.Exception
import Control.Monad
import Control.Monad.Effect -- the EffT types and useful combinators
import Module.RS -- built in modules, a reader module and a state module
import System.IO
import qualified Data.Map  as M
import qualified Data.Text as T

-- $ our monad-effect provides **module management** and **composable exceptions**
-- it's algebraic, performant, make sense, without sacrificing purity

-- | Wraps your effectul routine into EffT monad transformer
myLookup :: (Show k, Ord k, Monad m) => k -> EffT '[SModule (M.Map k v)] '[ErrorText "Map.keyNotFound"] m v
myLookup k
  = effMaybeInWith (ErrorText @"Map.keyNotFound" $ " where key = " <> T.pack (show k)) -- wraps Maybe into an exception
  $ getsS (M.lookup k) -- this just returns a monadic value of type `Maybe v`

-- | This effect can run in pure monads! like Identity
lookups :: forall v m. (Monad m) => EffT '[SModule (M.Map T.Text v)] '[ErrorText "Map.keyNotFound"] m (v, v, v)
lookups = do
  foo <- myLookup "foo"  -- this will throw an exception if "foo" is not found
  bar <- myLookup "bar"  -- instead of Nothing, you get an algebraic exception `ErrorText "Map.keyNotFound"` explaining what went wrong
  baz <- myLookup "baz"  -- just like Maybe and Either, when an exception is thrown, the computation stops and immediately returns
  return (foo, bar, baz)

parse :: String -> Maybe [Double]
parse = undefined -- some parsing logic that returns `Nothing` on failure

computeAverageFromFile
  :: FilePath
  -> Eff                            -- a synonym, Eff mods es a = EffT mods es IO a
      '[SModule (M.Map T.Text Int)] -- this effect can read and modify a value of type (Map Text Int)
      [ IOException                 -- composable and explicit exceptions
      , ErrorText "empty-file"      -- you know what types of error this effect can produce
      , ErrorText "zero-numbers"    -- just by observing its type signature
      , ErrorText "Map.keyNotFound"
      ]
      Double                        -- return type
computeAverageFromFile fp = do
  -- | the `liftIOException :: IO a -> Eff '[] '[IOException] a` captures `IOException`
  content <- embedError . liftIOException $ readFile' fp

  -- | throw an Algebraic error instead of an exception that you have no idea
  when (null content) $ do
    effThrowIn ("file is empty" :: ErrorText "empty-file")

  -- | this `pureMaybeInWith :: In e es => e -> Maybe a -> Eff mods es a` turns a Maybe value into an ad-hoc exception type!
  parsed <- pureMaybeInWith ("parse error" :: ErrorText "parse-error") (parse content) 
    `effCatch` (\(_ :: ErrorText "parse-error") -> return [0])
  -- ^ you can catch exception and deal with it, so the error is eliminated from the list

  -- | The type system will check whether you have the module needed to perform this action
  _ <- embedEffT $ lookups @Int

  -- | The type system will force you remember that we can return an exception with an custom type `ErrorText "zero-numbers"`
  when (null parsed) $ do
    effThrowIn ("zero numbers" :: ErrorText "zero-numbers")

  return $ sum parsed / fromIntegral (length parsed)
```

## Some Benchmarks

See the `benchmark` folder for more benchmarks. The benchmarks are copied from `heftia`, another effect system library and I added some modified versions.

#### Countdown `-O2`
![Countdown-O2](./benchmark/bench-result-O2/countdown-deep.svg)

#### Countdown `-O0`
![Countdown-O0](./benchmark/bench-result-O0/countdown-deep.svg)

#### Deep Catch `-O2`
![Catch-O2](./benchmark/bench-result-O2/catch-deep.svg)

#### Deep Catch `-O0`
![Catch-O0](./benchmark/bench-result-O0/catch-deep.svg)

#### Local State `-O2`
![Local-O2](./benchmark/bench-result-O2/local-deep.svg)

#### Local State `-O0`
![Local-O0](./benchmark/bench-result-O0/local-deep.svg)
