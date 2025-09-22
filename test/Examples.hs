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
  = effMaybeInWith (errorText @"Map.keyNotFound" $ " where key = " <> T.show k) -- wraps Maybe into an exception
  $ getsS (M.lookup k) -- this just returns a monadic value of type `Maybe v`

-- | This effect can run in pure monads! like Identity
lookups :: forall v m. (Monad m) => EffT '[SModule (M.Map T.Text v)] '[ErrorText "Map.keyNotFound"] m (v, v, v)
lookups = do
  foo <- myLookup "foo"  -- this will throw an exception if "foo" is not found
  bar <- myLookup "bar"  -- instead of Nothing, you get an algebraic exception `ErrorText "Map.keyNotFound"` explaining what went wrong
  baz <- myLookup "baz"  -- just like Maybe and Either, when an exception is thrown, the computation stops and immediately returns
  return (foo, bar, baz)

parse :: String -> Maybe [Double]
parse = undefined

computeAverageFromFile
  :: FilePath
  -> Eff
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
