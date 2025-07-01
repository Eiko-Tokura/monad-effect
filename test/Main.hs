{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Monad.Effect
import Criterion.Main
import Data.HList
import Data.Functor.Identity
import Module.RS
import qualified Control.Monad.State as S

-- The test result shows that there is around (k+1) ns overhead for fetching the k-th state inside the effect system
-- for typical applications with 8 modules,
-- the expected monad access & bind is around 5 ns, which is around 200000000 access & binds per second.
--
-- This means the overhead is completely neglegible in actual applications with effects.
--
-- But of course, in a very very tight loop, you should avoid using Eff, use a single layer of StateT or
-- embed your computation as pure functions.

testEffState :: Eff '[SModule Int] NoError ()
testEffState = do
  x <- getS @Int
  if x < 1_000_000
    then do
      putS (x + 1)
      testEffState
    else return ()

testEffStateAs :: Eff '[SModule Int] NoError ()
testEffStateAs = asStateT @Int $ S.mapStateT liftIO loop
  where loop = do
          x <- S.get
          if x < 1_000_000
            then do
              S.put (x + 1)
              loop
            else return ()

testPureState :: Pure '[SModule Int] NoError ()
testPureState = do
  x <- getS @Int
  if x < 1_000_000
    then do
      putS (x + 1)
      testPureState
    else return ()

testMtlState :: S.StateT Int IO ()
testMtlState = do
  x <- S.get
  if x < 1_000_000
    then do
      S.put (x + 1)
      testMtlState
    else return ()

testStateTEff :: S.StateT Int (Pure '[] NoError) ()
testStateTEff = do
  x <- S.get
  if x < 1_000_000
    then do
      S.put (x + 1)
      testStateTEff
    else return ()

main :: IO ()
main = defaultMain $ 
  [ bgroup "State Effect"
    [ bench "Eff" $ whnfIO $ runEffTNoError
        (SRead :** FNil)
        (SState 0 :** FNil)
        testEffState
    ]
  , bgroup "State Effect As StateT"
    [ bench "Eff" $ whnfIO $ runEffTNoError
        (SRead :** FNil)
        (SState 0 :** FNil)
        testEffStateAs
    ]
  , bgroup "Pure State Effect"
    [ bench "PureEff" $ whnf (\x -> (\(SState s) -> s) . getEF EZ . snd . runIdentity $ runEffTNoError
        (SRead :** FNil)
        (SState x :** FNil)
        testPureState
      ) 0
    ]
  , bgroup "Mtl State"
    [ bench "StateT" $ whnfIO $ S.runStateT testMtlState 0
    ]
  , bgroup "StateT in Eff"
    [ bench "StateTEff" $ whnf (\x -> (\(SState s) -> s) . getEF EZ . snd . runIdentity $
          runEffTNoError
          (SRead :** FNil)
          (SState x :** FNil)
        $ addStateT testStateTEff) 0
    ]
  , bgroup "Embed tight computation in Eff"
    [ bench "EmbedEff" $ whnfIO $ runEffTNoError FNil FNil $ liftIO $ S.runStateT testMtlState 0
    ]
  ]
