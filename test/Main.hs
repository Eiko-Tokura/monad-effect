{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Monad.Effect
-- import Control.Monad
import Criterion.Main
import Data.HList
import Data.Functor.Identity
import Data.FData
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
testEffState = getS @Int >>= \x ->
  if x < 1_000_000
    then putS (x + 1) >> testEffState
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

testPureState :: Pure '[SModule (Box Int)] NoError ()
testPureState = do
  x <- getsS (unBox @Int)
  if x < 1_000_000
    then do
      putS $ Box (x + 1)
      testPureState
    else return ()

data Box a = Box { unBox :: !a }

newtype TestEff c s a = TestEff
  { runTestEff
    :: c ModuleRead  '[SModule s]
    -> c ModuleState '[SModule s]
    -> (a, c ModuleState '[SModule s])
  }
instance Functor (TestEff c s) where
  fmap f (TestEff g) = TestEff $ \r s -> let (a, s') = g r s in (f a, s')
  {-# INLINE fmap #-}
instance Applicative (TestEff c s) where
  pure a = TestEff $ \_ s -> (a, s)
  {-# INLINE pure #-}
  TestEff f <*> TestEff g = TestEff $ \r s -> let (f', s') = f r s; (a, s'') = g r s' in (f' a, s'')
  {-# INLINE (<*>) #-}
instance Monad (TestEff c s) where
  TestEff f >>= g = TestEff $ \r s -> let (a, s') = f r s; TestEff h = g a in h r s'
  {-# INLINE (>>=) #-}

testTestEff :: TestEff FList Int ()
testTestEff = do
  SState s <- fmap (getEF EZ) $ TestEff $ \_ s -> (s, s)
  if s < 1_000_000
    then do
      TestEff $ \_ _ -> ((), SState (s + 1) :** FNil)
      testTestEff
    else return ()

testTestEffFData :: TestEff FData Int ()
testTestEffFData = do
  SState s <- fmap getFData $ TestEff $ \_ s -> (s, s)
  if s < (1_000_000 :: Int)
    then do
      TestEff $ \_ fd -> ((), modifyFData (\_ -> SState $ s + 1) fd)
      testTestEffFData
    else return ()

testMtlState :: S.StateT Int IO ()
testMtlState = do
  x <- S.get
  if x < 1_000_000
    then do
      S.put (x + 1)
      testMtlState
    else return ()

testMtlStateBox :: S.StateT (Box Int) IO ()
testMtlStateBox = do
  Box x <- S.get
  if x < 1_000_000
    then do
      S.put $ Box (x + 1)
      testMtlStateBox
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
    [ bench "PureEff" $ whnf (\x -> (\(SState (Box s)) -> s) . getEF EZ . snd . runIdentity $ runEffTNoError
        (SRead :** FNil)
        (SState x :** FNil)
        testPureState
      ) (Box 0)
    ]
  , bgroup "TestEff"
    [ bench "TestEff" $ whnf (\x -> (\(SState s) -> s) . getEF EZ . snd $ runTestEff
        testTestEff
        (SRead :** FNil)
        (SState x :** FNil)
      ) 0
    ]
  , bgroup "TestEffFData"
    [ bench "FData" $ whnf (\x -> (\(SState s) -> s) . getFDataByIndex SZero . snd $ runTestEff
        testTestEffFData
        (FData1 SRead)
        (FData1 $ SState x)
      ) (0 :: Int)
    ]
  , bgroup "Mtl State"
    [ bench "StateT" $ whnfIO $ S.runStateT testMtlState 0
    ]
  , bgroup "Mtl State Box"
    [ bench "StateT" $ whnfIO $ S.runStateT testMtlStateBox (Box 0)
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
