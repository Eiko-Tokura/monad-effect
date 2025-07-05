{-# OPTIONS_GHC -fconstraint-solver-iterations=100 -Wno-partial-type-signatures #-}
{-# LANGUAGE DataKinds, GADTs, PartialTypeSignatures #-}
module Main (main) where

import Control.Monad.Effect
import Criterion.Main
import Data.TypeList
import Data.TypeList.FData
import Module.RS
import qualified Control.Monad.State as S

-- The test result shows that for FList there is around (k+1) ns overhead for fetching the k-th state inside the effect system
-- for typical applications with 8 modules,
-- the expected monad access & bind is around 5 ns, which is around 200000000 access & binds per second.
--
-- This means the overhead is completely neglegible in actual applications with effects.
--
-- But of course, in a very very tight loop, you should avoid using Eff, use a single layer of StateT or
-- embed your computation as pure functions.
--
--
-- For FData, the overhead can be eliminated and GHC optimize it very very well
-- even without any optimizations, the speed is 10 times faster than FList
--
-- with -O2 -flate-dmd-anal it can optimize to a minimal hand-written fast loop!

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

testEffStateAs :: Eff '[SModule Int] NoError ()
testEffStateAs = asStateT @Int $ S.mapStateT liftIO loop
  where loop = do
          x <- S.get
          if x < 1_000_000
            then do
              S.put (x + 1)
              loop
            else return ()

testEffEmbed :: _ => EffT' flist '[RModule (), SModule Int, SModule Bool] NoError IO ()
testEffEmbed = do
  x <- embedMods @'[SModule Int] $ do
    x <- getS @Int
    putS (x + 1)
    return x
  if x < 1_000_000
    then do
      modifyS not
      testEffEmbed
    else return ()

main :: IO ()
main = do
  putStrLn "Some sanity checks"
  
  let testFList :: FList Maybe '[Int]
      testFList = Just 1 :** FNil

      unConsHead = fst . unConsF $ testFList

  print $ unConsHead

  print $ getIn @_ @Int testFList

  _ <- runEffTNoError
        (SRead :** FNil)
        (SState (0 :: Int) :** FNil)
        ( do
            liftIO $ putStrLn "Running initial state test"
            x <- getS @Int
            liftIO $ putStrLn $ "Initial state get: " ++ show x
            putS (x + 1)
            liftIO $ putStrLn "State updated"
            y <- getS @Int
            liftIO $ putStrLn $ "State after update: " ++ show y
            if y == x + 1
              then liftIO $ putStrLn "State updated correctly"
              else liftIO $ putStrLn "State updated incorrectly"
        )

  defaultMain $ 
    [ bgroup "Bind"
      [ bench "bind" $ whnfIO $ runEffTNoError
          (SRead :** FNil)
          (SState (0 :: Int) :** FNil)
          ( do
              x <- getS @Int
              putS (x + 1)
              y <- getS @Int
              if y == x + 1
                then return ()
                else liftIO $ putStrLn "State updated incorrectly"
          )
      ]
    , bgroup "State Effect Eff"
      [ bench "FList" $ whnfIO $ runEffTNoError
          (RRead () `FCons` SRead `FCons` SRead `FCons` FNil)
          (RState `FCons` SState 0 `FCons` SState False `FCons` FNil)
          testEffStateFPoly
      , bench "FData" $ whnfIO $ runEffTNoError
          (FData3 (RRead ()) SRead SRead)
          (FData3 (RState) (SState 0) (SState False))
          testEffStateFPoly
      ]
    , bgroup "Embed Eff functionality"
      [ bench "FList" $ whnfIO $ runEffTNoError
          (RRead () `FCons` SRead `FCons` SRead `FCons` FNil)
          (RState `FCons` SState 0 `FCons` SState False `FCons` FNil)
          testEffEmbed
      , bench "FData" $ whnfIO $ runEffTNoError
          (FData3 (RRead ()) SRead SRead)
          (FData3 (RState) (SState 0) (SState False))
          testEffEmbed
      ]
    , bgroup "Mtl State"
      [ bench "StateT" $ whnfIO $ S.runStateT testMtlState ((), 0, False)
      ]
    , bgroup "Embed tight computation in Eff"
      [ bench "EmbedEff" $ whnfIO $ runEffTNoError FNil FNil $ liftIO $ S.runStateT testMtlState ((), 0, False)
      ]
    , bgroup "State Effect As StateT"
      [ bench "Eff" $ whnfIO $ runEffTNoError
          (FData1 SRead)
          (FData1 $ SState 0)
          testEffStateAs
      ]
    ]
