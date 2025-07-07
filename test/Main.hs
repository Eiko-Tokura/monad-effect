{-# OPTIONS_GHC -fconstraint-solver-iterations=100 -Wno-partial-type-signatures -fmax-worker-args=16 #-}
{-# LANGUAGE DataKinds, GADTs, PartialTypeSignatures #-}
module Main (main) where

import Control.Monad.Effect as ME
import Criterion.Main
import Data.TypeList
import Data.TypeList.FData as ME
import Module.RS as ME
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

programMonadEffect :: ME.In (ME.SModule Int) mods => ME.EffT mods ME.NoError ME.Identity Int
programMonadEffect = do
    x <- ME.getS @Int
    if x == 0
        then pure x
        else do
            ME.putS (x - 1)
            programMonadEffect

countdownMonadEffect :: Int -> (Int, _)
countdownMonadEffect n =
    ME.runIdentity $ ME.runEffTNoError (ME.FData1 ME.SRead) (ME.FData1 $ ME.SState n) $ programMonadEffect

countdownMonadEffectDeep7 :: Int -> (Int, _)
countdownMonadEffectDeep7 n = ME.runIdentity $
    ME.runEffTNoError
      (ME.FData7 readUnit readUnit readUnit SRead readUnit readUnit readUnit) --readUnit SRead readUnit readUnit readUnit readUnit readUnit)
      (ME.FData7 rStateUnit rStateUnit rStateUnit (ME.SState n) rStateUnit rStateUnit rStateUnit) --rStateUnit rStateUnit rStateUnit)
        $ programMonadEffect
      where readUnit = ME.RRead ()
            rStateUnit = ME.RState

countdownMonadEffectDeep11 :: Int -> (Int, _)
countdownMonadEffectDeep11 n = ME.runIdentity $
    ME.runEffTNoError
      (ME.FData15 readUnit readUnit SRead readUnit readUnit readUnit readUnit readUnit readUnit readUnit readUnit readUnit readUnit readUnit readUnit)
      (ME.FData15 rStateUnit rStateUnit (ME.SState n) rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit rStateUnit)
        $ programMonadEffect
      where readUnit = ME.RRead ()
            rStateUnit = ME.RState

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
    , bgroup "Countdown"
      [ bench "Monad Effect" $ nf countdownMonadEffect 1_000_000
      , bench "Monad Effect Deep 7" $ nf countdownMonadEffectDeep7 1_000_000
      , bench "Monad Effect Deep 11" $ nf countdownMonadEffectDeep11 1_000_000
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
