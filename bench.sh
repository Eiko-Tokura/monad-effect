#!/bin/sh
set -e

# bench :: flag -> directory -> IO ()
function bench() {
  mkdir -p ./benchmark/$2
  cabal run monad-effect-bench -f "$1" -f noinline -- --svg ./benchmark/$2/countdown-shallow-noinline.svg --pattern '$2 == "countdown.shallow"'
  cabal run monad-effect-bench -f "$1" -f noinline -- --svg ./benchmark/$2/countdown-deep-noinline.svg --pattern '$2 == "countdown.deep"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/countdown-shallow.svg --pattern '$2 == "countdown.shallow"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/countdown-deep.svg --pattern '$2 == "countdown.deep"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/nondet-shallow.svg --pattern '$2 == "nondet.shallow"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/nondet-deep.svg --pattern '$2 == "nondet.deep"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/catch-shallow.svg --pattern '$2 == "catch.shallow"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/catch-deep.svg --pattern '$2 == "catch.deep"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/local-shallow.svg --pattern '$2 == "local.shallow"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/local-deep.svg --pattern '$2 == "local.deep"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/coroutine-shallow.svg --pattern '$2 == "coroutine.shallow"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/coroutine-deep.svg --pattern '$2 == "coroutine.deep"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/filesize-shallow.svg --pattern '$2 == "filesize.shallow"'
  cabal run monad-effect-bench -f "$1" -- --svg ./benchmark/$2/filesize-deep.svg --pattern '$2 == "filesize.deep"'
}

bench "benchO2" "bench-result-O2"
bench "benchO1" "bench-result-O1"
bench "benchO0" "bench-result-O0"

