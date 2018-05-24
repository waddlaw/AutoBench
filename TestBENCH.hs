
module Main (main) where

import Data.Default (def)

import AutoBench.Internal.Benchmarking
import AutoBench.Types
import Input

main :: IO ()
main  = AutoBench.Internal.Benchmarking.runBenchmarks (AutoBench.Internal.Benchmarking.genBenchmarksGenNfUn [("Input.slowRev", Input.slowRev), ("Input.fastRev", Input.fastRev)] ts) ts