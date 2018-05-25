
module Main (main) where

import qualified AutoBench.Internal.Benchmarking
import qualified Input

main :: IO ()
main  = AutoBench.Internal.Benchmarking.runBenchmarks (AutoBench.Internal.Benchmarking.genBenchmarksGenNfUn [("Input.slowRev", Input.slowRev), ("Input.fastRev", Input.fastRev)] ts) ts