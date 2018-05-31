
module Input where 

import AutoBench.Types 
import Data.Default 
import Data.List



import AutoBench.Internal.IO 
import AutoBench.Internal.UserInputChecks
import AutoBench.Internal.Types
import AutoBench.Internal.Analysis

import Prelude hiding (id, const)



slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys) 



ts :: TestSuite 
ts  = def { _progs = ["slowRev", "fastRev"], _baseline = True, _ghcFlags = ["lol"] }


const :: Int -> Int -> Int
const x y = x 

const2 :: Int -> Int -> Int
const2 x y = x 