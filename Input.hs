
module Input where 

import AutoBench.Types 
import Data.Default 
import Data.List

import Prelude hiding (id)


id :: [Int] -> Int 
id x = head x 

tDat :: UnaryTestData [Int]
tDat  = []

slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys)

ts :: TestSuite 
ts  = def { _dataOpts = Manual "tDat2", _progs = ["lol", "lol"] }


data Test = Test