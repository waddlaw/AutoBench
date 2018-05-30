
module Input where 

import AutoBench.Types 
import Data.Default 
import Data.List

import Prelude hiding (id, const)



slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys) 

tDat :: UnaryTestData [Int]
tDat  = take 20 $ zip [1..] $ fmap return (repeat [1])

ts :: TestSuite 
ts  = def { _progs = ["slowRev", "fastRev"], _baseline = True }


ts3 :: TestSuite 
ts3 = def { _dataOpts = Gen 0 5 20, _progs = ["const", "const2"] }

ts2 :: TestSuite
ts2 = def

const :: [Int] -> [Int] -> [Int]
const x y = x 

const2 :: [Int] -> [Int] -> [Int]
const2 x y = x 