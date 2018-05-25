
module Input where 

import AutoBench.Internal.Types 
import Data.Default 
import Data.List

import Prelude hiding (id)


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
ts  = def { _dataOpts = Manual "tDat", _baseline = True }