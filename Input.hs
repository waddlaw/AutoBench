
module Input where 

import Data.Default
import Prelude hiding (id)

import AutoBench.QuickBench
import AutoBench.QuickCheck
import AutoBench.Types 







slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys) 




ts :: TestSuite 
ts  = def { _baseline = True, _progs = ["fastRev"] }

