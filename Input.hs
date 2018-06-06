
module Input where 

import Data.Default (def)
import AutoBench.QuickCheck ()
--import AutoBench.QuickBench
import AutoBench.Types
import AutoBench.Internal.UserInputChecks

slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys)


ts :: TestSuite 
ts  = def

ts2 :: TestSuite 
ts2  = def