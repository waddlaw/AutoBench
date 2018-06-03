
module Input where 

import Data.Default
import Prelude hiding (id)

import AutoBench.QuickCheck
import AutoBench.Types 


--import AutoBench.Internal.IO 
--import AutoBench.Internal.Analysis


import AutoBench.QuickBench



{-
slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys) 
-}

id :: Int -> Int 
id x = x

ts :: TestSuite 
ts  = def { _baseline = True }

id2 :: Int -> Int 
id2 = id