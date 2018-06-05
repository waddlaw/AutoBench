
module Input where 

import Prelude hiding       (id)
import Data.Default         (def)
import AutoBench.Types      (DataOpts(..), TestSuite(..))
import AutoBench.QuickCheck ()

id :: [Int] -> [Int]
id x = x

revRev :: [Int] -> [Int]
revRev xs = reverse (reverse xs)

ts :: TestSuite 
ts  = def { _dataOpts = Gen 0 10000 200000 }