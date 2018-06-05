
module Input where 

import Data.Default         (def)
import AutoBench.Types      (DataOpts(..), TestSuite(..))
import AutoBench.QuickCheck ()

appRev :: ([Int], [Int]) -> [Int]
appRev (xs, ys) = reverse xs ++ reverse ys

revApp :: ([Int], [Int]) -> [Int]
revApp (xs, ys) = reverse (ys ++ xs)

ts :: TestSuite 
ts  = def { _dataOpts = Gen 0 10000 200000 }