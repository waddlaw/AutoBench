
module Input where 

import Data.Default         (def)
import AutoBench.Types      (DataOpts(..), TestSuite(..))
import AutoBench.QuickCheck ()

id_ :: [Int] -> [Int]
id_  = id

appRightId :: [Int] -> [Int]
appRightId xs = xs ++ []

ts :: TestSuite
ts  = def { _dataOpts = Gen 0 10000 200000 }