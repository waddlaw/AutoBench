
module Input where 

import Data.Default         (def)
import AutoBench.Types      (DataOpts(..), TestSuite(..))
import AutoBench.QuickCheck ()

appRightId :: [Int] -> [Int]
appRightId xs = xs ++ []

ts :: TestSuite
ts  = def { _dataOpts = Gen 0 10000 200000, _baseline = True }