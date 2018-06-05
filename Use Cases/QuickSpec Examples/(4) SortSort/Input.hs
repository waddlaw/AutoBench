
module Input where 

import           Data.Default         (def)
import qualified Data.List            as List
import           AutoBench.Types      (DataOpts(..), TestSuite(..))
import           AutoBench.QuickCheck ()

sort :: [Int] -> [Int]
sort  = List.sort 

sortSort :: [Int] -> [Int]
sortSort xs = List.sort (List.sort xs)

ts :: TestSuite 
ts  = def { _dataOpts = Gen 0 10000 200000 }