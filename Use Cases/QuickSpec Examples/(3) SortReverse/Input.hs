
module Input where 

import           Data.Default         (def)
import qualified Data.List            as List
import           AutoBench.Types      (DataOpts(..), TestSuite(..))
import           AutoBench.QuickCheck ()

sort :: [Int] -> [Int]
sort  = List.sort 

sortRev :: [Int] -> [Int]
sortRev xs = List.sort (reverse xs)

ts :: TestSuite 
ts  = def { _dataOpts = Gen 0 100000 2000000 }