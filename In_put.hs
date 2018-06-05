
module Input where 

import Data.Default         (def)
import AutoBench.Types         
import AutoBench.QuickCheck ()

appAssocLeft :: ([Int], [Int], [Int]) -> [Int]
appAssocLeft (xs, ys, zs) = (xs ++ ys) ++ zs

appAssocRight :: ([Int], [Int], [Int]) -> [Int]
appAssocRight (xs, ys, zs) = xs ++ (ys ++ zs)

ts :: TestSuite
ts  = def