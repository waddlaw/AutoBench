
module Input where 

import Data.Default         (def)
import AutoBench.Types      (DataOpts(..), TestSuite(..))
import AutoBench.QuickCheck ()


plus :: (Int, Int) -> Int 
plus (x, y) = x + y 

plusComm :: (Int, Int) -> Int 
plusComm (x, y) = y + x

-------------------------------------------------------------------------------

plusAssocLeft :: (Int, Int, Int) -> Int 
plusAssocLeft (x, y, z) = (z + y) + x

-- Note the argument re-ordering too.
plusAssocRight :: (Int, Int, Int) -> Int   
plusAssocRight (x, y, z) = y + (x + z)

-------------------------------------------------------------------------------

plusDistr :: (Int, Int, Int) -> Int 
plusDistr (x, y, z) = (x + y) + (x + z) 

plusNonDistr :: (Int, Int, Int) -> Int 
plusNonDistr (x, y, z) = (z + y) + (x + x)

-------------------------------------------------------------------------------

ts_plusComm :: TestSuite
ts_plusComm  = def { _progs = ["plus", "plusComm'"] }

ts_plusAssoc :: TestSuite
ts_plusAssoc  = def { _progs = ["plusAssocLeft", "plusAssocRight'"] }

ts_plusDistr :: TestSuite
ts_plusDistr  = def { _progs = ["plusDistr", "plusNonDistr'"] }