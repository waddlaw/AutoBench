
module Input where 

import           Control.DeepSeq      (NFData(..))
import           Data.Default         (def)  
import qualified Data.Heap.Leftist    as Heap
import           Data.Maybe           (fromJust)
import qualified Data.List            as List
import           Test.QuickCheck      (Arbitrary(..), sized)
import           Test.QuickCheck
import           Prelude              hiding (id)
import           AutoBench.Types      (DataOpts(..), TestSuite(..))
import           AutoBench.QuickCheck ()


-- WARNING: Data.Heap.Leftist is not what I expected. Do not run these test 
-- suites for now.

data SizedInt = SizedInt Int

instance Arbitrary SizedInt where 
  arbitrary = sized $ \n -> return (SizedInt n)

instance NFData SizedInt where 
  rnf (SizedInt n) = rnf n

instance (Ord a, Arbitrary a) => Arbitrary (Heap.Leftist a) where 
  arbitrary = sized $ \n -> Heap.fromList <$> resize n arbitrary

instance NFData a => NFData (Heap.Leftist a) where 
  rnf Heap.Leaf = ()
  rnf (Heap.Node rank l x r) = rnf rank `seq` rnf l `seq` rnf x `seq` rnf r

-------------------------------------------------------------------------------

id :: SizedInt -> Int 
id (SizedInt x) = x 

findEmpty :: SizedInt -> Int 
findEmpty (SizedInt x) = fromJust $ Heap.minimum (Heap.insert x Heap.empty)

-------------------------------------------------------------------------------

insertInsertEmpty :: (SizedInt, SizedInt) -> Heap.Leftist Int 
insertInsertEmpty (SizedInt x, SizedInt y) = Heap.insert x (Heap.insert y Heap.empty) 

insertInsertEmpty' :: (SizedInt, SizedInt) -> Heap.Leftist Int 
insertInsertEmpty' (SizedInt x, SizedInt y) = Heap.insert y (Heap.insert x Heap.empty) 

-------------------------------------------------------------------------------

-- This doesn't sort?!
toListFromList :: [Int] -> [Int]
toListFromList xs = Heap.toList (Heap.fromList xs)

sort :: [Int] -> [Int]
sort  = List.sort 

-------------------------------------------------------------------------------

-- Heap.merge h1 h == Heap.merge h h1
-- Heap.merge h1 (Heap.merge h h2) == Heap.merge h (Heap.merge h1 h2)
-- Heap.merge h (Heap.insert x h1) == Heap.insert x (Heap.merge h h1)
-- Heap.merge h (Heap.deleteMin h) == Heap.deleteMin (Heap.merge h h)
-- sort (toList h) == toList h
-- fromList (toList h) == h
-- fromList (sort xs) == fromList xs
-- fromList (ys++xs) == fromList (xs++ys)
-- insert x (fromList xs) == fromList (x:xs)
-- merge (fromList xs) (fromList ys) == fromList (xs++ys)
-- head (toList h) == findMin h

-------------------------------------------------------------------------------

toListMin :: Heap.Leftist Int -> [Int]
toListMin h = Heap.toList (Heap.deleteMin h) 

tailToList :: Heap.Leftist Int -> [Int]
tailToList h = tail (Heap.toList h)

-------------------------------------------------------------------------------

-- Do not run these!

ts_findMin :: TestSuite
ts_findMin  = def { _progs = ["findEmpty", "id"]
                  , _dataOpts = Gen 0 5000 100000 }

ts_insertInsert :: TestSuite
ts_insertInsert  = def { _progs = ["insertInsertEmpty", "insertInsertEmpty'"]
                       , _dataOpts = Gen 0 5000 100000 }

ts_toListFromList :: TestSuite
ts_toListFromList  = def { _progs = ["toListFromList", "sort"]
                         , _dataOpts = Gen 0 5000 100000 }

ts_toListMin :: TestSuite
ts_toListMin  = def { _progs = ["toListMin", "tailToList"]
                    , _dataOpts = Gen 5 5000 100000 }