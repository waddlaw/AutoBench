
-- The Genuine Sieve of Eratosthenes by Melissa E. O’Neill:
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

module Sieve where 

import           Control.DeepSeq (NFData(..))
import           Data.Default    (def)
import qualified Data.Map        as M 
import           Test.QuickCheck (Arbitrary(..), sized)

import AutoBench.Types (TestSuite(..), DataOpts(..))

-------------------------------------------------------------------------------
-- Instances:
-------------------------------------------------------------------------------

newtype NonNegative = NonNegative Int deriving Show 

instance Arbitrary NonNegative where
 arbitrary = sized (\n -> 
  if n < 0 
  then error $ "invalid NonNegative value: " ++ show n 
  else return $ NonNegative n)

instance NFData NonNegative where 
  rnf (NonNegative n) = rnf n

-------------------------------------------------------------------------------
-- Test inputs:
-------------------------------------------------------------------------------

unfaithfulPrimes :: NonNegative -> [Int] 
unfaithfulPrimes (NonNegative n) = take n (sieve [2..])

truePrimes_List :: NonNegative -> [Int] 
truePrimes_List (NonNegative n) = take n lstPrimes

truePrimes_PQ :: NonNegative -> [Int] 
truePrimes_PQ (NonNegative n) = pqPrimes n

-------------------------------------------------------------------------------
-- Test suites:
-------------------------------------------------------------------------------

ts_unfaithTrueList :: TestSuite 
ts_unfaithTrueList  = def { _dataOpts = Gen 0 5 100
                          , _progs = ["unfaithfulPrimes", "truePrimes_List"] }

ts_trueListTruePQ_1M :: TestSuite 
ts_trueListTruePQ_1M  = def { _dataOpts = Gen 0 50000 1000000
                            , _progs = ["truePrimes_List", "truePrimes_PQ"] }

ts_trueListTruePQ_1MO3 :: TestSuite 
ts_trueListTruePQ_1MO3  = def { _dataOpts = Gen 0 50000 1000000
                              , _progs = ["truePrimes_List", "truePrimes_PQ"]
                              , _ghcFlags = ["-O3"] }

ts_trueListTruePQ_10M:: TestSuite 
ts_trueListTruePQ_10M  = def { _dataOpts = Gen 0 500000 10000000
                             , _progs = ["truePrimes_List", "truePrimes_PQ"] }

-------------------------------------------------------------------------------
-- Implementations:
-------------------------------------------------------------------------------

-- Unfaithful implementation 
 
sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

-- Trial division 

trialPrimes :: [Int]
trialPrimes = 2 : [ x | x <- [3..], isPrime x]

isPrime x = all (\p -> x `mod` p > 0) (factorsToTry x)
  where factorsToTry x = takeWhile (\p -> p * p <= x) trialPrimes
  
-- Incremental: ---------------------------------------------------------------
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

incrPrimes :: Int -> [Int]
incrPrimes  = flip take (incrSieve [2..])

incrSieve xs = sieve' xs M.empty 
  where 
    sieve' []     table = [] 
    sieve' (x:xs) table = case M.lookup x table of 
      Nothing    -> x : sieve' xs (M.insert (x * x) [x] table)
      Just facts -> sieve' xs (foldl reInsert (M.delete x table) facts)
        where reInsert table prime = M.insertWith (++) (x + prime) [prime] table 
        
-- Priority Queue: ------------------------------------------------------------
-- https://github.com/BartMassey/genuine-sieve/blob/master/MPQ.hs

pqPrimes :: Int -> [Int]
pqPrimes  = flip take (2 : pqSieve [3,5..])

pqWheelPrimes :: Int -> [Int]
pqWheelPrimes  = flip take (2 : 3 : 5 : 7 : pqSieve (spin wheel2357 11))
  where 
    wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357
    spin (x : xs) n = n : spin xs (n + x)

pqSieve :: [Int] -> [Int]
pqSieve []       = []
pqSieve (x0 : xs0) = 
  x0 : sieve' xs0 (insertprime x0 xs0 empty)
  where
    insertprime p xs table = insert (p * p) (map (* p) xs) table
    sieve' [] _ = []
    sieve' (x:xs) table
      | nextComposite <= x = sieve' xs (adjust table)
      | otherwise          = x : sieve' xs (insertprime x xs table)
      where
        nextComposite = minKey table
        adjust table'
          | n <= x    = adjust (deleteMinAndInsert n' ns table')
          | otherwise = table'
          where (n, n':ns) = findMin table'

merge :: Ord a => [a] -> [a] -> [a]
merge xs1 [] = xs1
merge [] xs2 = xs2
merge l1@(x1 : xs1) l2@(x2 : xs2) =
  case compare x1 x2 of
    LT -> x1 : merge xs1 l2
    EQ -> x1 : merge xs1 xs2
    GT -> x2 : merge l1 xs2

empty :: M.Map Int [Int]
empty = M.empty

deleteMin :: M.Map Int [Int] ->
             M.Map Int [Int]
deleteMin q = M.deleteMin q

insert :: Int -> [Int] ->
          M.Map Int [Int] ->
          M.Map Int [Int]
insert k xs q =
  case M.lookup k q of
    Nothing -> M.insert k xs q
    Just xs' -> M.insert k (merge xs xs') q

deleteMinAndInsert :: Int -> [Int] -> 
                      M.Map Int [Int] ->
                      M.Map Int [Int]
deleteMinAndInsert k v q =
  insert k v $ deleteMin q

findMin :: M.Map Int [Int] -> (Int, [Int])
findMin q = M.findMin q

minKey :: M.Map Int [Int] -> Int
minKey q = fst $ M.findMin q

-- List-based: ----------------------------------------------------------------
-- By Richard Bird. See epilogue of 
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

lstPrimes :: [Int]
lstPrimes  = 2 : ([3..] `minus` composites)
  where
    composites = union [multiples p | p <- lstPrimes]
    multiples n = map (n *) [n..]
    
    (x:xs) `minus` (y:ys) 
      | x < y  = x : (xs `minus` (y : ys))
      | x == y = xs `minus` ys
      | x > y  = (x : xs) `minus` ys
    
    union = foldr merge []
       where
         merge  (x : xs) ys = x : merge' xs ys
         merge' (x : xs) (y : ys) 
           | x < y  = x : merge' xs (y:ys)
           | x == y = x : merge' xs ys
           | x > y  = y : merge' (x:xs) ys