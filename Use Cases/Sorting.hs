 
-- In 2002, Ian Lynagh suggested changing Data.List.sort from quicksort to 
-- merge sort because of the worst case behaviour: O(n^2) vs. O(n log_2 n) 
-- respectively. The change was accepted.
--
-- Ian's merge sort was updated at a later date to a smooth merge sort 
-- implementation. I believe this code came from Lennart Augustus but I could 
-- be wrong.
--
-- Some details can be found in the source file:
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/src/Data.OldList.html#sort

module Sorting where 

import Control.DeepSeq     (NFData(..))
import Control.Monad       (replicateM)
import Data.Default        (def)
import Data.List           (sort, sortBy)
import Data.List.Split     (chunksOf)
import Test.QuickCheck     (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose, resize, sample', sized, vectorOf)

import AutoBench.QuickCheck ()
import AutoBench.Types      (TestSuite(..), DataOpts(..))

-------------------------------------------------------------------------------
-- Different kinds of integer lists to test:
-------------------------------------------------------------------------------

newtype SortedIntList             = SortedIntList             [Int] deriving Show
newtype ReverseSortedIntList      = ReverseSortedIntList      [Int] deriving Show
newtype NearlySortedIntList       = NearlySortedIntList       [Int] deriving Show
newtype RandomIntList             = RandomIntList             [Int] deriving Show
newtype StrictlyDecreasingIntList = StrictlyDecreasingIntList [Int] deriving Show 
newtype StrictlyIncreasingIntList = StrictlyIncreasingIntList [Int] deriving Show

-------------------------------------------------------------------------------
-- Generators:
-------------------------------------------------------------------------------

instance Arbitrary SortedIntList where 
  arbitrary = sized $ \n -> SortedIntList . sort <$> vectorOf n arbitrary

instance Arbitrary ReverseSortedIntList where 
  arbitrary = sized $ \n -> ReverseSortedIntList . 
    sortBy (flip compare) <$> vectorOf n arbitrary

-- This works fine for reasonably sized lists but don't use for sizes smaller than ~100.
instance Arbitrary NearlySortedIntList where 
  arbitrary = sized $ \n ->
    if n == 0 
    then return (NearlySortedIntList []) 
    else do

    d <- choose (0.10, 0.25) :: Gen Double                    -- Between 10% and 25% random, rest sorted
    let r = max (round (fromIntegral n * d) :: Int) 1         -- Number of random elements
        s = n - r                                             -- Number of sorted elements

    -- Sorted elements
    SortedIntList xs <- arbitrary :: Gen SortedIntList
    let xs' = drop r xs

    -- Split them up
    i <- choose (1, r)
    let xss = chunksOf i xs'
    -- Max. value in xs
        m   = maximum xs'
    -- Generate random elements outside of the sorted range
    ys <- replicateM r $ choose (m, m + 100)
    -- Split them up 
    j <- choose (1, r)
    let yss = chunksOf j ys

    -- Interleave sorted/random
    return (NearlySortedIntList $ concat $ interleave xss yss)

    where 
      interleave []      y = y
      interleave x      [] = x 
      interleave (a : x) y = a : interleave y x

instance Arbitrary RandomIntList where 
  arbitrary = sized $ \n -> RandomIntList <$> vectorOf n arbitrary

instance Arbitrary StrictlyDecreasingIntList where 
  arbitrary = sized $ \n -> return $ StrictlyDecreasingIntList [n,(n-1)..0]

instance Arbitrary StrictlyIncreasingIntList where 
  arbitrary = sized $ \n -> return $ StrictlyIncreasingIntList [0..n]

-------------------------------------------------------------------------------
-- NFData instances:
-------------------------------------------------------------------------------

instance NFData SortedIntList where 
  rnf (SortedIntList xs) = rnf xs

instance NFData ReverseSortedIntList where 
  rnf (ReverseSortedIntList xs) = rnf xs

instance NFData NearlySortedIntList where
  rnf (NearlySortedIntList xs) = rnf xs

instance NFData RandomIntList where
  rnf (RandomIntList xs) = rnf xs

instance NFData StrictlyDecreasingIntList where
  rnf (StrictlyDecreasingIntList xs) = rnf xs

instance NFData StrictlyIncreasingIntList where
  rnf (StrictlyIncreasingIntList xs) = rnf xs

-------------------------------------------------------------------------------
-- Sampling generators:
-------------------------------------------------------------------------------

sampleSorted :: IO [SortedIntList]
sampleSorted  = sample' $ resize 20 arbitrary

sampleReverseSorted :: IO [ReverseSortedIntList]
sampleReverseSorted  = sample' $ resize 20arbitrary

sampleNearlySorted :: IO [NearlySortedIntList]
sampleNearlySorted  = sample' $ resize 20 arbitrary

sampleRandom :: IO [RandomIntList]
sampleRandom  = sample' $ resize 20 arbitrary

sampleStrictlyDecreasing :: IO [StrictlyDecreasingIntList]
sampleStrictlyDecreasing  = sample' $ resize 20 arbitrary

sampleStrictlyIncreasing :: IO [StrictlyIncreasingIntList]
sampleStrictlyIncreasing  = sample' $ resize 20 arbitrary

-------------------------------------------------------------------------------
-- Test inputs:
-------------------------------------------------------------------------------

-- Merge sort

sort_Random :: RandomIntList -> [Int]
sort_Random (RandomIntList xs) = sort xs 

sort_Sorted :: SortedIntList -> [Int]
sort_Sorted (SortedIntList xs) = sort xs 

-- Old merge sort

ghcMSort_Random :: RandomIntList -> [Int]
ghcMSort_Random (RandomIntList xs) = oldGhcMSort xs

ghcMSort_NearlySorted :: NearlySortedIntList -> [Int]
ghcMSort_NearlySorted (NearlySortedIntList xs) = oldGhcMSort xs 

ghcMSort_ReverseSorted :: ReverseSortedIntList -> [Int]
ghcMSort_ReverseSorted (ReverseSortedIntList xs) = oldGhcMSort xs 

ghcMSort_StrictlyIncreasing :: StrictlyIncreasingIntList -> [Int]
ghcMSort_StrictlyIncreasing (StrictlyIncreasingIntList xs) = oldGhcMSort xs

-- Quicksort 

ghcQSort_Random :: RandomIntList -> [Int]
ghcQSort_Random (RandomIntList xs) = oldGhcQSort xs

ghcQSort_NearlySorted :: NearlySortedIntList -> [Int]
ghcQSort_NearlySorted (NearlySortedIntList xs) = oldGhcQSort xs 

ghcQSort_ReverseSorted :: ReverseSortedIntList -> [Int]
ghcQSort_ReverseSorted (ReverseSortedIntList xs) = oldGhcQSort xs 

ghcQSort_StrictlyIncreasing :: StrictlyIncreasingIntList -> [Int]
ghcQSort_StrictlyIncreasing (StrictlyIncreasingIntList xs) = oldGhcQSort xs 


-------------------------------------------------------------------------------
-- Test suites:
-------------------------------------------------------------------------------

ts_StrictlyIncreasing :: TestSuite
ts_StrictlyIncreasing  = def { _progs = [ "ghcQSort_StrictlyIncreasing"
                                        , "ghcMSort_StrictlyIncreasing" ]
                             , _dataOpts = Gen 0 1000 20000 }

ts_Random :: TestSuite
ts_Random  = def { _progs = [ "ghcQSort_Random"
                            , "ghcMSort_Random" ]
                 , _dataOpts = Gen 0 1000 20000 }

ts_NearlySorted :: TestSuite
ts_NearlySorted  = def { _progs = [ "ghcQSort_NearlySorted"
                                  , "ghcMSort_NearlySorted" ]
                       , _dataOpts = Gen 0 1000 20000 }                           

ts_ReverseSorted :: TestSuite 
ts_ReverseSorted  = def { _progs = [ "ghcQSort_ReverseSorted"
                                   , "ghcMSort_ReverseSorted" ]
                        , _dataOpts = Gen 0 1000 20000 } 

ts_Sorted :: TestSuite 
ts_Sorted  = def { _progs = ["sort_Sorted"]
                 , _dataOpts = Gen 0 100000 2000000 }

ts_NaiveRandomNoOpt :: TestSuite 
ts_NaiveRandomNoOpt  = def { _progs = ["naiveQSort_Random", "sort_Random"]
                           , _dataOpts = Gen 0 10000 200000 }

ts_NaiveRandomO3 :: TestSuite 
ts_NaiveRandomO3  = def { _progs = ["naiveQSort_Random", "sort_Random"]
                        , _dataOpts = Gen 0 10000 200000
                        , _ghcFlags = ["-O3"] }

-------------------------------------------------------------------------------
-- Implementations:
-------------------------------------------------------------------------------

-- GHC's current implementation of merge sort.

ghcMSort :: [Int] -> [Int]
ghcMSort  = sort

-- GHC's implementation of merge sort until 2009.

oldGhcMSort :: [Int] -> [Int]
oldGhcMSort  = mergesort compare

mergesort :: (a -> a -> Ordering) -> [a] -> [a]
mergesort cmp = mergesort' cmp . map wrap

mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesort' _   [] = []
mergesort' _   [xs] = xs
mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)

merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
merge_pairs _   [] = []
merge_pairs _   [xs] = [xs]
merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _   [] ys = ys
merge _   xs [] = xs
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)

wrap :: a -> [a]
wrap x = [x]

-- GHC's implementation of quicksort until 2002.

oldGhcQSort :: [Int] -> [Int]
oldGhcQSort  = flip (qsort compare) []

qsort :: Show a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
qsort _   []       r = r
qsort _   [x]      r = x : r
qsort cmp (x : xs) r = qpart cmp x xs [] [] r

qpart :: Show a => (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart cmp x []       rlt rge r = rqsort cmp rlt (x : rqsort cmp rge r)
qpart cmp x (y : ys) rlt rge r =
  case cmp x y of
    GT -> qpart cmp x ys (y : rlt) rge       r
    _  -> qpart cmp x ys rlt       (y : rge) r
    
rqsort :: Show a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
rqsort _   []       r = r
rqsort _   [x]      r = x : r
rqsort cmp (x : xs) r = rqpart cmp x xs [] [] r

rqpart :: Show a => (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart cmp x [] rle rgt r       = qsort cmp rle (x : qsort cmp rgt r)
rqpart cmp x (y : ys) rle rgt r =
  case cmp y x of
    GT -> rqpart cmp x ys rle       (y : rgt) r
    _  -> rqpart cmp x ys (y : rle) rgt       r
    
-- Naive implementation of merge sort.

naiveMSort :: [Int] -> [Int]
naiveMSort []  = []
naiveMSort [x] = [x]
naiveMSort xs  = merge (naiveMSort $ fstHalf xs) (naiveMSort $ sndHalf xs)
  where 
    fstHalf xs = take (length xs `div` 2) xs
    sndHalf xs = drop (length xs `div` 2) xs

    merge xs [] = xs
    merge [] ys = ys 
    merge (x : xs) (y : ys) 
      | (x <= y)  = x : (merge xs (y : ys)) 
      | otherwise = y : (merge (x : xs) ys)


naiveMSort_Random :: RandomIntList -> [Int]
naiveMSort_Random (RandomIntList xs) = naiveMSort xs 

-- Naive implementation of quicksort.

naiveQSort :: [Int] -> [Int]
naiveQSort []       = []
naiveQSort (x : xs) = naiveQSort small ++ (x : naiveQSort large)
  where 
    small = [y | y <- xs, y <= x]
    large = [y | y <- xs, y > x]

naiveQSort_Random :: RandomIntList -> [Int]
naiveQSort_Random (RandomIntList xs) = naiveQSort xs 









{-

{-# LANGUAGE BangPatterns #-}

-- Smooth merge sort (Data.List.sort):

sequences (a : b : xs) | a `compare` b == GT = descending b [a]   xs
                       | otherwise           = ascending  b (a :) xs
sequences xs = [xs]

descending a as (b : bs) | a `compare` b == GT = descending b (a : as) bs
descending a as bs                             = (a : as) : sequences bs

ascending a as (b : bs) | a `compare` b /= GT = ascending b (\ys -> as (a : ys)) bs
ascending a as bs                             = let !x = as [a] in x : sequences bs

mergeAll [x] = x
mergeAll xs  = mergeAll (mergePairs xs)

mergePairs (a : b : xs) = let !x = merge a b in x : mergePairs xs
mergePairs xs           = xs

merge as@(a : as') bs@(b : bs') | a `compare` b == GT = b : merge as  bs'
                                | otherwise           = a : merge as' bs
merge [] bs = bs
merge as [] = as

-}