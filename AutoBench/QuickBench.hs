
{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE BangPatterns #-}

{-|

  Module      : AutoBench.QuickBench
  Description : <TO-DO>
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  <TO-DO>

  Note that QuickBench only supports testing with automatically generated test
  data. Use 'AutoBench.Main' for user-specified test data.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - QuickCheck semantic equality checks are missing;
   - 
-}


module AutoBench.QuickBench 
  (

  -- * Datatypes
    GenRange(..)         -- QuickBench test data size range.
  , QuickOpts(..)        -- QuickBench options.
  -- * QuickBench top-level functions: default 'QuickOpts'
  , quickBench           -- QuickBench: unary test programs; test cases evaluated to normal form; default quick options.
  , quickBench'          -- As above but user can specify the names of test programs.
  , quickBenchWhnf       -- QuickBench: unary test programs; test cases evaluated to weak head normal form; default quick options.
  , quickBench2          -- QuickBench: binary test programs; test cases evaluated to normal form; default quick options.
  , quickBenchWhnf2      -- QuickBench: binary test programs; test cases evaluated to weak head normal form; default quick options.
  -- * QuickBench top-level functions: with 'QuickOpts'
  , quickBenchNfWith     -- As above but with user-specified 'QuickOpts'.
  , quickBenchWhnfWith
  , quickBenchNf2With
  , quickBenchWhnf2With

  ) where 

import Control.Applicative    (liftA2)
import Control.DeepSeq        (NFData(..))
import Criterion.Measurement  (measure)
import Data.Default           (Default(..))
import Data.Maybe             (catMaybes)
import Test.QuickCheck        (Arbitrary(..), resize)
import Test.QuickCheck.Gen    (Gen, unGen)
import Test.QuickCheck.Random (QCGen, newQCGen)

import Criterion.Types        
  ( Benchmarkable
  , fromDouble
  , measMutatorCpuSeconds
  , nf
  , whnf
  )

import AutoBench.Internal.Types (AnalOpts, Coord, Coord3)

-- * Datatypes

-- | Size range of test data to be generated for QuickBenching.
-- Default is: @GenRange 0 5 100@
data GenRange = GenRange Int Int Int 

instance Default GenRange where 
  def = GenRange 0 5 100

-- | QuickBench user options. These include:
--
-- * The names of test programs for plotting on graphs and to be included in
--   tables of results;
-- * The size range of generated test data;
-- * Statistical analysis options;
-- * The number of times to run each test case. An average runtime is then 
--   calculated.
--
-- The default QuickBench user options are:
-- @ 
-- QuickOpts
--   { _qProgs    = [ "P1", "P2"... ]
--   , _qGenRange = GenRange 0 5 100
--   , _qAnalOpts = def                 -- see 'AnalOpts'.
--   , _qRuns     = 1
--   }
-- @
data QuickOpts = 
  QuickOpts 
    { _qProgs    :: [String]   -- ^ Names of test programs.
    , _qGenRange :: GenRange   -- ^ Size range of generated test data.
    , _qAnalOpts :: AnalOpts   -- ^ Statistical analysis options.
    , _qRuns     :: Int        -- ^ How many times to run each test case.
    }

instance Default QuickOpts where 
  def = QuickOpts 
          { _qProgs    = fmap (('P' :) . show) ([1..] :: [Int]) 
          , _qGenRange = def 
          , _qAnalOpts = def 
          , _qRuns     = 1
          }

-- * QuickBench top-level functions: default 'QuickOpts'

-- | QuickBench a number of unary test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * Default 'QuickOpts' are used.
quickBench :: (Arbitrary a, NFData a, NFData b) => [(a -> b)] -> IO ()
quickBench  = quickBenchNfWith def

-- | QuickBench a number of unary test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * Default 'QuickOpts' are used.
-- * Specified names override default 'QuickOpts' '_qProgs' list.
quickBench' 
  :: (Arbitrary a, NFData a, NFData b) 
  => [(a -> b)] 
  -> [String]
  -> IO ()
quickBench' ps names = quickBenchNfWith def { _qProgs = names } ps

-- | QuickBench a number of unary test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * Default 'QuickOpts' are used.
quickBenchWhnf :: (Arbitrary a, NFData a) => [(a -> b)] -> IO ()
quickBenchWhnf  = quickBenchWhnfWith def

-- | QuickBench a number of binary test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * Default 'QuickOpts' are used.
quickBench2 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, NFData c) 
  => [(a -> b -> c)] 
  -> IO ()
quickBench2 = quickBenchNf2With def

-- | QuickBench a number of binary test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * Default 'QuickOpts' are used.
quickBenchWhnf2 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b) 
  => [(a -> b -> c)] 
  -> IO ()
quickBenchWhnf2 = quickBenchWhnf2With def

-- * QuickBench top-level functions: with 'QuickOpts'

-- | QuickBench a number of unary test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * User-specified 'QuickOpts'.
quickBenchNfWith
  :: (Arbitrary a, NFData a, NFData b)
  => QuickOpts
  -> [(a -> b)]
  -> IO ()
quickBenchNfWith qOpts ps = do 
  seed <- newQCGen
  let !dats = unGen' seed (genSizedUn $ _qGenRange qOpts)
  coords <- quickBenchNfUn qOpts ps dats
  undefined

-- | QuickBench a number of unary test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * User-specified 'QuickOpts'.
quickBenchWhnfWith 
  :: (Arbitrary a, NFData a)
  => QuickOpts
  -> [(a -> b)]
  -> IO ()
quickBenchWhnfWith qOpts ps = do
  seed <- newQCGen
  let !dats = unGen' seed (genSizedUn $ _qGenRange qOpts)
  coords <- quickBenchWhnfUn qOpts ps dats
  undefined

-- | QuickBench a number of binary test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * User-specified 'QuickOpts'.
quickBenchNf2With
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, NFData c)
  => QuickOpts
  -> [(a -> b -> c)]
  -> IO ()
quickBenchNf2With qOpts ps = do 
  seed <- newQCGen
  let !dats = unGen' seed (genSizedBin $ _qGenRange qOpts)
  coords3 <- quickBenchNfBin qOpts ps dats
  undefined

-- | QuickBench a number of binary test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * User-specified 'QuickOpts'.
quickBenchWhnf2With 
  :: (Arbitrary a, Arbitrary b, NFData a)
  => QuickOpts
  -> [(a -> b -> c)]
  -> IO ()
quickBenchWhnf2With qOpts ps = do 
  seed <- newQCGen
  let !dats = unGen' seed (genSizedBin $ _qGenRange qOpts)
  coords3 <- quickBenchWhnfBin qOpts ps dats
  undefined 

-- * Helpers 

-- ** Generating test data 

-- | Run the generator on the given seed. Note size parameter here doesn't 
-- matter.
unGen' :: QCGen -> Gen a -> a
unGen' seed gen = unGen gen seed 1 

-- | Generator for sized unary test data of a specific type.
genSizedUn :: Arbitrary a => GenRange -> Gen [a]
genSizedUn  = mapM (flip resize arbitrary) . toHRange

-- | Generator for sized binary test data of specific types.
genSizedBin :: (Arbitrary a, Arbitrary b) => GenRange -> Gen [(a, b)]
genSizedBin range = liftA2 (,) <$> genSizedUn range <*> genSizedUn range

-- ** Benchmarking 

-- | Benchmark a number of unary test programs on the same test data. 
-- Test cases are evaluated to normal form. Returns sets of 
-- (input size, runtime) coordinates: one for each test program.
quickBenchNfUn 
  :: NFData b  
  => QuickOpts
  -> [(a -> b)] 
  -> [a]
  -> IO [[Coord]]
quickBenchNfUn qOpts ps !dats = do 
  let benchs = [ fmap (nf p) dats | p <- ps ]           -- Generate benchmarkables.
      runs   = _qRuns qOpts                             -- How many times to execute each benchmarkable?
  times <- mapM (mapM $ qBench runs) benchs             -- Do the benchmarking.
  return $ fmap (verifyTimes $ _qGenRange qOpts) times  -- Verify Criterion's measurements.
  
-- | Benchmark a number of unary test programs on the same test data. 
-- Test cases are evaluated to weak head normal form.
-- Returns sets of (input size, runtime) coordinates: one for each test program.
quickBenchWhnfUn 
  :: QuickOpts
  -> [(a -> b)] 
  -> [a]
  -> IO [[Coord]]
quickBenchWhnfUn qOpts ps !dats = do 
  let benchs = [ fmap (whnf p) dats | p <- ps ]
      runs   = _qRuns qOpts
  times <- mapM (mapM $ qBench runs) benchs
  return $ fmap (verifyTimes $ _qGenRange qOpts) times

-- | Benchmark a number of binary test programs on the same test data. 
-- Test cases are evaluated to normal form.
-- Returns sets of (input size, input size, runtime) coordinates: one for each
-- test program.
quickBenchNfBin
  :: NFData c 
  => QuickOpts
  -> [(a -> b -> c)] 
  -> [(a, b)]
  -> IO [[Coord3]]
quickBenchNfBin qOpts ps !dats = do 
  let benchs = [ fmap (nf $ uncurry p) dats | p <- ps ]
      runs   = _qRuns qOpts
  times <- mapM (mapM $ qBench runs) benchs
  return $ fmap (verifyTimes3 $ _qGenRange qOpts) times

-- | Benchmark a number of binary test programs on the same test data. 
-- Test cases are evaluated to weak head normal form.
-- Returns sets of (input size, input size, runtime) coordinates: one for each
-- test program.
quickBenchWhnfBin
  :: QuickOpts
  -> [(a -> b -> c)] 
  -> [(a, b)]
  -> IO [[Coord3]]
quickBenchWhnfBin qOpts ps !dats = do 
  let benchs = [ fmap (whnf $ uncurry p) dats | p <- ps ]
      runs   = _qRuns qOpts
  times <- mapM (mapM $ qBench runs) benchs
  return $ fmap (verifyTimes3 $ _qGenRange qOpts) times

-- | Execute a benchmarkable for @n@ iterations measuring the total number of
-- CPU seconds taken. Then calculate the average.
qBench :: Int -> Benchmarkable -> IO Double
qBench n b = (/ fromIntegral n) . measMutatorCpuSeconds . 
  fst <$> measure b (fromIntegral n)

-- | Convert a 'GenRange' to a Haskell range.
toHRange :: GenRange -> [Int]
toHRange (GenRange l s u) = [l, (l + s) .. u]

-- | Verify Criterion measurements while producing (input size, runtime)
-- coordinates for test results relating to unary test programs.
verifyTimes :: GenRange -> [Double] -> [Coord]
verifyTimes range times = catMaybes $ zipWith (\s m -> fromDouble m >>= \d -> 
  Just (fromIntegral s, d)) (toHRange range) times

-- | Verify Criterion measurements while producing (input size, input size, 
-- runtime) coordinates for test results relating to binary test programs.
verifyTimes3 :: GenRange -> [Double] ->  [Coord3]
verifyTimes3 range times = catMaybes $ zipWith (\(s1, s2) m -> fromDouble m 
  >>= \d -> Just (fromIntegral s1, fromIntegral s2, d)) sizes times
  where 
    sizes  = (,) <$> hRange <*> hRange
    hRange = toHRange range