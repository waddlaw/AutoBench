
{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE BangPatterns #-}

{-|

  Module      : AutoBench.QuickBench
  Description : Main module for QuickBenching.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module exports all of QuickBench's functionality.

  There is no AutoBench/QuickBench manual yet, but see
  http://www.cs.nott.ac.uk/~psxmah/ for videos introducing the system.
  An introductory paper, "AutoBench: comparing the time performance of Haskell 
  programs", is also accessible via the same link.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - Commenting;
   -
-}

module AutoBench.QuickBench 
  (

  -- * Datatypes
    QGen(..)             -- QuickBench test data size range.
  , QuickOpts(..)        -- QuickBench options.
  
  -- * QuickBench: no EQ
  -- ** QuickBench top-level functions: default 'QuickOpts'
  , quickBench           -- QuickBench: unary test programs; test cases evaluated to normal form; default quick options.
  , quickBench'          -- As above but user can specify the names of test programs.
  , quickBenchWhnf       -- QuickBench: unary test programs; test cases evaluated to weak head normal form; default quick options.
  , quickBench2          -- QuickBench: binary test programs; test cases evaluated to normal form; default quick options.
  , quickBenchWhnf2      -- QuickBench: binary test programs; test cases evaluated to weak head normal form; default quick options.
  -- ** QuickBench top-level functions: with 'QuickOpts'
  , quickBenchNfWith     -- As above but with user-specified 'QuickOpts'.
  , quickBenchWhnfWith
  , quickBenchNf2With
  , quickBenchWhnf2With

  -- * QuickBench: with EQ
  -- ** QuickBench top-level functions: default 'QuickOpts'
  , _quickBench           -- QuickBench: unary test programs; test cases evaluated to normal form; default quick options.
  , _quickBench'          -- As above but user can specify the names of test programs.
  , _quickBenchWhnf       -- QuickBench: unary test programs; test cases evaluated to weak head normal form; default quick options.
  , _quickBench2          -- QuickBench: binary test programs; test cases evaluated to normal form; default quick options.
  , _quickBenchWhnf2      -- QuickBench: binary test programs; test cases evaluated to weak head normal form; default quick options.
  -- ** QuickBench top-level functions: with 'QuickOpts'
  , _quickBenchNfWith     -- As above but with user-specified 'QuickOpts'.
  , _quickBenchWhnfWith
  , _quickBenchNf2With
  , _quickBenchWhnf2With

  ) where 

import Control.Applicative    (liftA2)
import Control.DeepSeq        (NFData(..), deepseq)
import Criterion.Measurement  (measure)
import Data.Default           (Default(..))
import Data.Maybe             (catMaybes)
import Test.QuickCheck        ( Arbitrary(..), Args(..), quickCheckWithResult 
                              , resize, stdArgs )
import Test.QuickCheck.Gen    (Gen, sample', unGen)
import Test.QuickCheck.Random (QCGen, newQCGen)
import Test.QuickCheck.Test   (isSuccess)

import Criterion.Types        
  ( Benchmarkable
  , fromDouble
  , measMutatorCpuSeconds
  , nf
  , whnf
  )

import AutoBench.Internal.AbstractSyntax  (Id)
import AutoBench.Internal.Analysis        (quickAnalyseWith)
import AutoBench.Internal.Configuration   (minimumTestInputs)
import AutoBench.Internal.Types           ( AnalOpts(..), InputError(..)
                                          , QuickReport(..) )
import AutoBench.Internal.UserInputChecks (validateAnalOpts)
import AutoBench.Internal.Utils           (allEq)

-- * Datatypes

-- | Size range of test data to be generated for QuickBenching.
-- Default is: @QGen 0 5 200@
data QGen = QGen Int Int Int 

instance Default QGen where 
  def = QGen 0 5 200

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
--   , _qGen      = QGen 0 5 200
--   , _qAnalOpts = def { _topModels = 3 }         -- see 'AnalOpts'.
--   , _qRuns     = 1
--   }
-- @
data QuickOpts = 
  QuickOpts 
    { 
      _qProgs    :: [String]   -- ^ Names of test programs.
    , _qGen      :: QGen       -- ^ Size range of generated test data.
    , _qAnalOpts :: AnalOpts   -- ^ Statistical analysis options.
    , _qRuns     :: Int        -- ^ How many times to run each test case.
    }

instance Default QuickOpts where 
  def = QuickOpts 
          { 
            _qProgs    = fmap (('P' :) . show) ([1..] :: [Int]) 
          , _qGen      = def 
          , _qAnalOpts = def { _topModels = 3 }
          , _qRuns     = 1
          }

-- * QuickBench: no EQ

-- ** QuickBench top-level functions: default 'QuickOpts'

-- | QuickBench a number of /unary/ test programs using generated test data:
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

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * Default 'QuickOpts' are used.
quickBenchWhnf :: (Arbitrary a, NFData a) => [(a -> b)] -> IO ()
quickBenchWhnf  = quickBenchWhnfWith def

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * Default 'QuickOpts' are used.
quickBench2 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, NFData c) 
  => [(a -> b -> c)] 
  -> IO ()
quickBench2 = quickBenchNf2With def

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * Default 'QuickOpts' are used.
quickBenchWhnf2 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b) 
  => [(a -> b -> c)] 
  -> IO ()
quickBenchWhnf2 = quickBenchWhnf2With def

-- * QuickBench top-level functions: with 'QuickOpts'

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * User-specified 'QuickOpts'.
quickBenchNfWith
  :: (Arbitrary a, NFData a, NFData b)
  => QuickOpts
  -> [(a -> b)]
  -> IO ()
quickBenchNfWith _ [] = noTestProgramsError
quickBenchNfWith qOpts ps 
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedUn $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchNfUn qOpts ps dats -- QuickReports.
      quickAnalyseWith (_qAnalOpts qOpts) False qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts True qOpts  -- True for unary test programs.

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * User-specified 'QuickOpts'.
quickBenchWhnfWith 
  :: (Arbitrary a, NFData a)
  => QuickOpts
  -> [(a -> b)]
  -> IO ()
quickBenchWhnfWith _ [] = noTestProgramsError
quickBenchWhnfWith qOpts ps 
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedUn $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchWhnfUn qOpts ps dats
      quickAnalyseWith (_qAnalOpts qOpts) False qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts True qOpts  -- True for unary test programs.

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * User-specified 'QuickOpts'.
quickBenchNf2With
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, NFData c)
  => QuickOpts
  -> [(a -> b -> c)]
  -> IO ()
quickBenchNf2With _ [] = noTestProgramsError
quickBenchNf2With qOpts ps
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedBin $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchNfBin qOpts ps dats
      quickAnalyseWith (_qAnalOpts qOpts) False qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts False qOpts  -- False for binary test programs.

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * User-specified 'QuickOpts'.
quickBenchWhnf2With 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b)
  => QuickOpts
  -> [(a -> b -> c)]
  -> IO ()
quickBenchWhnf2With _ [] = noTestProgramsError
quickBenchWhnf2With qOpts ps
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedBin $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchWhnfBin qOpts ps dats
      quickAnalyseWith (_qAnalOpts qOpts) False qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts False qOpts  -- False for binary test programs.

-- * QuickBench: with EQ.

-- ** QuickBench top-level functions: default 'QuickOpts'

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * Default 'QuickOpts' are used.
_quickBench :: (Arbitrary a, NFData a, NFData b, Eq b) => [(a -> b)] -> IO ()
_quickBench  = _quickBenchNfWith def

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * Default 'QuickOpts' are used.
-- * Specified names override default 'QuickOpts' '_qProgs' list.
_quickBench' 
  :: (Arbitrary a, NFData a, NFData b, Eq b) 
  => [(a -> b)] 
  -> [String]
  -> IO ()
_quickBench' ps names = _quickBenchNfWith def { _qProgs = names } ps

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * Default 'QuickOpts' are used.
_quickBenchWhnf :: (Arbitrary a, NFData a, Eq b) => [(a -> b)] -> IO ()
_quickBenchWhnf  = _quickBenchWhnfWith def

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * Default 'QuickOpts' are used.
_quickBench2 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, NFData c, Eq c) 
  => [(a -> b -> c)] 
  -> IO ()
_quickBench2 = _quickBenchNf2With def

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * Default 'QuickOpts' are used.
_quickBenchWhnf2 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, Eq c) 
  => [(a -> b -> c)] 
  -> IO ()
_quickBenchWhnf2 = _quickBenchWhnf2With def

-- * QuickBench top-level functions: with 'QuickOpts'

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * User-specified 'QuickOpts'.
_quickBenchNfWith
  :: (Arbitrary a, NFData a, NFData b, Eq b)
  => QuickOpts
  -> [(a -> b)]
  -> IO ()
_quickBenchNfWith _ [] = noTestProgramsError
_quickBenchNfWith qOpts ps 
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedUn $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchNfUn qOpts ps dats -- QuickReports.
      b     <- quickCheckUn ps
      quickAnalyseWith (_qAnalOpts qOpts) b qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts True qOpts  -- True for unary test programs.

-- | QuickBench a number of /unary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * User-specified 'QuickOpts'.
_quickBenchWhnfWith 
  :: (Arbitrary a, NFData a, Eq b)
  => QuickOpts
  -> [(a -> b)]
  -> IO ()
_quickBenchWhnfWith _ [] = noTestProgramsError
_quickBenchWhnfWith qOpts ps
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedUn $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchWhnfUn qOpts ps dats
      b     <- quickCheckUn ps
      quickAnalyseWith (_qAnalOpts qOpts) b qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts True qOpts  -- True for unary test programs.

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to normal form;
-- * User-specified 'QuickOpts'.
_quickBenchNf2With
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, NFData c, Eq c)
  => QuickOpts
  -> [(a -> b -> c)]
  -> IO ()
_quickBenchNf2With _ [] = noTestProgramsError
_quickBenchNf2With qOpts ps
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedBin $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchNfBin qOpts ps dats
      b     <- quickCheckBin ps
      quickAnalyseWith (_qAnalOpts qOpts) b qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts False qOpts  -- False for binary test programs.

-- | QuickBench a number of /binary/ test programs using generated test data:
-- 
-- * Test cases are evaluated to weak head normal form;
-- * User-specified 'QuickOpts'.
_quickBenchWhnf2With 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, Eq c)
  => QuickOpts
  -> [(a -> b -> c)]
  -> IO ()
_quickBenchWhnf2With _ [] = noTestProgramsError
_quickBenchWhnf2With qOpts ps 
  | null errs = do 
      seed <- newQCGen
      let dats = unGen' seed (genSizedBin $ _qGen qOpts)
      qReps <- dats `deepseq` quickBenchWhnfBin qOpts ps dats
      b     <- quickCheckBin ps
      quickAnalyseWith (_qAnalOpts qOpts) b qReps
  | otherwise = quickOptsErrors errs
  where errs = validateQuickOpts False qOpts  -- False for binary test programs.

-- * Helpers 

-- ** Generating test data 

-- | Run the generator on the given seed. Note size parameter here doesn't 
-- matter.
unGen' :: QCGen -> Gen a -> a
unGen' seed gen = unGen gen seed 1 

-- | Generator for sized unary test data of a specific type.
genSizedUn :: Arbitrary a => QGen -> Gen [a]
genSizedUn  = mapM (flip resize arbitrary) . toHRange

-- | Generator for sized binary test data of specific types.
genSizedBin :: (Arbitrary a, Arbitrary b) => QGen -> Gen [(a, b)]
genSizedBin range = liftA2 (,) <$> genSizedUn range <*> genSizedUn range

-- ** Benchmarking 

-- | Benchmark a number of unary test programs on the same test data. 
-- Test cases are evaluated to normal form. 
-- Returns a list of 'QuickReport's: one for each test program.
quickBenchNfUn 
  :: (NFData a, NFData b) 
  => QuickOpts
  -> [(a -> b)] 
  -> [a]
  -> IO [QuickReport]
quickBenchNfUn _ _ [] = return []
quickBenchNfUn qOpts ps !dats = do 
  let benchs = [ fmap (nf p) dats | p <- ps ]                           -- Generate Benchmarkables.
      runs   = _qRuns qOpts                                             -- How many times to execute each Benchmarkable?
  times <- mapM (mapM $ qBench runs) benchs                             -- Do the benchmarking.
  return $ zipWith (flip verifyTimesUn $ _qGen qOpts) names times       -- Verify Criterion's measurements and generate 'QuickReport's.
  where names = genNames (_qProgs qOpts)
  
-- | Benchmark a number of unary test programs on the same test data. 
-- Test cases are evaluated to weak head normal form.
-- Returns a list of 'QuickReport's: one for each test program.
quickBenchWhnfUn 
  :: NFData a
  => QuickOpts
  -> [(a -> b)] 
  -> [a]
  -> IO [QuickReport]
quickBenchWhnfUn _ _ [] = return []
quickBenchWhnfUn qOpts ps dats = do 
  let benchs = [ fmap (whnf p) dats | p <- ps ]
      runs   = _qRuns qOpts
  times <- mapM (mapM $ qBench runs) benchs
  return $ zipWith (flip verifyTimesUn $ _qGen qOpts) names times
  where names = genNames (_qProgs qOpts)

-- | Benchmark a number of binary test programs on the same test data. 
-- Test cases are evaluated to normal form.
-- Returns a list of 'QuickReport's: one for each test program.
quickBenchNfBin
  :: (NFData a, NFData b, NFData c) 
  => QuickOpts
  -> [(a -> b -> c)] 
  -> [(a, b)]
  -> IO [QuickReport]
quickBenchNfBin _ _ [] = return []
quickBenchNfBin qOpts ps dats = do 
  let benchs = [ fmap (nf $ uncurry p) dats | p <- ps ]
      runs   = _qRuns qOpts
  times <- mapM (mapM $ qBench runs) benchs
  return $ zipWith (flip verifyTimesBin $ _qGen qOpts) names times
  where names = genNames (_qProgs qOpts)

-- | Benchmark a number of binary test programs on the same test data. 
-- Test cases are evaluated to weak head normal form.
-- Returns a list of 'QuickReport's: one for each test program.
quickBenchWhnfBin
  :: (NFData a, NFData b)
  => QuickOpts
  -> [(a -> b -> c)] 
  -> [(a, b)]
  -> IO [QuickReport]
quickBenchWhnfBin _ _ [] = return []
quickBenchWhnfBin qOpts ps dats = do 
  let benchs = [ fmap (whnf $ uncurry p) dats | p <- ps ]
      runs   = _qRuns qOpts
  times <- mapM (mapM $ qBench runs) benchs
  return $ zipWith (flip verifyTimesBin $ _qGen qOpts) names times
  where names = genNames (_qProgs qOpts)

-- | Execute a benchmarkable for @n@ iterations measuring the total number of
-- CPU seconds taken. Then calculate the average.
qBench :: Int -> Benchmarkable -> IO Double
qBench n b = do 
  throwAway <- measMutatorCpuSeconds . fst <$> measure b 1 
  d <- throwAway `deepseq` (measMutatorCpuSeconds . fst <$> measure b (fromIntegral n))
  return (d / fromIntegral n)




-- ** Generating reports

-- | Verify Criterion measurements while producing a 'QuickReport'
-- for test results relating to unary test programs.
verifyTimesUn :: Id -> QGen -> [Double] -> QuickReport
verifyTimesUn idt range times = 
  QuickReport 
    {
      _qName     = idt
    , _qRuntimes = Left $ catMaybes $ zipWith (\s m -> fromDouble m >>= \d -> 
        Just (fromIntegral s, d)) (toHRange range) times
    }

-- | Verify Criterion measurements while producing a 'QuickReport'
-- for test results relating to binary test programs.
verifyTimesBin :: Id -> QGen -> [Double] -> QuickReport
verifyTimesBin idt range times = 
  QuickReport 
    {
      _qName     = idt
    , _qRuntimes = Right $ catMaybes $ zipWith (\(s1, s2) m -> fromDouble m 
        >>= \d -> Just (fromIntegral s1, fromIntegral s2, d)) sizes times
    }
  where 
    sizes  = (,) <$> hRange <*> hRange
    hRange = toHRange range

-- ** QuickCheck testing

-- | Check whether test programs are semantically equal using QuickCheck.
-- For unary test programs.
quickCheckUn :: (Arbitrary a, Eq b) => [a -> b] -> IO Bool
quickCheckUn ps = do                                             
  tDats <- sample' arbitrary                                    -- Generate some inputs.
  isSuccess <$> quickCheckWithResult stdArgs { chatty = False } -- Turn off QuickCheck output.
    (and [ allEq [ p tDat | p <- ps ] | tDat <- tDats ])        -- Check whether test programs give same results.

-- | Check whether test programs are semantically equal using QuickCheck.
-- For binary test programs.
quickCheckBin :: (Arbitrary a, Arbitrary b, Eq c) => [a -> b -> c] -> IO Bool 
quickCheckBin ps = do
  tDats <- sample' arbitrary                                    -- Generate pairs of inputs.
  isSuccess <$> quickCheckWithResult stdArgs { chatty = False } 
    (and [ allEq [ p tDat1 tDat2 | p <- ps ] | (tDat1, tDat2) <- tDats ])  

-- ** Validation 

-- | Validate 'QuickOpts' to ensure test settings are acceptable.
validateQuickOpts :: Bool -> QuickOpts -> [InputError] 
validateQuickOpts unary qOpts = 
  validQGen unary     (_qGen      qOpts)
  ++ validateAnalOpts (_qAnalOpts qOpts)  -- See AutoBench.Internal.UserInputChecks.
  ++ validRuns        (_qRuns     qOpts)

  where
    -- '_qRuns' must be strictly positive.
    validRuns :: Int -> [InputError]
    validRuns runs 
      | runs > 0  = []
      | otherwise = [qRunsErr]

    -- Minimum of 'minimumTestInputs' test cases. 
    validQGen :: Bool -> QGen -> [InputError]
    validQGen True (QGen l s u)                                                  -- True for unary test programs.
      | l < 0 || s <= 0 || u <= 0       = [qGenParErr]                           -- l >= 0, s > 0, u > 0.
      | (u - l) `div` s + 1 < minimumTestInputs = [qGenSizeErr]                  -- Size range >= minimumTestInputs.
      | otherwise = []
    validQGen False (QGen l s u)                                                 -- False for binary test programs.
      | l < 0 || s <= 0 || u <= 0                      = [qGenParErr]            -- l >= 0, s > 0, u > 0.
      | ((u - l) `div` s + 1) ^ (2 :: Int) < minimumTestInputs = [qGenSizeErr]   -- Size range >= minimumTestInputs.
      | otherwise = []

    -- Errors:
    qRunsErr    = QuickOptsErr "Number of test program runs must be strictly positive."
    qGenParErr  = QuickOptsErr "Invalid values for 'QGen' bounds and/or step." 
    qGenSizeErr = QuickOptsErr $ "A minimum of " ++ show minimumTestInputs ++ " test inputs are required."

-- | Output error for no test programs.
noTestProgramsError :: IO ()
noTestProgramsError  = print $ QuickBenchErr "No test programs."

-- | Output 'QuickOpts' errors.
quickOptsErrors :: [InputError] -> IO ()
quickOptsErrors  = mapM_ print 

-- ** Misc.

-- | Generate an infinite number of test program names.
genNames :: [String] -> [String]
genNames ns = ns ++ fmap (('P' :) . show) ([l..] :: [Int])
  where l = length ns + 1

-- | Convert a 'QGen' to a Haskell range.
toHRange :: QGen -> [Int]
toHRange (QGen l s u) = [l, (l + s) .. u]