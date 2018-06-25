
{-# OPTIONS_GHC -Wall #-}

-- |
--
-- Module      : AutoBench.Internal.Benchmarking
-- Description : Generating and running Criterion 'Benchmarks'
-- Copyright   : (c) 2018 Martin Handley
-- License     : BSD-style
-- Maintainer  : martin.handley@nottingham.ac.uk
-- Stability   : Experimental
-- Portability : GHC
--
-- This module is responsible for generating benchmarks for test programs
-- to be executed by Criterion. In some cases (i.e., when users require the
-- system to automatically generate test data), this processes involves 
-- generating appropriate benchmark inputs using 
-- 'AutoBench.Internal.DataGeneration'.

-- Also included is AutoBench's top-level function for running benchmarks,
-- see 'runBenchmarks'. Note this function passes a user-specified 
-- configuration to Criterion, see '_critCfg' in 'TestSuite'.

-- Due to 'Arbitrary'\/'NFData' type constraints, one generation function is 
-- required for each possible configuration of:

-- * Generated or user-specified test data ('Gen' or 'Man' suffix);
-- * Results of test programs evaluated to normal form or weak head normal form
--   ('Nf' or 'Whnf' suffix);
-- * Unary or binary test programs ('Un' or 'Bin' suffix).

-- Therefore eight generation functions are required in total. 
--
-- For example, 'genBenchmarksGenNfUn' means:
-- "Generate benchmarks for /Un/ary test programs, that require /Gen/erated test 
-- data whereby test cases (i.e., the results of test programs) are evaluated
-- to /N/ormal /f/orm".
--

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------

module AutoBench.Internal.Benchmarking 
  (

  -- * Generating Criterion benchmarks
  -- ** Automatically generated test data 
    genBenchmarksGenNfUn      -- Cfg: generated test data, results to nf, unary test programs.
  , genBenchmarksGenWhnfUn    -- Cfg: generated test data, results to whnf, unary test programs.
  , genBenchmarksGenNfBin     -- Cfg: generated test data, results to nf, binary test programs. 
  , genBenchmarksGenWhnfBin   -- Cfg: generated test data, results to whnf, binary test programs.
  -- ** User-specified test data 
  , genBenchmarksManNfUn      -- Cfg: user-specified test data, results to nf, unary test programs.
  , genBenchmarksManWhnfUn    -- Cfg: user-specified test data, results to whnf, unary test programs.
  , genBenchmarksManNfBin     -- Cfg: user-specified test data, results to nf, binary test programs.
  , genBenchmarksManWhnfBin   -- Cfg: user-specified test data, results to whnf, binary test programs.
  -- * Running Criterion benchmarks
  , runBenchmarks             -- AutoBench's top-level function for running benchmarks with Criterion.

  ) where 

import Control.DeepSeq (NFData)
import Criterion.Main  (defaultMainWith)
import Data.List       (sortBy)
import Data.Maybe      (fromMaybe)
import Data.Ord        (comparing)
import Test.QuickCheck (Arbitrary)
import Criterion.Types 
  ( Benchmark
  , bench
  , bgroup
  , env
  , jsonFile
  , nf
  , reportFile
  , whnf
  )

import           AutoBench.Internal.AbstractSyntax (Id)
import qualified AutoBench.Internal.Configuration  as Config
import           AutoBench.Internal.DataGeneration (genDataBin, genDataUn)
import           AutoBench.Internal.Types          ( BinaryTestData
                                                   , TestSuite(..)
                                                   , UnaryTestData, toHRange )

-- * Generate Criterion benchmarks

-- ** Automatically generated test data 

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Generated test data;
-- * Evaluate the results of test programs to normal form;
-- * Unary test programs.
genBenchmarksGenNfUn 
  :: (Arbitrary a, NFData a, NFData b) 
  => [(Id, a -> b)]  -- [(idt, test program)]
  -> TestSuite
  -> [Benchmark]
genBenchmarksGenNfUn ps ts = gen ps <$> 
  zip (toHRange $ _dataOpts ts) (genDataUn $ _dataOpts ts)
  where 
    -- Whether to include baseline measurements.
    baseline = _baseline ts 

    -- Generate the benchmarks.
    gen progs (size, d) 
      | baseline = bgroup Config.withBaselineBenchmarkGroupIdentifier
          [ env d 
             ( \xs -> 
                 bgroup (Config.inputSizesBenchmarkGroupIdentifier [size])
                   [ bench idt $ nf prog xs
                   | (idt, prog) <- progs
                   ]
             )
          , env (snd (head progs) <$> d) 
              (bench (Config.baselineBenchmarkIdentifier [size]) . nf id)
          ]
      | otherwise = env d 
          ( \xs -> 
              bgroup (Config.inputSizesBenchmarkGroupIdentifier [size])
                [ bench idt $ nf prog xs
                | (idt, prog) <- progs
                ]
          )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Generated test data;
-- * Evaluate the results of test programs to weak head normal form;
-- * Unary test programs.
genBenchmarksGenWhnfUn 
  :: (Arbitrary a, NFData a) 
  => [(Id, a -> b)]  -- [(idt, test program)]
  -> TestSuite
  -> [Benchmark]
genBenchmarksGenWhnfUn ps ts = gen ps <$> 
  zip (toHRange $ _dataOpts ts) (genDataUn $ _dataOpts ts)
  where 
    -- Generate the benchmarks.
    gen progs (size, d) = env d 
      ( \xs -> 
          bgroup (Config.inputSizesBenchmarkGroupIdentifier [size])
            [ bench idt $ whnf prog xs
            | (idt, prog) <- progs
            ]
      )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Generated test data;
-- * Evaluate the results of test programs to normal form;
-- * Binary test programs.
genBenchmarksGenNfBin 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b, NFData c)
  => [(Id, a -> b -> c)]  -- [(idt, test program)]
  -> TestSuite
  -> [Benchmark]
genBenchmarksGenNfBin ps ts = fmap (gen ps) testData
  where 
    -- Whether to include baseline measurements.
    baseline = _baseline ts 

    -- Generate the benchmarks.
    gen progs ((s1, s2), d) 
      | baseline = bgroup Config.withBaselineBenchmarkGroupIdentifier
          [ env d 
              ( \xs -> bgroup (Config.inputSizesBenchmarkGroupIdentifier [s1, s2])
                  [ bench idt $ nf (uncurry prog) xs
                  | (idt, prog) <- progs 
                  ]
              )
          , env ((uncurry $ snd $ head progs) <$> d) 
              (bench (Config.baselineBenchmarkIdentifier [s1, s2]) . nf id)
          ] 

      | otherwise = env d 
          ( \xs -> bgroup (Config.inputSizesBenchmarkGroupIdentifier [s1, s2])
              [ bench idt $ nf (uncurry prog) xs
              | (idt, prog) <- progs 
              ]
          )

    testData = zip ((,) <$> size <*> size) (genDataBin $ _dataOpts ts)
    size     = toHRange (_dataOpts ts)

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Generated test data;
-- * Evaluate the results of test programs to weak head normal form;
-- * Binary test programs.
genBenchmarksGenWhnfBin 
  :: (Arbitrary a, Arbitrary b, NFData a, NFData b)
  => [(Id, a -> b -> c)] -- [(idt, test program)]
  -> TestSuite
  -> [Benchmark]
genBenchmarksGenWhnfBin ps ts = fmap (gen ps) testData
  where
    -- Generate the benchmarks. 
    gen progs ((s1, s2), d) = env d  
      ( \xs -> bgroup (Config.inputSizesBenchmarkGroupIdentifier [s1, s2])
          [ bench idt $ whnf (uncurry prog) xs
          | (idt, prog) <- progs 
          ]
      )

    testData = zip ((,) <$> size <*> size) (genDataBin $ _dataOpts ts)
    size     = toHRange (_dataOpts ts)

-- ** User-specified test data

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Manually specified test data;
-- * Evaluate the results of test programs to normal form;
-- * Unary test programs.
--
-- Note: the user-specified test data is sorted by size.
genBenchmarksManNfUn 
  :: (NFData a, NFData b) 
  => [(Id, a -> b)]  -- [(idt, test program)]
  -> TestSuite
  -> UnaryTestData a
  -> [Benchmark]
genBenchmarksManNfUn ps ts = fmap (gen ps) . sortBy (comparing fst)
  where 
    -- Whether to include baseline measurements.
    baseline = _baseline ts 

    -- Generate the benchmarks.
    gen progs (size, d) 
      | baseline = bgroup Config.withBaselineBenchmarkGroupIdentifier
          [ env d 
             ( \xs -> 
                 bgroup (Config.inputSizesBenchmarkGroupIdentifier [size])
                   [ bench idt $ nf prog xs
                   | (idt, prog) <- progs
                   ]
             )
          , env (snd (head progs) <$> d) 
              (bench (Config.baselineBenchmarkIdentifier [size]) . nf id)
          ]
      | otherwise = env d 
          ( \xs -> 
              bgroup (Config.inputSizesBenchmarkGroupIdentifier [size])
                [ bench idt $ nf prog xs
                | (idt, prog) <- progs
                ]
          )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Manually specified test data;
-- * Evaluate the results of test programs to weak head normal form;
-- * Unary test programs.
--
-- Note: the user-specified test data is sorted by size.
genBenchmarksManWhnfUn 
  :: NFData a 
  => [(Id, a -> b)]  -- [(idt, test program)]
  -> TestSuite
  -> UnaryTestData a
  -> [Benchmark]
genBenchmarksManWhnfUn ps _ = fmap (gen ps) . sortBy (comparing fst)
  where
    -- Generate the benchmarks.
    gen progs (size, d) = env d 
      ( \xs -> 
          bgroup (Config.inputSizesBenchmarkGroupIdentifier [size])
            [ bench idt $ whnf prog xs
            | (idt, prog) <- progs
            ]
      )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Manually specified test data;
-- * Evaluate the results of test programs to normal form;
-- * Binary test programs.
--
-- Note: the user-specified test data is sorted by size.
genBenchmarksManNfBin 
  :: (NFData a, NFData b, NFData c) 
  => [(Id, a -> b -> c)]  -- [(idt, test program)]
  -> TestSuite
  -> BinaryTestData a b  
  -> [Benchmark]
genBenchmarksManNfBin ps ts = 
  fmap (gen ps) . sortBy (comparing (\(s1, s2, _, _) -> (s1, s2)))
  where 
    -- Whether to include baseline measurements.
    baseline = _baseline ts 

    -- Generate the benchmarks.
    gen progs (s1, s2, d1, d2) 
      | baseline = bgroup Config.withBaselineBenchmarkGroupIdentifier
          [ env ((,) <$> d1 <*> d2)
              ( \xs -> bgroup (Config.inputSizesBenchmarkGroupIdentifier [s1, s2])
                  [ bench idt $ nf (uncurry prog) xs
                  | (idt, prog) <- progs 
                  ]
              )
          , env ((snd $ head progs) <$> d1 <*> d2) 
              (bench (Config.baselineBenchmarkIdentifier [s1, s2]) . nf id)
          ] 

      | otherwise = env ((,) <$> d1 <*> d2) 
          ( \xs -> bgroup (Config.inputSizesBenchmarkGroupIdentifier [s1, s2])
              [ bench idt $ nf (uncurry prog) xs
              | (idt, prog) <- progs 
              ]
          )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Manually specified test data;
-- * Evaluate the results of test programs to weak head normal form;
-- * Binary test programs.
--
-- Note: the user-specified test data is sorted by size.
genBenchmarksManWhnfBin 
  :: (NFData a, NFData b) 
  => [(Id, a -> b -> c)] -- [(idt, test program)]  
  -> TestSuite
  -> BinaryTestData a b  
  -> [Benchmark]
genBenchmarksManWhnfBin ps _ = 
  fmap (gen ps) . sortBy (comparing (\(s1, s2, _, _) -> (s1, s2)))
  where 
    -- Generate the benchmarks.
    gen progs (s1, s2, d1, d2) = env ((,) <$> d1 <*> d2) 
      ( \xs -> bgroup (Config.inputSizesBenchmarkGroupIdentifier [s1, s2])
          [ bench idt $ whnf (uncurry prog) xs
          | (idt, prog) <- progs 
          ]
      )

-- * Running Criterion benchmarks.

-- | Run one or more Criterion benchmarks sequentially.
--
-- Note: Criterion /must/ output a JSON report file for benchmark results so 
-- runtimes can be analysed by AutoBench. If the user-specified Criterion 
-- configuration doesn't include a JSON file (the default configuration 
-- doesn't), AutoBench uses its own default: 
-- 'Config.defaultBenchmarkReportFilepath'.
runBenchmarks :: [Benchmark] -> TestSuite -> IO () 
runBenchmarks bs ts = defaultMainWith cfg { jsonFile = Just repFile } bs
  where 
    cfg     = _critCfg ts
    repFile = fromMaybe Config.defaultBenchmarkReportFilepath (reportFile cfg)