
{-# OPTIONS_GHC -Wall #-}


{-|

  Module      : AutoBench.Internal.Benchmarking
  Description : <TO-DO>
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  <TO-DO>

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - Comment all;
   - 
-}

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


  -- * Running benchmarks
  , runBenchmarks               -- Run benchmarks with Criterion.
  ) where 

import Control.DeepSeq (NFData)
import Criterion.Main  (defaultMainWith)
import Data.Maybe      (fromMaybe)
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

import AutoBench.AbstractSyntax          (Id)
import AutoBench.Internal.DataGeneration (genDataUn, genDataBin)
import AutoBench.Types
  ( UnaryTestData
  , BinaryTestData
  , TestSuite(..)
  , defBenchRepFilename
  , toHRange
  )


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
      | baseline = bgroup ("With Baseline")
          [ env d 
             ( \xs -> 
                 bgroup ("Input Size: " ++ show (size :: Int))
                   [ bench idt $ nf prog xs
                   | (idt, prog) <- progs
                   ]
             )
          , env (snd (head progs) <$> d) 
              (bench ("Baseline for Input Size " ++ show (size :: Int)) . nf id)
          ]
      | otherwise = env d 
          ( \xs -> 
              bgroup ("Input Size: " ++ show (size :: Int))
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
          bgroup ("Input Size: " ++ show (size :: Int))
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
      | baseline = bgroup ("With Baseline")
          [ env d 
              ( \xs -> bgroup ("Input Sizes: (" ++ show (s1 :: Int)
                                                ++ ", " 
                                                ++ show (s2 :: Int)
                                                ++ ")")
                  [ bench idt $ nf (uncurry prog) xs
                  | (idt, prog) <- progs 
                  ]
              )
          , env ((uncurry $ snd $ head progs) <$> d) 
              ( bench ("Baseline for Input Size " ++ show (s1 :: Int)
                                                 ++ ", " 
                                                 ++ show (s2 :: Int)
                                                 ++ ")"
                      ) . nf id
              )
          ] 

      | otherwise = env d 
          ( \xs -> bgroup ("Input Sizes: (" ++ show (s1 :: Int)
                                            ++ ", " 
                                            ++ show (s2 :: Int)
                                            ++ ")")
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
      ( \xs -> bgroup ("Input Sizes: (" ++ show (s1 :: Int)
                                        ++ ", " 
                                        ++ show (s2 :: Int)
                                        ++ ")")
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
genBenchmarksManNfUn 
  :: (NFData a, NFData b) 
  => [(Id, a -> b)]  -- [(idt, test program)]
  -> TestSuite
  -> UnaryTestData a
  -> [Benchmark]
genBenchmarksManNfUn ps ts = fmap (gen ps)
  where 
    -- Whether to include baseline measurements.
    baseline = _baseline ts 

    -- Generate the benchmarks.
    gen progs (size, d) 
      | baseline = bgroup ("With Baseline")
          [ env d 
             ( \xs -> 
                 bgroup ("Input Size: " ++ show (size :: Int))
                   [ bench idt $ nf prog xs
                   | (idt, prog) <- progs
                   ]
             )
          , env (snd (head progs) <$> d) 
              (bench ("Baseline for Input Size " ++ show (size :: Int)) . nf id)
          ]
      | otherwise = env d 
          ( \xs -> 
              bgroup ("Input Size: " ++ show (size :: Int))
                [ bench idt $ nf prog xs
                | (idt, prog) <- progs
                ]
          )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Manually specified test data;
-- * Evaluate the results of test programs to weak head normal form;
-- * Unary test programs.
genBenchmarksManWhnfUn 
  :: NFData a 
  => [(Id, a -> b)]  -- [(idt, test program)]
  -> TestSuite
  -> UnaryTestData a
  -> [Benchmark]
genBenchmarksManWhnfUn ps _ = fmap (gen ps)
  where
    -- Generate the benchmarks.
    gen progs (size, d) = env d 
      ( \xs -> 
          bgroup ("Input Size: " ++ show (size :: Int))
            [ bench idt $ whnf prog xs
            | (idt, prog) <- progs
            ]
      )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Manually specified test data;
-- * Evaluate the results of test programs to normal form;
-- * Binary test programs.
genBenchmarksManNfBin 
  :: (NFData a, NFData b, NFData c) 
  => [(Id, a -> b -> c)]  -- [(idt, test program)]
  -> TestSuite
  -> BinaryTestData a b  
  -> [Benchmark]
genBenchmarksManNfBin ps ts = fmap (gen ps)
  where 
    -- Whether to include baseline measurements.
    baseline = _baseline ts 

    -- Generate the benchmarks.
    gen progs (s1, s2, d1, d2) 
      | baseline = bgroup ("With Baseline")
          [ env ((,) <$> d1 <*> d2)
              ( \xs -> bgroup ("Input Sizes: (" ++ show (s1 :: Int)
                                                ++ ", " 
                                                ++ show (s2 :: Int)
                                                ++ ")")
                  [ bench idt $ nf (uncurry prog) xs
                  | (idt, prog) <- progs 
                  ]
              )
          , env ((snd $ head progs) <$> d1 <*> d2) 
              ( bench ("Baseline for Input Size " ++ show (s1 :: Int)
                                                  ++ ", " 
                                                  ++ show (s2 :: Int)
                                                  ++ ")"
                      ) . nf id
              )
          ] 

      | otherwise = env ((,) <$> d1 <*> d2) 
          ( \xs -> bgroup ("Input Sizes: (" ++ show (s1 :: Int)
                                            ++ ", " 
                                            ++ show (s2 :: Int)
                                            ++ ")")
              [ bench idt $ nf (uncurry prog) xs
              | (idt, prog) <- progs 
              ]
          )

-- | Generate benchmarks for a test suite with the following configuration:
--
-- * Manually specified test data;
-- * Evaluate the results of test programs to weak head normal form;
-- * Binary test programs.
genBenchmarksManWhnfBin 
  :: (NFData a, NFData b) 
  => [(Id, a -> b -> c)] -- [(idt, test program)]  
  -> TestSuite
  -> BinaryTestData a b  
  -> [Benchmark]
genBenchmarksManWhnfBin ps _ = fmap (gen ps)
  where 
    -- Generate the benchmarks.
    gen progs (s1, s2, d1, d2) = env ((,) <$> d1 <*> d2) 
      ( \xs -> bgroup ("Input Sizes: (" ++ show (s1 :: Int)
                                        ++ ", " 
                                        ++ show (s2 :: Int)
                                        ++ ")")
          [ bench idt $ whnf (uncurry prog) xs
          | (idt, prog) <- progs 
          ]
      )




runBenchmarks :: [Benchmark] -> TestSuite -> IO () 
runBenchmarks bs ts = defaultMainWith cfg { jsonFile = Just repFile } bs
  where 
    cfg     = _critCfg ts
    repFile = fromMaybe defBenchRepFilename (reportFile cfg)