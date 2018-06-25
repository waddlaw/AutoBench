
{-# OPTIONS_GHC -Wall #-}

-- |
--
-- Module      : AutoBench.Internal.Configuration
-- Description : Configuration file
-- Copyright   : (c) 2018 Martin Handley
-- License     : BSD-style
-- Maintainer  : martin.handley@nottingham.ac.uk
-- Stability   : Experimental
-- Portability : GHC
--
-- This module contains system-wide settings and defaults.
--

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------

module AutoBench.Internal.Configuration 
  (

  -- * User inputs
    binaryTestDataConstructor               -- The data constructor for user-specified test data for binary test programs.
  , minimumTestInputs                       -- The minimum number of distinctly sized test inputs required by test suites.
  , testSuiteDataConstructor                -- The data constructor for test suites.
  , unaryTestDataConstructor                -- The data constructor for user-specified test data for unary test programs.

  -- * Benchmarking
  , baselineBenchmarkIdentifier             -- Generate benchmark identifiers for baseline measurements based on input size.
  , defaultBenchmarkReportFilepath          -- The default filepath for saving benchmarking data produced by Criterion.
  , inputSizesBenchmarkGroupIdentifier      -- Generate benchmark group identifiers for test cases.
  , withBaselineBenchmarkGroupIdentifier    -- The benchmark group identifier for test cases where baseline measurements are to be taken.
  -- * Statistical analysis 
  , maximumCVIterations                     -- The maximum number of cross-validation iterations.
  , maximumCVTrainingData                   -- The maximum percentage of data to use for training regression models during cross-validation.
  , maximumModelPredictors                  -- The maximum number of predictors for regression models.
  , minimumCVIterations                     -- The minimum number of cross-validation iterations.
  , minimumCVTrainingData                   -- The minimum percentage of data to use for training regression models during cross-validation.
  -- * Misc 
  , version                                 -- The system's version.

  ) where 

-- * User inputs 

-- | The minimum number of distinctly sized test inputs required by test suites.
--
-- > minimumTestInputs = 20
minimumTestInputs :: Int
minimumTestInputs  = 20

-- | The data constructor for user-specified test data for unary test programs.
--
-- > unaryTestDataConstructor  = "UnaryTestData"
unaryTestDataConstructor :: String 
unaryTestDataConstructor  = "UnaryTestData"

-- | The data constructor for user-specified test data for binary test programs.
--
-- > binaryTestDataConstructor  = "BinaryTestData"
binaryTestDataConstructor :: String 
binaryTestDataConstructor  = "BinaryTestData"

-- | The data constructor for test suites.
--
-- > testSuiteDataConstructor  = "TestSuite"
testSuiteDataConstructor :: String 
testSuiteDataConstructor  = "TestSuite"

-- * Benchmarking

-- | The default filepath for saving benchmarking data produced by Criterion. 
-- Note: the report produced by Criterion is in a JSON format.
--
-- > defaultBenchmarkReportFilepath  = "./autobench_tmp.json"
defaultBenchmarkReportFilepath :: FilePath
defaultBenchmarkReportFilepath  = "./autobench_tmp.json"

-- | The benchmark group identifier for test cases where baseline measurements 
-- are to be taken.
-- 
-- > withBaselineBenchmarkGroupIdentifier  = "With Baseline"
withBaselineBenchmarkGroupIdentifier :: String 
withBaselineBenchmarkGroupIdentifier  = "With Baseline"

-- | Generate benchmark group identifiers for test cases. Note that test cases 
-- are grouped by the size of the test inputs.
inputSizesBenchmarkGroupIdentifier :: [Int] -> String
inputSizesBenchmarkGroupIdentifier [x] = "Input Size " ++ show x
inputSizesBenchmarkGroupIdentifier [x1, x2] = 
  "Input Sizes (" ++ show x1 ++ ", " ++ show x2 ++ ")"
inputSizesBenchmarkGroupIdentifier _ = error "AutoBench.Internal.Configuration.inputSizesBenchmarkGroupIdentifier: invalid input sizes." 

-- | Generate benchmark identifiers for baseline measurements based on input 
-- size.
baselineBenchmarkIdentifier :: [Int] -> String 
baselineBenchmarkIdentifier [x] = "Baseline for Input Size " ++ show x
baselineBenchmarkIdentifier [x1, x2] = 
  "Baseline for Input Sizes (" ++ show x1 ++ ", " ++ show x2 ++ ")"
baselineBenchmarkIdentifier _ = error "AutoBench.Internal.Configuration.baselineBenchmarkIdentifier: invalid input sizes." 

-- * Statistical analysis

-- | The maximum number of predictors for regression models.
--
-- > maximumModelPredictors = 10
maximumModelPredictors :: Int
maximumModelPredictors  = 10

-- | The minimum percentage of data to use for training regression models during
-- cross-validation.
--
-- > minimumCVTrainingData = 0.5
minimumCVTrainingData :: Double
minimumCVTrainingData  = 0.5

-- | The maximum percentage of data to use for training regression models during
-- cross-validation.
--
-- > maximumCVTrainingData = 0.8
maximumCVTrainingData :: Double 
maximumCVTrainingData  = 0.8

-- | The minimum number of cross-validation iterations.
--
-- > minimumCVIterations = 100
minimumCVIterations :: Int 
minimumCVIterations  = 100

-- | The maximum number of cross-validation iterations.
--
-- > maximumCVIterations = 500
maximumCVIterations :: Int 
maximumCVIterations  = 500

-- * Misc 

-- | The system's version.
--
-- > version  = "AutoBench (Version 0.1.0.0)"
version :: String 
version  = "AutoBench (Version 0.1.0.0)"