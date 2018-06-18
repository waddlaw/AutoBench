
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
-- 

module AutoBench.Internal.Configuration 
  (

  -- * User inputs
    minimumTestInputs            -- The minimum number of distinctly sized test inputs required by test suites.
  -- * Benchmarking
  , defaultBenchmarkFilepath     -- The default filepath for saving benchmarking data produced by Criterion.
  -- * Statistical analysis 
  , maximumCVIterations          -- The maximum number of cross-validation iterations.
  , maximumCVTrainingData        -- The maximum percentage of data to use for training regression models during cross-validation.
  , maximumModelPredictors       -- The maximum number of predictors for regression models.
  , minimumCVIterations          -- The minimum number of cross-validation iterations.
  , minimumCVTrainingData        -- The minimum percentage of data to use for training regression models during cross-validation.

  ) where 


-- * User inputs 

-- | The minimum number of distinctly sized test inputs required by test suites.
--
-- > minimumTestInputs = 20
minimumTestInputs :: Int
minimumTestInputs  = 20

-- * Benchmarking

-- | The default filepath for saving benchmarking data produced by Criterion. 
-- Note: the report produced by Criterion is in a JSON format.
--
-- > defaultBenchmarkFilepath  = "./autobench_tmp.json"
defaultBenchmarkFilepath :: FilePath
defaultBenchmarkFilepath  = "./autobench_tmp.json"

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