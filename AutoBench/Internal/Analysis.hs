

{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall      #-} 

{-|

  Module      : AutoBench.Internal.Analysis
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
   - Move validateTestReport to user input checks;
   - Better comments all around.
   - 
-}

module AutoBench.Internal.Analysis where 

import           Control.Arrow         ((&&&))
import           Criterion.Types       (OutlierEffect(..))
import           Data.Default          (def)
import           Data.List             (genericLength, sort, sortBy)
import           Data.Maybe            (catMaybes, fromMaybe)
import qualified Data.Vector.Storable  as V
import           Numeric.LinearAlgebra (Vector, norm_1, norm_2)
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Random         (randomRIO)

import AutoBench.Internal.AbstractSyntax  (Id)    
import AutoBench.Internal.UserIO          ( outputAnalysisReport
                                          , outputQuickAnalysis )
import AutoBench.Internal.Regression      ( generateLinearCandidate
                                          , fitRidgeRegress )
import AutoBench.Internal.UserInputChecks ( validateAnalOpts
                                          , validateTestReport )
import AutoBench.Internal.Utils           (notNull, uniqPairs)
import AutoBench.Internal.Types  
  ( AnalOpts(..)
  , AnalysisReport(..)
  , BenchReport(..)
  , Coord
  , Coord3
  , CVStats(..)
  , Improvement
  , LinearCandidate(..)
  , LinearFit(..)
  , SimpleReport(..)
  , SimpleResults(..)
  , Stats(..)
  , TestReport(..)
  , QuickAnalysis(..)
  , QuickReport(..)
  , QuickResults(..)
  , maxPredictors
  , numPredictors
  , simpleReportsToCoords
  )   





-- * QuickBench Top-level 

quickAnalyseWith :: AnalOpts -> Bool -> [QuickReport] -> IO ()                                        -- <TO-DO> ** COMMENT **
quickAnalyseWith aOpts eql qrs -- Don't check 'QuickReport's because users can't manipulate them.
  | notNull aOptsErrs = do  
      putStrLn "Cannot analyse results due to one or more errors:"
      mapM_ print aOptsErrs
  | otherwise = outputQuickAnalysis aOpts eql $ quickAnalysis aOpts qrs
  where aOptsErrs = validateAnalOpts aOpts -- Validate the 'AnalOpts'.
    

-- * AutoBench Top-level 

analyse :: TestReport -> IO ()                                                                        -- <TO-DO> ** COMMENT **
analyse  = analyseWith def

analyseWith :: AnalOpts -> TestReport -> IO ()                                                        -- <TO-DO> ** COMMENT **
analyseWith aOpts tr 
  | notNull (aOptsErrs ++ trErrs) = do  
      putStrLn "Cannot analyse results due to one or more errors:"
      mapM_ print (aOptsErrs ++ trErrs)   
  -- Just output the results of statistical analysis.
  | otherwise = outputAnalysisReport aOpts tr $ calculateAnalysisReport aOpts tr

  where 
    -- Validate the 'AnalOpts'.
    aOptsErrs = validateAnalOpts aOpts
    -- Validate the 'TestReport'
    trErrs    = validateTestReport tr

-- * QuickBench Analysis

quickAnalysis :: AnalOpts -> [QuickReport] -> QuickAnalysis
quickAnalysis aOpts qrs = 
  QuickAnalysis
    {
      _qAnlys = quickResults aOpts qrs                               -- A set of quick results ('QuickResults') for each test program.
    , _qImps  = catMaybes $ fmap (uncurry $ calculateImprovements    -- A set of improvements.
        (_improv aOpts)) (uniqPairs $ zip names coords)
    }
  where 
    -- Names of test programs 
    names = fmap _qName qrs
    -- (Input size(s), runtime) coordinates. 
    coords = fmap _qRuntimes qrs

-- | Perform statistical analysis on the benchmarking results (i.e., runtime 
-- measurements of test programs) in the given @[QuickReport]@ and produce an
-- analysis report ('QuickResults') /for each test program/.
quickResults :: AnalOpts -> [QuickReport] -> [QuickResults]
quickResults aOpts = fmap quickResult
  where 
    quickResult :: QuickReport -> QuickResults 
    quickResult qr = 
      QuickResults 
        {
          _qrIdt  = _qName qr
        , _qrRaws = coords
        , _qrFits = fitCoords aOpts coords
        }
      where coords = _qRuntimes qr

-- * AutoBench Analysis 

-- | Perform statistical analysis on the benchmarking results (i.e., runtime 
-- measurements of test programs) in the given 'TestReport' and produce an 
-- 'AnalysisReport' to summarise the overall analysis phase.
calculateAnalysisReport :: AnalOpts -> TestReport -> AnalysisReport                                                                      
calculateAnalysisReport aOpts tr = 
  AnalysisReport
    {
      _anlys = anlys                                               -- A set of simple results ('SimpleResults') for each test program.
    , _imps  = catMaybes $ fmap (uncurry $ calculateImprovements   -- A set of improvements.
        (_improv aOpts)) (uniqPairs $ zip srNames srCoords)  
    , _blAn  = blAn                                                -- Analysis of baseline measurements, if applicable.
    }
  where
    -- 'SimpleResults' for test programs and baseline measurements, if applicable.
    (anlys, blAn) =  calculateSimpleResults aOpts tr    
    -- SimpleReports.
    srs = _reports (_br tr)
    -- Names of test programs from 'SimpleReport's.
    srNames  = fmap (_name . head) srs           
    -- (Input size(s), runtime) coordinates.         
    srCoords = zipWith simpleReportsToCoords srNames srs

-- | Perform statistical analysis on the benchmarking results (i.e., runtime 
-- measurements of test programs) in the given 'TestReport' and produce an
-- analysis report ('SimpleResults') /for each test program/.
calculateSimpleResults 
  :: AnalOpts 
  -> TestReport 
  -> ([SimpleResults], Maybe SimpleResults)                           
calculateSimpleResults aOpts tr = (progResults, blResults)
  
  where
    -- Analysis for test programs.
    progResults = zipWith calculateSimpleResult (_tProgs tr) (_reports $ _br tr)
    -- Analysis for baseline measurements.
    blResults = case _baselines $ _br tr of 
      []  -> Nothing 
      bls -> Just $ calculateSimpleResult "Baseline Measurements" bls                                -- This shouldn't be a string.

    -- For a set of 'SimpleReport's (i.e., test cases) relating to a 
    -- given test program, perform statistical analysis and generate
    -- 'SimpleResults'. 
    calculateSimpleResult 
      :: Id               -- Name of test program.
      -> [SimpleReport]   -- Benchmarking measurements for each test case.
      -> SimpleResults
    calculateSimpleResult idt srs = 
      SimpleResults
        {
          _srIdt           = idt 
        , _srRaws          = coords
        , _srStdDev        = stdDev
        , _srAvgOutVarEff  = avgOutVarEff
        , _srAvgPutVarFrac = avgOutVarFrac
        , _srFits          = fitCoords aOpts coords
        }
      where 
        coords = simpleReportsToCoords idt srs
        (stdDev, avgOutVarFrac, avgOutVarEff) = 
          fromMaybe (0, 0, Unaffected) (simpleReportSummary srs)                              -- <TO-DO> error handling: fromMaybe crap_if_error ...

    -- Provide an overall summary of the simple statistics taken from 
    -- Criterion's 'Report's relating to the /same/ test program:
    -- 
    -- * Standard deviation of all test cases;
    -- * Average effect of outliers on variance (percentage);
    -- * Average effect of outliers on variance ('OutlierEffect').
    simpleReportSummary                                                                      
      :: [SimpleReport] 
      -> Maybe (Double, Double, OutlierEffect)
    simpleReportSummary []  = Nothing
    simpleReportSummary srs = Just (stdDev srs, avgOutVarFrac, avgOutVarEff)
      where 
        -- Standard deviation for all test cases: sqrt (SUM variance_i/samples_i).
        stdDev  = sqrt . sum . fmap (uncurry (/) . ((** 2) . 
          _stdDev &&& fromIntegral . _samples)) 
        
        -- Average effect of outliers on variance.
        avgOutVarFrac = sum (fmap _outVarFrac srs) / genericLength srs
        avgOutVarEff 
          | avgOutVarFrac < 0.01 = Unaffected
          | avgOutVarFrac < 0.1  = Slight
          | avgOutVarFrac < 0.5  = Moderate
          | otherwise            = Severe

-- * Linear fitting

-- | Fit a 'LinearCandidate' model to a data set, generating a 'LinearFit'
-- that can be used to asses the model's goodness of fit.
candidateFit 
  :: ([Coord] -> LinearCandidate -> Vector Double)  -- Fitting function.
  -> Double                                         -- Cross-validation train/validate data split.
  -> Int                                            -- Cross-validation iterations.
  -> [Coord]                                        -- Data set.
  -> LinearCandidate                                -- Model to fit.
  -> Maybe LinearFit 
candidateFit ff split iters coords c
  | length coords < maxPredictors + 1 = Nothing     -- Need a minimum of (maxPredictors + 1) coordinates.
  | otherwise = Just
  LinearFit 
    {
      _lft  = _lct   c 
    , _cfs  = coeffs
    , _ex   = _fex   c coeffs
    , _yhat = _fyhat c coeffs
    , _sts  = sts
    }
  where 
    -- Model coefficients when fit to /entire/ data set.
    coeffs = ff coords c
    -- Statistics from cross-validation.
    cvSts  = cvCandidateFit ff split iters coords c
    -- Combine statistics from each cross-validation iteration and also compute
    -- other goodness of fit measures.
    sts    = stats c coords coeffs cvSts

-- ** Linear fitting cross-validation

-- | Fit a 'LinearCandidate' to a data set using Monte Carlo cross-validation.
-- Return the fitting statistics ('CVStats') from each iteration of 
-- cross-validation.
cvCandidateFit 
  :: ([Coord] -> LinearCandidate -> Vector Double)  -- Fitting function.
  -> Double                                         -- Train/evaluate data split.
  -> Int                                            -- Number of iterations.
  -> [Coord]                                        -- Full data set.
  -> LinearCandidate                                -- Model to fit.
  -> [CVStats]
cvCandidateFit ff split iters coords c = 
  fmap (uncurry trainAndValidate) splits 
  where 
    splits = fmap (splitRand split) (replicate iters coords)

    trainAndValidate :: [Coord] -> [Coord] -> CVStats
    trainAndValidate train valid = cvStats c valid (ff train c)

-- ** Linear fitting statistics

-- | Fitting statistics generated from a /single/ iteration of cross-validation.
-- See 'stats' for cumulative fitting statistics.
cvStats 
  :: LinearCandidate    -- The model.
  -> [Coord]            -- The /evaluation/ set of this iteration of cross-validation.
  -> Vector Double      -- The coefficients of the model calculated using the /training/ set.
  -> CVStats            -- The model's fitting statistics for this iteration of cross-validation.
cvStats c coords coeffs = 
  CVStats
   {
     _cv_mse    = mse 
   , _cv_mae    = mae
   , _cv_ss_tot = ss_tot
   , _cv_ss_res = ss_res
   }
  where 
    ---------------------------------------------------------------------------
    (xs, ys) = unzip coords 
    _X       = V.fromList xs                                                                         -- X. ** don't transform using fxs **
    _Y       = V.fromList ys                                                                         -- Y.
    n        = length coords        
    n'       = fromIntegral n                                                                        -- n data points.
    ---------------------------------------------------------------------------
    yhat    = _fyhat c coeffs _X                                                                     -- Model y-coords.
    ybar    = V.sum _Y / n'                                                                          -- Average of data y-coords.
    ybars   = V.replicate n ybar    
    ---------------------------------------------------------------------------
    mse     = ss_res / n'                                                                            -- Mean squared error       ss_res / n
    mae     = (norm_1 $ V.zipWith (-) _Y  yhat) / n'                                                 -- Mean absolute error:     (SUM |y_i - f(y_i)|) / n             
    ss_tot  = (norm_2 $ V.zipWith (-) _Y  ybars) ** 2.0                                              -- Total sum of squares:    SUM (y_i - avg(y))^2
    ss_res  = (norm_2 $ V.zipWith (-) _Y  yhat)  ** 2.0                                              -- Residual sum of squares: SUM (y_i - f(y_i))^2


-- | Overall fitting statics for a model and a given data set. Some statistics 
-- are cumulative fitting stats, generated by cross-validation (PMSE, PMAE, 
-- PRESS, SST, Pred. R^2); others are generated by using the whole data set as 
-- training data (R^2, Adj. R^2, AIC, BIC, Mallow's CP).
stats 
  :: LinearCandidate  -- The model.
  -> [Coord]          -- The data set
  -> Vector Double    -- The coefficients of the model calculated using the /entire/ set.
  -> [CVStats]        -- The statistics from each iteration of cross-validation.
  -> Stats            -- The cumulative fitting statistics.
stats c coords coeffs cvSts = _sts 
  where
    -- Statistics calculated below.
    _sts = Stats 
            {
              _p_mse    = p_mse
            , _p_mae    = p_mae
            , _ss_tot   = cv_ss_tot
            , _p_ss_res = p_ss_res
            , _r2       = r2
            , _a_r2     = a_r2
            , _p_r2     = p_r2
            , _bic      = bic
            , _aic      = aic
            , _cp       = cp
            }
    --------------------------------------------------------------------------- 
    -- Stats for /complete/ data set:
    --------------------------------------------------------------------------- 
    (xs, ys) = unzip coords 
    _X       = V.fromList xs                                                                         -- X. ** do not transform using fxs **
    _Y       = V.fromList ys                                                                         -- Y.
    n        = length coords        
    n'       = fromIntegral n                                                                        -- n data points.
    k'       = fromIntegral (numPredictors $ _lct c)                                                 -- k predictors.
    ---------------------------------------------------------------------------
    yhat     = _fyhat c coeffs _X                                                                    -- Model y-coords.
    ybar     = V.sum _Y / n'                                                                         -- Average of data y-coords.
    ybars    = V.replicate n ybar    
    ---------------------------------------------------------------------------           
    _mse     = _ss_res / n'                                                                          -- Mean squared error:                       ss_res / n
    _ss_tot  = (norm_2 $ V.zipWith (-) _Y ybars) ** 2.0                                              -- Total sum of squares:                     SUM (y_i - avg(y))^2
    _ss_res  = (norm_2 $ V.zipWith (-) _Y yhat)  ** 2.0                                              -- Residual sum of squares:                  SUM (y_i - f(y_i))^2
    r2       = 1.0 - (_ss_res / _ss_tot)                                                             -- Coefficient of Determination:             1 - ss_res/ss_tot
    a_r2     = 1.0 - ((1.0 - r2) * (n' - 1.0) / (n' - k' - 1.0))                                     -- Adjusted Coefficient of Determination:    1 - ((1 - r^2)(n - 1)/(n - k - 1))
    ---------------------------------------------------------------------------
    -- http://www.stat.wisc.edu/courses/st333-larget/aic.pdf (implementations from R).
    ---------------------------------------------------------------------------
    aic = (n' * log (_ss_res / n')) + (2 * k')                                                       -- Akaike’s Information Criterion:           n * ln(ss_res/n) + 2k
    bic = (n' * log (_ss_res / n')) + (k' * log n')                                                  -- Bayesian Information Criterion:           n * ln(ss_res/n) + k * ln(n)
    cp  = (_ss_res / _mse) - (n' - (2 * k'))                                                         -- Mallows' Cp Statistic:                    (ss_res/_mse) - (n - 2k)
    --------------------------------------------------------------------------- 
    -- Cross-validated stats:
    ---------------------------------------------------------------------------   
    cv_n      = genericLength cvSts
    p_mse     = (sum $ fmap _cv_mse cvSts)    / cv_n                                                 -- 1/n * SUM mse_k         for each iteration k
    p_mae     = (sum $ fmap _cv_mae cvSts)    / cv_n                                                 -- 1/n * SUM mae_k         for each iteration k
    cv_ss_tot = (sum $ fmap _cv_ss_tot cvSts) / cv_n                                                 -- 1/n * SUM ss_tot_k      for each iteration k
    p_ss_res  = (sum $ fmap _cv_ss_res cvSts) / cv_n                                                 -- 1/n * SUM ss_res_k      for each iteration k
    p_r2     = 1.0 - (p_ss_res / cv_ss_tot)                                                          -- 1 - p_ss_res/cv_ss_tot

-- * Helpers 

-- | Split a list into two random sublists of specified sizes.
-- Warning: uses 'unsafePerformIO' for randomness.
splitRand :: Double -> [a] -> ([a], [a])
splitRand split xs = (take len shuff, drop len shuff) 
  where 
    len   = ceiling (fromIntegral (length xs) * split)
    -- Eeeeeeft.
    shuff = unsafePerformIO (shuffle xs)

-- | Naive way to shuffle a list.
shuffle :: [a] -> IO [a]                                                                   -- Could shuffle in the top-level functions to avoid using unsafePerformIO?
shuffle x = 
  if length x < 2 
  then return x 
  else do
   i <- randomRIO (0, length x - 1)
   r <- shuffle (take i x ++ drop (i + 1) x)
   return (x !! i : r)

-- | Compare the runtimes of two test programs, given as a list of 
-- xy-coordinates, pointwise to generate improvement results. Only compare 
-- results relating to test programs of the same airty. (Note: this should 
-- always be the case when used in practice).
calculateImprovements
 :: ([(Double, Double)] -> Maybe (Ordering, Double)) 
 -> (Id, Either [Coord] [Coord3]) 
 -> (Id, Either [Coord] [Coord3])
 -> Maybe Improvement
calculateImprovements improv (idt1, Left cs1) (idt2, Left cs2) -- Unary against unary.
  -- If all coordinates didn't match, then can't generate improvements.
  | length toCompare < length cs1 = Nothing 
  | otherwise = case improv toCompare of 
      Nothing       -> Nothing
      Just (ord, d) -> Just (idt1, ord, idt2, d)
  where toCompare = matchCoords (sort cs1) (sort cs2)
calculateImprovements improv (idt1, Right cs1) (idt2, Right cs2) -- Binary against binary.
  -- If all coordinates didn't match, then can't generate improvements.
  | length toCompare < length cs1 = Nothing
  | otherwise = case improv toCompare of 
      Nothing       -> Nothing
      Just (ord, d) -> Just (idt1, ord, idt2, d)
  where toCompare = matchCoords3 (sort cs1) (sort cs2) 
calculateImprovements _ _ _ = Nothing -- Shouldn't happen.

-- | Make sure coordinates to compare when generating improvement results have 
-- the same size information. (Note: this should always be the case because of 
-- how they are generated, but better to check.)
matchCoords :: [Coord] -> [Coord] -> [(Double, Double)]
matchCoords [] _  = []
matchCoords _  [] = []
matchCoords ((s1, t1) : cs1) ((s1', t2) : cs2)
  | s1 == s1' = (t1, t2) : matchCoords cs1 cs2 
  | otherwise = matchCoords cs1 cs2 

-- | Make sure coordinates to compare when generating improvement results have 
-- the same size information. (Note: this should always be the case because of 
-- how they are generated, but better to check.)
matchCoords3 :: [Coord3] -> [Coord3] -> [(Double, Double)]
matchCoords3 [] _  = []
matchCoords3 _  [] = []
matchCoords3 ((s1, s2, t1) : cs1) ((s1', s2', t2) : cs2)
  | s1 == s1' && s2 == s2' = (t1, t2) : matchCoords3 cs1 cs2 
  | otherwise = matchCoords3 cs1 cs2 

fitCoords :: AnalOpts -> Either [Coord] [Coord3] -> [LinearFit]                                           -- <TO-DO>: comment
fitCoords _ Right{} = []
fitCoords aOpts (Left coords) = take (_topModels aOpts)
   $ sortBy (\lf1 lf2 -> (_statsSort aOpts) (_sts lf1) (_sts lf2))
   $ filter (\lf -> (_statsFilt aOpts) (_sts lf))
   $ catMaybes
   $ fmap ( ( candidateFit 
               fitRidgeRegress           -- Use ridge regression to fit.
               (_cvTrain aOpts)          -- Train/evaluate data split.
               (_cvIters aOpts)          -- Number of cross-validation iterations.  
               coords                    -- Data set.
            ) . generateLinearCandidate   -- 'LinearType' -> 'LinearCandidate'.
          ) (_linearModels aOpts)         -- Fit all models in 'AnalOpts'.