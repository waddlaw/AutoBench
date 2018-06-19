
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.Internal.Types
  Description : Datatypes and associated helper functions\/defaults.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  Datatypes used internally throughout AutoBench's implementation and any 
  associated helper functions\/defaults.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 'DataOpts' Discover setting;
   - Make AnalOpts in TestSuite a maybe type? In case users don't want to 
     analyse right away;
   - 'UserInputs' PP doesn't wrap;
   - 'TestSuite's PP isn't alphabetical by test program name;
-}

module AutoBench.Internal.Types 
  (

  -- * Re-exports
    module AutoBench.Types
  -- * User inputs
  -- ** Test data options
  , toHRange               -- Convert @Gen l s u :: DataOpts@ to a Haskell range.
  -- ** Internal representation of user inputs
  , UserInputs(..)         -- A data structure maintained by the system to classify user inputs.
  , initUserInputs         -- Initialise a 'UserInputs' data structure.
  -- * Benchmarking
  , BenchReport(..)        -- A report to summarise the benchmarking phase of testing.
  , Coord                  -- (Input Size, Runtime) results as coordinates for unary test programs.
  , Coord3                 -- (Input Size, Input Size, Runtime) results as coordinates for binary test programs.
  , DataSize(..)           -- The size of unary and binary test data.
  , SimpleReport(..)       -- A simplified version of Criterion's 'Report'. See 'Criterion.Types.Report'.
  -- * Test results 
  , TestReport(..)         -- A report to summarise the system's testing phase.
  -- ** QuickBench
  , QuickReport(..)        -- A report to summarise the QuickBench phase of testing.
  -- * Statistical analysis 
  , AnalysisReport(..)     -- A report to summarise the system's analysis phase.                                                                                 
  , CVStats(..)            -- Fitting statistics calculated for regression models per each iteration of cross-validation.
  , Improvement            -- An efficiency improvement is an ordering between two test programs and a rating
                           -- 0 <= d <= 1 that corresponds to the percentage of test cases that support the ordering.
  , Exp                    -- Expressions with 'Double' literals.
  , LinearCandidate(..)    -- The details of a regression model necessary to fit it to a given dataset.
  , LinearFit(..)          -- A regression model's fitting statistics and helper functions: predicting y-coordinates, pretty printing.
  , SimpleResults(..)      -- Simple statistical analysis results for each test program.
  , numPredictors          -- Number of predictors for each type of model.
  , simpleReportToCoord    -- Convert a 'SimpleReport' to a (input size(s), runtime) coordinate, i.e., 'Coord' or 'Coord3'.
  , simpleReportsToCoords  -- Convert a list of 'SimpleReport' to a (input size(s), runtime) coordinate, i.e., 'Coord' or 'Coord3'.
  -- ** QuickBench
  , QuickAnalysis(..)      -- A report to summarise the system's analysis phase for QuickBenching.
  , QuickResults(..)       -- Simple statistical analysis results for each test program for QuickBenching.
  -- * Errors
  -- ** System errors
  , SystemError(..)        -- System errors.
  -- ** Input errors
  , InputError(..)         -- User input errors.

  ) where

import           Control.Exception.Base (Exception)
import           Criterion.Types        (OutlierEffect(..))
import           Data.Either            (partitionEithers)
import           Numeric.LinearAlgebra  (Vector)

import qualified AutoBench.Internal.Expr as E
import           AutoBench.Types  -- Re-export.

import AutoBench.Internal.AbstractSyntax 
  ( HsType
  , Id
  , ModuleElem(..)
  , TypeString
  )

-- * User inputs

-- ** Test suites

-- | Convert @Gen l s u :: DataOpts@ to a Haskell range.
toHRange :: DataOpts -> [Int]
toHRange Manual{}    = []
toHRange (Gen l s u) = [l, (l + s) .. u]

-- ** Internal representation of user inputs

-- | While user inputs are being analysed by the system, a 'UserInputs' data
-- structure is maintained. The purpose of this data structure is to classify 
-- user inputs according to the properties they satisfy. For example, when the 
-- system first interprets a user input file, all of its definitions are added 
-- to the '_allElems' list. This list is then processed to determine which 
-- definitions have function types that are syntactically compatible with the 
-- requirements of the system (see 'AutoBench.Internal.StaticChecks'). 
-- Definitions that are compatible are added to the '_validElems' list, and 
-- those that aren't are added to the '_invalidElems' list. Elements in the 
-- '_validElems' list are then classified according to, for example, whether 
-- they are nullary, unary, or binary functions. This check process continues
-- until all user  inputs are classified according to the list headers below. 
-- Note that both static ('AutoBench.Internal.StaticChecks') and dynamic 
-- ('AutoBench.Internal.DynamicChecks') checks are required to classify user 
-- inputs.
--
-- Notice that each /invalid/ definitions has one or more input errors 
-- associated with it.
--
-- After the system has processed all user inputs, users can review this data 
-- structure to see how the system has classified their inputs, and if any 
-- input errors have been generated. 
data UserInputs = 
  UserInputs
   {
     _allElems           :: [(ModuleElem, Maybe TypeString)]         -- ^ All definitions in a user input file.
   , _invalidElems       :: [(ModuleElem, Maybe TypeString)]         -- ^ Syntactically invalid definitions (see 'AutoBench.Internal.AbstractSyntax').
   , _validElems         :: [(Id, HsType)]                           -- ^ Syntactically valid definitions (see 'AutoBench.Internal.AbstractSyntax').
   , _nullaryFuns        :: [(Id, HsType)]                           -- ^ Nullary functions.
   , _unaryFuns          :: [(Id, HsType)]                           -- ^ Unary functions.
   , _binaryFuns         :: [(Id, HsType)]                           -- ^ Binary functions.
   , _arbFuns            :: [(Id, HsType)]                           -- ^ Unary/binary functions whose input types are members of the Arbitrary type class.
   , _benchFuns          :: [(Id, HsType)]                           -- ^ Unary/binary functions whose input types are members of the NFData type class.
   , _nfFuns             :: [(Id, HsType)]                           -- ^ Unary/binary functions whose result types are members of the NFData type class.
   , _invalidData        :: [(Id, HsType, [InputError])]             -- ^ Invalid user-specified test data. 
   , _unaryData          :: [(Id, HsType, [Int])]                    -- ^ Valid user-specified test data for unary functions /with size information/.
   , _binaryData         :: [(Id, HsType, [(Int, Int)])]             -- ^ Valid user-specified test data for binary functions /with size information/.
   , _invalidTestSuites  :: [(Id, [InputError])]                     -- ^ Invalid test suites.
   , _testSuites         :: [(Id, TestSuite)]                        -- ^ Valid test suites.
   }

-- | Initialise a 'UserInputs' data structure by specifying the '_allElems' 
-- list. 
initUserInputs :: [(ModuleElem, Maybe TypeString)] -> UserInputs
initUserInputs xs = 
  UserInputs
    {
      _allElems          = xs
    , _invalidElems      = []
    , _validElems        = []
    , _nullaryFuns       = []
    , _unaryFuns         = []
    , _binaryFuns        = []
    , _arbFuns           = []
    , _benchFuns         = []
    , _nfFuns            = []
    , _invalidData       = []
    , _unaryData         = []
    , _binaryData        = []
    , _invalidTestSuites = []
    , _testSuites        = []
    }

-- * Benchmarking

-- | (Input Size, Runtime) results as coordinates for unary test programs.
type Coord = (Double, Double)

-- | (Input Size, Input Size, Runtime) results as coordinates for binary test 
-- programs.        
type Coord3 = (Double, Double, Double) 

-- | The size of unary and binary test data.
data DataSize = 
    SizeUn Int       -- ^ The size of unary test data.
  | SizeBin Int Int  -- ^ The size of binary test data.
    deriving (Ord, Eq)

-- | A report to summarise the benchmarking phase of testing.
data BenchReport =
  BenchReport 
    {
      _reports   :: [[SimpleReport]]  -- ^ Individual reports for each test case, per test program.
    , _baselines :: [SimpleReport]    -- ^ Baseline measurements (will be empty if '_baseline' is set to @False@).
    }

-- | A simplified version of Criterion's 'Report' datatype, see 
-- 'Criterion.Types.Report'.
data SimpleReport = 
  SimpleReport 
   { 
     _name       :: Id              -- ^ Name of test program.
   , _size       :: DataSize        -- ^ Size of test data.
   , _samples    :: Int             -- ^ Number of samples used to calculate statistics below.
   , _runtime    :: Double          -- ^ Estimate runtime.
   , _stdDev     :: Double          -- ^ Estimate standard deviation.
   , _outVarEff  :: OutlierEffect   -- ^ Outlier effect. 
   , _outVarFrac :: Double          -- ^ Outlier effect as a percentage.
   }

-- * Test results 

-- | A report to summarise the system's testing phase.
data TestReport = 
  TestReport 
    {
      _tProgs    :: [String]          -- ^ Names of all test programs.
    , _tDataOpts :: DataOpts          -- ^ Which test data options were used.
    , _tNf       :: Bool              -- ^ Whether test cases were evaluated to normal form.
    , _tGhcFlags :: [String]          -- ^ Flags used when compiling the benchmarking file.
    , _eql       :: Bool              -- ^ Whether test programs are semantically equal according to QuickCheck testing.
    , _br        :: BenchReport       -- ^ Benchmarking report.
    }

-- ** QuickBench 

-- | A report to summarise the QuickBench testing phase.
data QuickReport = 
  QuickReport 
    {
      _qName     :: Id                        -- ^ Name of test program.
    , _qRuntimes :: Either [Coord] [Coord3]   -- ^ [(Input size(s), mean runtime)].
    } deriving Show

-- * Statistical analysis

-- | A report to summarise the system's analysis phase.
data AnalysisReport = 
  AnalysisReport
    {
      _anlys :: [SimpleResults]       -- ^ Simple statistical analysis results per test program.   
    , _imps  :: [Improvement]         -- ^ Improvement results.
    , _blAn  :: Maybe SimpleResults   -- ^ Analysis of baseline measurements, if applicable.
    }

-- | Simple statistical analysis results for each test program. 
data SimpleResults = 
  SimpleResults 
   {
     _srIdt           :: Id                           -- ^ Name of test program.
   , _srRaws          :: Either [Coord] [Coord3]      -- ^ Raw input size/runtime results.
   , _srStdDev        :: Double                       -- ^ Standard deviation of all runtime results.
   , _srAvgOutVarEff  :: OutlierEffect                -- ^ Average outlier effect. 
   , _srAvgPutVarFrac :: Double                       -- ^ Average outlier effect as a percentage.
   , _srFits          :: [LinearFit]                  -- ^ Fitting statistics for each candidate model.
   }

-- | An efficiency improvement is an ordering between two test programs and a 
-- rating 0 <= d <= 1 that corresponds to the percentage of test cases that
-- support the ordering.
type Improvement = (Id, Ordering, Id, Double)

-- | Fitting statistics calculated for regression models per each iteration of
-- cross-validation. Cumulative fitting statistics are produced by combining 
-- 'CVStats' from all iterations, for example, PMSE and PMAE. See 'Stats'. 
data CVStats = 
  CVStats 
   { 
     _cv_mse    :: Double   -- ^ Mean squared error.
   , _cv_mae    :: Double   -- ^ Mean absolute error.
   , _cv_ss_tot :: Double   -- ^ Total sum of squares.
   , _cv_ss_res :: Double   -- ^ Residual sum of squares.
   } deriving Eq

-- | Expressions with 'Double' literals.
type Exp = E.Expr Double

-- | Each 'LinearType' gives rise to a 'LinearCandidate' that is then fitted to 
-- a given data set generating a 'LinearFit'. Unlike a 'LinearType', which
-- just describes a particular regression model, a 'LinearCandidate' 
-- encompasses the required information to fit a model to a given data set. 
-- For example, it includes '_fxs' to transforms the raw x-coordinates 
-- of the dataset before fitting the model, and '_fyhat' which can be used to 
-- generate y-coordinates predicted by the model once it has been fit.
--
-- For example, if fitting a 'Log b 1' model, '_fxs' will transform each 
-- x-coordinate in the data set to log_b(x) before fitting. Then the linear 
-- relationship between the /resulting/ xy-coordinates corresponds to a 
-- logarithmic relationship between the /initial/ xy-coordinates.
--
-- When the coefficients of a model are determined by regression analysis, 
-- the corresponding 'LinearCandidate' gives rise to a 'LinearFit'.
data LinearCandidate = 
  LinearCandidate 
   { 
     _lct    :: LinearType                                         -- ^ The model.
   , _fxs    :: Vector Double -> Vector Double                     -- ^ A function to transform x-coords before fitting.
   , _fex    :: Vector Double -> Exp                               -- ^ A function to generate the model's equation as an 'Exp'.
   , _fyhat  :: Vector Double -> Vector Double -> Vector Double    -- ^ A function to generate model's predicted y-coords.
   }

-- | When a 'LinearCandidate' is fitted to a given data set and its coefficients
-- determined by regression analysis, a 'LinearFit' is generated. 
-- A 'LinearFit' primarily includes the fitting statistics ('Stats') of the 
-- model to be used to compare it against other models also fitted to the same 
-- data set. In order to be able to plot a 'LinearFit' on a results graph 
-- as a line of best fit, the '_yhat' function generates y-coordinates 
-- predicted by the model for a given set of x-coordinates. To pretty print the 
-- 'LinearFit', the '_ex' function generates an 'Exp' expression for the model's
-- equation that has a pretty printing function.
data LinearFit =
  LinearFit 
   { 
     _lft  :: LinearType                       -- ^ The model.
   , _cfs  :: Vector Double                    -- ^ The coefficients of the model.
   , _ex   :: Exp                              -- ^ The model's equation as an 'Exp'.
   , _yhat :: Vector Double -> Vector Double   -- ^ A function to generate the model's predicted y-coords for a given set of x-coords.
   , _sts  :: Stats                            -- ^ Fitting statistics.
   }

-- | Number of predictors for each type of model.
numPredictors :: LinearType -> Int 
numPredictors (Poly      k) = k + 1 
numPredictors (Log     _ k) = k + 1 
numPredictors (PolyLog _ k) = k + 1 
numPredictors Exp{}         = 2

-- | Convert a list of 'SimpleReport's to a list of (input size(s), runtime) 
-- coordinates, i.e., a list 'Coord's or 'Coord3's. The name of each 
-- simple report is verified against the given test program identifier.
simpleReportsToCoords :: Id -> [SimpleReport] -> Either [Coord] [Coord3]
simpleReportsToCoords idt srs = case (cs, cs3) of 
  ([], _) -> Right cs3 
  (_, []) -> Left cs 
  _       -> Left [] -- Shouldn't happen.
  where 
    srs' = filter (\sr -> _name sr == idt) srs
    (cs, cs3) = partitionEithers (fmap simpleReportToCoord srs')

-- | Convert a 'SimpleReport' to a (input size(s), runtime) coordinate, 
-- i.e., 'Coord' or 'Coord3'.
simpleReportToCoord :: SimpleReport -> Either Coord Coord3 
simpleReportToCoord sr = case _size sr of 
  SizeUn n      -> Left  (fromIntegral n, _runtime sr)
  SizeBin n1 n2 -> Right (fromIntegral n1, fromIntegral n2, _runtime sr)

-- ** QuickBench 

-- | A report to summarise the system's analysis phase for QuickBenching.
data QuickAnalysis = 
  QuickAnalysis
    {
      _qAnlys :: [QuickResults]    -- ^ Quick results per test program.   
    , _qImps  :: [Improvement]     -- ^ Improvement results.
    }

-- | Simple statistical analysis results for each test program for QuickBenching.
data QuickResults = 
  QuickResults 
   {
     _qrIdt   :: Id                           -- ^ Name of test program.
   , _qrRaws  :: Either [Coord] [Coord3]      -- ^ Raw input size/runtime results.
   , _qrFits  :: [LinearFit]                  -- ^ Fitting statistics for each candidate model.
   }

-- * Errors 

-- | Errors raised by the system due to implementation failures. These can be 
-- generated at any time but are usually used to report unexpected IO results. 
-- For example, when dynamically checking user inputs (see 
-- 'AutoBench.Internal.UserInputChecks'), system errors are used to relay 
-- 'InterpreterError's thrown by functions in the hint package in cases
-- where the system didn't expect errors to result.
data SystemError = InternalErr String

-- Note: needed for the 'Exception' instance.
instance Show SystemError where 
  show (InternalErr s) = "Internal error: " ++ s ++ "\n** Please report on GitHub **"

instance Exception SystemError

-- ** Input errors

-- | Input errors are generated by the system while analysing user input 
-- files. Examples input errors include erroneous test options, invalid test 
-- data, and test programs with missing Arbitrary/NFData instances.
--
-- In general, the system always attempts to continue with its execution for as 
-- long as possible. Therefore, unless a critical error is encountered, such as 
-- a filepath or file access error, it will collate all non-critical input 
-- errors. These will then be summarised after the user input file has been 
-- fully analysed.
data InputError = 
    FilePathErr   String    -- ^ Invalid filepath.
  | FileErr       String    -- ^ File access error.
  | TestSuiteErr  String    -- ^ Invalid test suite.
  | DataOptsErr   String    -- ^ Invalid data options.
  | AnalOptsErr   String    -- ^ Invalid statistical analysis options.
  | TypeErr       String    -- ^ Invalid type signature.
  | InstanceErr   String    -- ^ One or more missing instances
  | TestReportErr String    -- ^ Invalid test report.
  | QuickOptsErr  String    -- ^ Invalid quick options. See AutoBench.QuickBench.
  | QuickBenchErr String    -- ^ QuickBench error. See AutoBench.QuickBench.

-- Note: needed for the 'Exception' instance.
instance Show InputError where 
  show (FilePathErr   s) = "File path error: "        ++ s
  show (FileErr       s) = "File error: "             ++ s
  show (TestSuiteErr  s) = "Test suite error: "       ++ s
  show (DataOptsErr   s) = "Test data error: "        ++ s
  show (AnalOptsErr   s) = "Analysis options error: " ++ s
  show (TypeErr       s) = "Type error: "             ++ s
  show (InstanceErr   s) = "Instance error: "         ++ s
  show (TestReportErr s) = "Test report error: "      ++ s
  show (QuickOptsErr  s) = "Quick options error: "    ++ s
  show (QuickBenchErr s) = "QuickBench test error: "  ++ s

instance Exception InputError