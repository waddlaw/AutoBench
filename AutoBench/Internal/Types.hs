
{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
   - 
-}

module AutoBench.Internal.Types 
  (

  -- * Re-exports
    module AutoBench.Types
  -- * User inputs
  -- ** Test data options
  , toHRange               -- Convert @Gen l s u :: DataOpts@ to a Haskell range.
  , minInputs              -- Minimum number of distinctly sized test inputs.
  , defBenchRepFilename    -- Default benchmarking JSON report filename.
  -- ** Statistical analysis options
  , maxPredictors          -- Maximum number of predictors for models to be used for regression analysis.         
  , minCVTrain             -- Minimum percentage of data set to use for cross-validation.
  , maxCVTrain             -- Maximum percentage of data set to use for cross-validation.
  , minCVIters             -- Minimum number of cross-validation iterations.
  , maxCVIters             -- Maximum number of cross-validation iterations.
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
  -- * Helpers 
  -- ** Pretty printing
  , docCoords              -- Generate a 'PP.Doc' for list of 'Coord's or 'Coord3's.
  , docImprovement         -- Generate a 'PP.Doc' for an 'Improvement'.
  , docQuickResults        -- Generate a 'PP.Doc' for list of 'QuickResults'.
  , docSimpleReport        -- Generate a 'PP.Doc' for a 'SimpleReport'.
  , docSimpleResults       -- Generate a 'PP.Doc' for list of 'SimpleResults'.
  , docTestReport          -- Generate a 'PP.Doc' for a 'TestReport'.
  , docTestSuite           -- Generate a 'PP.Doc' for a 'TestSuite'.
  , docUserInputs          -- Generate a 'PP.Doc' for a 'UserInputs'.
  , showImprovements       -- Pretty printing for a list of 'Improvement's.

  ) where

import           Control.Arrow             ((&&&))
import           Control.Exception.Base    (Exception)
import           Criterion.Types           (OutlierEffect(..))
import           Data.Either               (partitionEithers)
import           Data.List                 (sort, sortBy, transpose)
import           Data.List.Split           (chunksOf)
import           Data.Ord                  (comparing)
import           Data.Tuple.Select         (sel1, sel2, sel3)
import           Numeric.LinearAlgebra     (Vector)
import           Text.Printf               (printf)
import qualified Text.PrettyPrint.HughesPJ as PP

import qualified AutoBench.Internal.Expr  as E
import           AutoBench.Internal.Utils ( bySide, deggar, forceSecs, secs 
                                          , wrapPPList )
import           AutoBench.Types  -- Re-export.

import AutoBench.Internal.AbstractSyntax 
  ( HsType
  , Id
  , ModuleElem(..)
  , TypeString
  , prettyPrint
  , unqualIdt
  )


-- * User inputs

-- ** Test suites

-- | Convert @Gen l s u :: DataOpts@ to a Haskell range.
toHRange :: DataOpts -> [Int]
toHRange Manual{}    = []
toHRange (Gen l s u) = [l, (l + s) .. u]

-- | Each test suite requires a minimum number of distinctly sized test inputs.
-- 
-- > minInputs = 20
minInputs :: Int 
minInputs  = 20

-- | Default benchmarking JSON report filename.
defBenchRepFilename :: String
defBenchRepFilename  = "autobench_tmp.json"

-- ** Statistical analysis options

-- | Maximum number of predictors for models to be used for regression analysis.
--
-- > maxPredictors = 10
maxPredictors :: Int 
maxPredictors  = 10

-- | Minimum percentage of data set to use for cross-validation.
--
-- > minCVTrain = 0.5
minCVTrain :: Double 
minCVTrain  = 0.5

-- | Maximum percentage of data set to use for cross-validation.
--
-- > maxCVTrain = 0.8
maxCVTrain :: Double 
maxCVTrain  = 0.8

-- | Minimum number of cross-validation iterations.
--
-- > minCVIters = 100
minCVIters :: Int 
minCVIters  = 100

-- | Maximum number of cross-validation iterations.
--
-- > maxCVIters = 500
maxCVIters :: Int 
maxCVIters  = 500

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

instance Show DataSize where 
  show (SizeUn n)      = show n 
  show (SizeBin n1 n2) = show (n1, n2)

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

instance Show CVStats where 
  show cvSts = flip bySide " " $ fmap PP.vcat $ transpose  
    [ [ PP.text "MSE", PP.char '=', PP.text $ printf ".4g" (_cv_mse    cvSts) ] 
    , [ PP.text "MAE", PP.char '=', PP.text $ printf ".4g" (_cv_mae    cvSts) ]
    , [ PP.text "SST", PP.char '=', PP.text $ printf ".4g" (_cv_ss_tot cvSts) ]
    , [ PP.text "SSR", PP.char '=', PP.text $ printf ".4g" (_cv_ss_res cvSts) ]
    ]

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

instance Show SystemError where 
  show (InternalErr s) = "Internal error: " ++ s ++ "\nplease report on GitHub."

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

instance Show InputError where 
  show (FilePathErr   s) = "File path error: "        ++ s
  show (FileErr       s) = "File error: "             ++ s
  show (TestSuiteErr  s) = "Test suite error: "       ++ s
  show (DataOptsErr   s) = "Test data error: "        ++ s
  show (AnalOptsErr   s) = "Analysis options error: " ++ s
  show (TypeErr       s) = "Type error: "             ++ s
  show (InstanceErr   s) = "Instance error: "         ++ s
  show (TestReportErr s) = "Test report error: "      ++ s

instance Exception InputError

-- * Helpers 

-- ** Pretty printing 

-- | Pretty printing for 'Improvement's.
docImprovement :: Bool -> Improvement -> PP.Doc 
docImprovement b (idt1, ord, idt2, d) = 
  PP.hsep $ docImprovement' b (unqualIdt idt1, ord, unqualIdt idt2, d)

-- | Pretty printing helper for 'Improvement's.
docImprovement' :: Bool -> Improvement -> [PP.Doc] 
docImprovement' b (idt1, LT, idt2, d) = docImprovement' b (idt2, GT, idt1, d)
docImprovement' True  (idt1, EQ, idt2, d) = 
  [ PP.text idt1, PP.text "\8804\8805", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]
docImprovement' False (idt1, EQ, idt2, d) =  
  [ PP.text idt1, PP.text "\8818\8819", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]
docImprovement' True  (idt1, GT, idt2, d) = 
  [ PP.text idt1, PP.text "\8805", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]
docImprovement' False (idt1, GT, idt2, d) = 
  [ PP.text idt1, PP.text "\8819", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]

-- | Pretty printing for a list of 'Improvement's.
showImprovements :: Bool -> [Improvement] -> String 
showImprovements b imps = bySide (fmap PP.vcat $ transpose docImps) " "
  where 
    imps' = fmap (\(idt1, ord, idt2, d) -> 
      (unqualIdt idt1, ord, unqualIdt idt2, d)) imps
    docImps = fmap (docImprovement' b) imps'

-- | Simplified pretty printing for the 'TestSuite' data structure.
-- Note: prints test programs and 'DataOpts' only.
docTestSuite :: TestSuite -> PP.Doc                                                                        
docTestSuite ts = PP.vcat 
  [ 
    wrapPPList 60 ", " (_progs ts)   -- Names of test programs.
  , PP.text $ show $ _dataOpts ts    -- Data options.
  ]

-- | Pretty printing for the 'UserInputs' data structure. 
-- Prints all fields; invalids are printed last.                                               
docUserInputs :: UserInputs -> PP.Doc 
docUserInputs inps = PP.vcat $ PP.punctuate (PP.text "\n")
  [ PP.text "All module elements:"     PP.$$ (PP.nest 2 $ showElems             $ _allElems          inps)
  , PP.text "Valid module elements:"   PP.$$ (PP.nest 2 $ showTypeableElems     $ _validElems        inps)
  , PP.text "Nullary functions:"       PP.$$ (PP.nest 2 $ showTypeableElems     $ _nullaryFuns       inps)
  , PP.text "Unary functions:"         PP.$$ (PP.nest 2 $ showTypeableElems     $ _unaryFuns         inps)
  , PP.text "Binary functions:"        PP.$$ (PP.nest 2 $ showTypeableElems     $ _binaryFuns        inps)
  , PP.text "Benchmarkable functions:" PP.$$ (PP.nest 2 $ showTypeableElems     $ _benchFuns         inps)
  , PP.text "Arbitrary functions:"     PP.$$ (PP.nest 2 $ showTypeableElems     $ _arbFuns           inps)
  , PP.text "NFData functions:"        PP.$$ (PP.nest 2 $ showTypeableElems     $ _nfFuns            inps)
  , PP.text "Unary test data:"         PP.$$ (PP.nest 2 $ showTypeableElems     $ fmap (sel1 &&& sel2) $ _unaryData  inps) -- Don't print sizing information.
  , PP.text "Binary test data:"        PP.$$ (PP.nest 2 $ showTypeableElems     $ fmap (sel1 &&& sel2) $ _binaryData inps) -- Don't print sizing information.
  , PP.text "Test suites:"             PP.$$ (PP.nest 2 $ showTestSuites        $ _testSuites        inps)
  -- Invalids come last because they have 'InputError's.
  , PP.text "Invalid module elements:" PP.$$ (PP.nest 2 $ showElems             $ _invalidElems      inps) 
  , PP.text "Invalid test data:"       PP.$$ (PP.nest 2 $ showInvalidData       $ _invalidData       inps)
  , PP.text "Invalid test suites:"     PP.$$ (PP.nest 2 $ showInvalidTestSuites $ _invalidTestSuites inps)
  ]
  
  where 
    -- Pretty printing for @[(ModuleElem, Maybe TypeString)]@.
    showElems :: [(ModuleElem, Maybe TypeString)] -> PP.Doc 
    showElems [] = PP.text "N/A"
    showElems xs = PP.vcat [showDs, showCs, showFs]
      where 
        -- Split into (Fun, Class, Data).
        ((fs, tys), cs, ds) = foldr splitShowModuleElems (([], []), [], []) xs

        -- Data.
        showDs | null ds   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Data:"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort ds
                   ]
        -- Class.
        showCs | null cs   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Class:"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort cs
                   ]
        -- Fun.
        showFs | null fs   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Fun:"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort $ 
                       zipWith (\idt ty -> idt ++ " :: " ++ ty) (deggar fs) tys
                   ]

    -- Pretty printing for @[(Id, HsType)]@.
    showTypeableElems :: [(Id, HsType)] -> PP.Doc
    showTypeableElems [] = PP.text "N/A"
    showTypeableElems xs = PP.vcat $ fmap PP.text $ sort $ 
      zipWith (\idt ty -> idt ++ " :: " ++ prettyPrint ty) (deggar idts) tys
      where (idts, tys) = unzip xs

    -- Pretty printing for 'TestSuite's.
    showTestSuites :: [(Id, TestSuite)] -> PP.Doc 
    showTestSuites [] = PP.text "N/A"
    showTestSuites xs = PP.vcat $ fmap (uncurry showTestSuite) $ 
      sortBy (comparing fst) xs
      where 
        showTestSuite :: Id -> TestSuite -> PP.Doc 
        showTestSuite idt ts = PP.vcat 
          [ PP.text idt PP.<+> PP.text ":: TestSuite"
          , PP.nest 2 $ PP.vcat $ fmap PP.text (_progs ts)
          ]

    -- Invalids, need additional nesting for input errors: --------------------
    -- Note: don't forget to sort alphabetically. 

    showInvalidData :: [(Id, HsType, [InputError])] -> PP.Doc
    showInvalidData [] = PP.text "N/A"
    showInvalidData xs = PP.vcat $ fmap showInvalidDat $ 
      sortBy (comparing sel1) xs
      where 
        showInvalidDat :: (Id, HsType, [InputError]) -> PP.Doc
        showInvalidDat (idt, ty, errs) = PP.vcat 
          [ PP.text $ idt ++ " :: " ++ prettyPrint ty
          , PP.nest 2 $ PP.vcat $ fmap (PP.text . show) $ 
              sortBy (comparing show) errs 
          ]

    showInvalidTestSuites :: [(Id, [InputError])]  -> PP.Doc 
    showInvalidTestSuites [] = PP.text "N/A"
    showInvalidTestSuites xs = PP.vcat $ fmap showInvalidTestSuite $ 
      sortBy (comparing fst) xs
      where 
        showInvalidTestSuite :: (Id, [InputError]) -> PP.Doc 
        showInvalidTestSuite (idt, errs) = PP.vcat 
          [ PP.text idt PP.<+> PP.text ":: TestSuite"
          , PP.nest 2 $ PP.vcat $ fmap (PP.text . show) $ 
              sortBy (comparing show) errs 
          ]

    -- Helpers:

    -- Split the 'ModuleElem's to display 'Fun' types by the side of 'Fun' 
    -- identifiers. (The 'Class' and 'Data' 'ModuleElem's don't have typing 
    -- information.)
    splitShowModuleElems 
      :: (ModuleElem, Maybe TypeString)
      -> (([String], [String]), [String], [String]) 
      -> (([String], [String]), [String], [String])
    -- Types.
    splitShowModuleElems (Fun idt, Just ty) ((fs, tys), cs, ds) = 
      ((idt : fs, ty : tys), cs, ds)
    -- No types.
    splitShowModuleElems (Fun idt, Nothing) ((fs, tys), cs, ds) = 
      ((idt : fs, "" : tys), cs, ds) -- Shouldn't happen.
    splitShowModuleElems (Class idt _, _) (fs, cs, ds) = (fs, idt : cs, ds)
    splitShowModuleElems (Data idt _, _)  (fs, cs, ds) = (fs, cs, idt : ds)

-- | Pretty printing for the 'SimpleReport' data structure. 
docSimpleReport :: SimpleReport -> PP.Doc 
docSimpleReport sr = PP.vcat $
  [ PP.text (_name sr)
  , PP.nest 2 $ ppSizeRuntime (_size sr) (_runtime sr)
  ]
  where 
    ppSizeRuntime (SizeUn n) d = PP.char '(' PP.<> PP.int n PP.<> PP.text ", " 
      PP.<> PP.double d PP.<> PP.char ')'
    ppSizeRuntime (SizeBin n1 n2) d = PP.char '(' PP.<> PP.int n1 PP.<> 
      PP.text ", " PP.<> PP.int n2 PP.<> PP.text ", " PP.<> 
      PP.double d PP.<> PP.char ')'

-- | Full pretty printing for 'TestReport' data structure.
docTestReport :: TestReport -> PP.Doc
docTestReport tr = PP.vcat 
  [ PP.hcat $ (PP.text "Test programs: ") : (PP.punctuate (PP.text ", ") $
      fmap PP.text $ _tProgs tr)
  , PP.text "Data options:" PP.<+> PP.text (show $ _tDataOpts tr)
  , PP.text "Normal form:" PP.<+> PP.text (show $ _tNf tr)
  , PP.text "Semantically equal" PP.<+> PP.text (show $ _eql tr)
  , ppGhcFlags (_tGhcFlags tr)
  , PP.text ""
  , docBenchReport (_br tr)
  ]

  where 
    -- Pretty print list of GHC flags.
    ppGhcFlags :: [String] -> PP.Doc 
    ppGhcFlags [] = PP.empty
    ppGhcFlags flags = PP.vcat $ (PP.text "GHC flags: ") : 
      (PP.punctuate (PP.text ", ") $ fmap PP.text $ flags)

-- | Pretty printing for 'BenchReport' data structure.
docBenchReport :: BenchReport -> PP.Doc 
docBenchReport br = PP.vcat
  [ PP.text "Reports:" PP.$$ PP.nest 2 (PP.vcat $ fmap ppTestResults $ _reports br)
  , ppBaselines (_baselines br)
  ]
  
  where 
    -- Pretty print 'SimpleReport's belonging to same test program.
    ppTestResults :: [SimpleReport] -> PP.Doc 
    ppTestResults [] = PP.empty
    ppTestResults srs = PP.vcat $ 
      [ PP.text (_name $ head srs)
      , PP.nest 2 $ PP.vcat $ 
          fmap (\sr -> ppSizeRuntime (_size sr) (_runtime sr)) srs
      ]

    -- Pretty print baseline measurements.
    ppBaselines :: [SimpleReport] -> PP.Doc
    ppBaselines []  = PP.empty
    ppBaselines bls = PP.text "Baseline measurements:" PP.$$ (PP.nest 2 $ 
      PP.vcat $ fmap (\bl -> ppSizeRuntime (_size bl) (_runtime bl)) bls)

    -- PrettyPrint (input size(s), runtime) 2- or 3-tuples.
    ppSizeRuntime :: DataSize -> Double -> PP.Doc
    ppSizeRuntime (SizeUn n) d = PP.char '(' PP.<> PP.int n PP.<> PP.text ", " 
      PP.<> PP.double d PP.<> PP.char ')'
    ppSizeRuntime (SizeBin n1 n2) d = PP.char '(' PP.<> PP.int n1 PP.<> 
      PP.text ", " PP.<> PP.int n2 PP.<> PP.text ", " PP.<> 
      PP.double d PP.<> PP.char ')'

-- | Pretty printing for a list of 'SimpleResults'. The maximum runtime among 
-- all test cases is formatted into seconds/milliseconds/nanoseconds etc. and 
-- the rest of the results are forced into the same units for consistency. 
-- This makes it easier to compare runtimes at a glance of the raw results on 
-- the command line.
docSimpleResults :: [SimpleResults] -> PP.Doc 
docSimpleResults srs = (PP.vcat $ PP.punctuate (PP.text "\n") $ 
  fmap (docSimpleResult units) srs) PP.<> PP.text "\n" 
  where 
    maxRuntime = maximum $ fmap (maxFromCoords . _srRaws) srs  -- Maximum runtime of all test cases.
    (_, units) = secs maxRuntime                               -- Display all runtimes in the same /units/.

    -- Maximum runtime from a set of coordinates.
    maxFromCoords :: Either [Coord] [Coord3] -> Double 
    maxFromCoords (Left cs)  = maximum (fmap snd cs)
    maxFromCoords (Right cs) = maximum (fmap sel3 cs)

-- | Pretty printing for 'SimpleResults' data structure. The runtimes of test
-- programs are formatted according the /units/ parameter. See 'forceSecs'.
-- (This makes it easier to compare runtimes as they are all in the same units.)
docSimpleResult :: String -> SimpleResults -> PP.Doc 
docSimpleResult units sr = title PP.$$ (PP.nest 2 $ PP.vcat 
  [ PP.text size   PP.<+> sizes    -- Input sizes.
  , PP.text time   PP.<+> runtimes -- Runtimes.
   -- Simple cumulative statistics for all test cases.
  , PP.text stdDev PP.<+> PP.text (forceSecs maxWidth units $ _srStdDev sr)    
  , (PP.text $ "Average variance introduced by outliers: " ++ 
      printf "%d%% (%s)" (round (_srAvgPutVarFrac sr * 100) :: Int) wibble) PP.<> PP.text "\n" 
  , fits -- 'LinearFits'.
  ])

  where 
    -- Side headings with some manual spacing so everything aligns properly.
    title = PP.text (_srIdt sr) PP.<> PP.char ':'                 -- Name of program.
    size   = "Size    " ++ replicate (length sUnits) ' ' ++ " "   -- Input sizes.
    time   = "Time    " ++ sUnits ++ " "                          -- Runtime measurements.
    stdDev = "Std dev " ++ sUnits ++ " "                          -- Standard deviation.
    sUnits  = "(" ++ units ++ ")"                                 -- Forced units.
    
    -- Output input sizes and runtime measurements in a tabular format with
    -- maximum width of ~80.
    (sizes, runtimes) = docCoordsTabular 60 maxWidth units (_srRaws sr)
    
    -- Pretty print the equations of 'LinearFits'.
    fits :: PP.Doc 
    fits = case _srFits sr of 
      []   -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> PP.text "N/A"
      [lf] -> PP.text ("Fit" ++ replicate (length units + 8) ' ') 
        PP.<+> docLinearFitsTabular 60 [lf]
      lfs  -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> docLinearFitsTabular 60 lfs

    -- Helpers:

    -- Maximum width of input sizes: to align table columns.
    maxWidth :: Int 
    maxWidth  = case (_srRaws sr) of
      Left  cs -> max (length . show . round' . maximum $ fmap fst cs) 7
      Right cs -> max (maximum $ fmap (\(x1, x2, _) -> 
        length (show $ round' x1) + length (show $ round' x2) + 5) cs) 7

    -- Note: taken from Criterion source code.. wibble??
    wibble = case _srAvgOutVarEff sr of
      Unaffected -> "unaffected"
      Slight     -> "slightly inflated"
      Moderate   -> "moderately inflated"
      Severe     -> "severely inflated"
 

-- | Pretty printing for a list of 'QuickResults'. The maximum runtime among 
-- all test cases is formatted into seconds/milliseconds/nanoseconds etc. and 
-- the rest of the results are forced into the same units for consistency. 
-- This makes it easier to compare runtimes at a glance of the raw results on 
-- the command line.
docQuickResults :: [QuickResults] -> PP.Doc 
docQuickResults qrs = (PP.vcat $ PP.punctuate (PP.text "\n") $ 
  fmap (docQuickResult units) qrs) PP.<> PP.text "\n"
  where 
    maxRuntime = maximum $ fmap (maxFromCoords . _qrRaws) qrs  -- Maximum runtime of all test cases.
    (_, units) = secs maxRuntime                               -- Display all runtimes in the same /units/.

    -- Maximum runtime from a set of coordinates.
    maxFromCoords :: Either [Coord] [Coord3] -> Double 
    maxFromCoords (Left cs)  = maximum (fmap snd cs)
    maxFromCoords (Right cs) = maximum (fmap sel3 cs)


-- | Pretty printing for 'QuickResults' data structure. The runtimes of test
-- programs are formatted according the /units/ parameter. See 'forceSecs'.
-- (This makes it easier to compare runtimes as they are all in the same units.)
docQuickResult :: String -> QuickResults -> PP.Doc 
docQuickResult units qr = title PP.$$ (PP.nest 2 $ PP.vcat 
  [ 
    PP.text size PP.<+> sizes                        -- Input sizes.
  , PP.text time PP.<+> runtimes PP.<> PP.text "\n"  -- Runtimes.
  , fits                                             -- 'LinearFits'.
  ])

  where 
    -- Side headings with some manual spacing so everything aligns properly.
    title = PP.text (_qrIdt qr) PP.<> PP.char ':'                 -- Name of program.
    size   = "Size    " ++ replicate (length sUnits) ' ' ++ " "   -- Input sizes.
    time   = "Time    " ++ sUnits ++ " "                          -- Runtime measurements.
    sUnits  = "(" ++ units ++ ")"                                 -- Forced units.
    
    -- Output input sizes and runtime measurements in a tabular format with
    -- maximum width of ~80.
    (sizes, runtimes) = docCoordsTabular 60 maxWidth units (_qrRaws qr)
    
    -- Pretty print the equations of 'LinearFits'.
    fits :: PP.Doc 
    fits = case _qrFits qr of 
      []   -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> PP.text "N/A"
      [lf] -> PP.text ("Fit" ++ replicate (length units + 8) ' ') 
        PP.<+> docLinearFitsTabular 60 [lf]
      lfs  -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> docLinearFitsTabular 60 lfs

    -- Helpers:

    -- Maximum width of input sizes: to align table columns.
    maxWidth :: Int 
    maxWidth  = case _qrRaws qr of
      Left  cs -> max (length . show . round' . maximum $ fmap fst cs) 7
      Right cs -> max (maximum $ fmap (\(x1, x2, _) -> 
        length (show $ round' x1) + length (show $ round' x2) + 5) cs) 7

-- | Pretty print the equations of linear fits in a tabular layout of a given 
-- width.
docLinearFitsTabular :: Int -> [LinearFit] -> PP.Doc 
docLinearFitsTabular _ [] = PP.empty 
docLinearFitsTabular width lfs = 
  PP.vcat $ fmap ((PP.text "y =" PP.<+>) . E.wrapDocExpr (width - 5) . _ex) lfs -- Subtract 5 width for "y = ".

-- | Layout coordinates in tabular format of a given width. It is 
-- slightly involved because multiple rows are required for both input sizes 
-- and runtimes, so stack them. Force runtimes to be the given units.
docCoordsTabular 
  :: Int                        -- Table width.
  -> Int                        -- Width of each column.
  -> String                     -- Runtime units.
  -> Either [Coord] [Coord3]    -- Measurements.
  -> (PP.Doc, PP.Doc)
docCoordsTabular maxWidth width units (Left cs) = 
  (PP.vcat $ hsepChunks' xs, PP.vcat $ hsepChunks' ys)
  where 
    xs = fmap (PP.text . printf ("%-" ++ show width ++ "s") . show . 
      round' . fst) cs' 
    ys = fmap (PP.text . forceSecs width units . snd) cs'
    cs' = sort cs
    hsepChunks' = hsepChunks maxWidth width
-- Input sizes for 'Coord3's are printed as tuples.
docCoordsTabular maxWidth width units (Right cs) = 
  (PP.vcat $ hsepChunks' xs, PP.vcat $ hsepChunks' ys')
  where 
    (xs1, xs2, ys) = unzip3 cs'
    xs = zipWith (\x1 x2 -> PP.text $ printf ("%-" ++ show width ++ "s") $ 
      show $ PP.char '(' PP.<> PP.int (round' x1) PP.<> PP.char ',' PP.<+> 
      PP.int (round x2) PP.<> PP.char ')') xs1 xs2
    ys' = fmap (PP.text . forceSecs width units) ys
    cs' = sort cs
    hsepChunks' = hsepChunks maxWidth width

-- | Chunk off rows into maxWidth/width columns and then display chunks 
-- horizontally. 
hsepChunks :: Int -> Int -> [PP.Doc] -> [PP.Doc]
hsepChunks maxWidth width = fmap PP.hsep . chunksOf (maxWidth `div` width)

-- | Just for typing information.
round' :: Double -> Int 
round'  = round

-- | Pretty printing for coordinates, non-tabular format.
docCoords :: Either [Coord] [Coord3] -> PP.Doc 
docCoords (Left  cs) = PP.vcat $ fmap (\(s, t) -> PP.int (round s) PP.<> 
  PP.char ',' PP.<> PP.double t) cs
docCoords (Right cs) = PP.vcat $ fmap (\(s1, s2, t) -> PP.int (round s1) 
  PP.<> PP.char ',' PP.<> PP.int (round s2) PP.<> PP.char ',' PP.<>
  PP.double t) cs