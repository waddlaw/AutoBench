
{-# LANGUAGE DeriveGeneric        #-}
{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

  Module      : AutoBench.Types
  Description : User input datatypes and associated helper functions and 
                defaults.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module defines user input datatypes:

  * Test suites: 'TestSuite's;
  * Manual test data: 'UnaryTestData', 'BinaryTestData';
  * Test data options: 'DataOpts';
  * Statistical analysis options: 'AnalOpts'.

  These are to be included in user input files to customise the functionality
  of the system.

  This module also defines datatypes used for statistical analysis:

  * Regression models: 'LinearTypes';
  * Fitting statistics: 'Stats'.

  Users should be aware of these datatypes should they wish to provide
  custom 'AnalOpts'. 

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.Types 
  (

  -- * User inputs
  -- ** Test suites
    TestSuite(..)                       -- Test suites are AutoBench's principle user input datatype.
  -- ** Test data options
  , UnaryTestData                       -- User-specified test data for unary test programs.
  , BinaryTestData                      -- User-specified test data for binary test programs.
  , DataOpts(..)                        -- Test data options.
  -- ** Statistical analysis options
  , AnalOpts(..)                        -- Statistical analysis options.
  -- * Statistical analysis
  , LinearType(..)                      -- Functions used as models for regression analysis.
  , Stats(..)                           -- Fitting statistics used to compare regression models.

  ) where

import           Control.DeepSeq                  (NFData)
import qualified Criterion.Types                  as Criterion
import qualified Criterion.Main                   as Criterion
import           Data.Default                     (Default(..))
import           Data.List                        (genericLength, transpose)
import           GHC.Generics                     (Generic)
import           Numeric.MathFunctions.Comparison (relativeError)
import qualified Text.PrettyPrint.HughesPJ        as PP
import           Text.Printf                      (printf)

import AutoBench.Internal.AbstractSyntax (Id)
import AutoBench.Internal.Utils          (bySide, subNum, superNum)


-- To be able to 'rnf' 'TestSuite's.
instance NFData Criterion.Verbosity
instance NFData Criterion.Config

-- * User inputs

-- ** Test suites

-- | Test suites are AutoBench's principle user input datatype, and are used to 
-- structure performance tests into logical units that can be checked, 
-- validated, and executed independently. 
--
-- An advantage of this approach is that users can group multiple test 
-- suites in the same file according to some testing context, whether it be 
-- analysing the performance of the same programs subject to different levels 
-- of optimisation, or comparing different implementations under the same
-- test conditions. Another advantage is that if one or more test suites in a 
-- user input file are erroneous, other, valid test suites in the same file can 
-- be executed nonetheless.
--
-- Test suites are constructed as follows:
--
-- * '_progs': the so-called \'progs\' list contains the names of the programs 
--   to be tested. Each program in the list must be defined in the same
--   file as the 'TestSuite'. All programs should have the same type, and that
--   type must be compatible with the remainder of the 'TestSuite's settings. 
--   For example, if @_nf = True@, then the result type of test programs must 
--   be a member of the 'NFData' type class. If @_dataOpts = Gen ...@, then 
--   the input type of test programs must be a member of the 'Arbitrary' type 
--   class. Users can specify an empty \'progs\' list, in which case all 
--   programs in the input file will be considered for testing: zero or more 
--   test suites will be generated with non-empty \'progs\' list satisfying the 
--   remainder of the 'TestSuite's settings. (The system effectively 'fills'
--   in the details on behalf of users.)
-- * '_dataOpts': the data options ('DataOpts') specify which test data to 
--   use. Users have two options: provide their own test data (@Manual "..."@) 
--   or have the system generate it (@Gen ...@). See 'DataOpts' for more 
--   information. Again, the types of the test programs must be compatible with 
--   this setting, for example, @Gen ...@ requires the input types of test 
--   programs be members of the 'Arbitrary' type class.
-- * '_analOpts': a large number of user options for statistical analysis. 
--   These options include: which types of functions to use as models for 
--   regression analysis (when approximating the time complexity of each 
--   test program); functions to calculate improvement results, functions 
--   to filter and compare regression models based on fitting 'Stats' produced 
--   by the system. See 'AnalOpts' for more information.
-- * '_critCfg': Criterion's configuration. When benchmarks are executed by 
--   Criterion, this configuration is used. This allows users to configure
--   Criterion as if it was being used directly. Note: the default 
--   configuration works just fine and this option is mainly for users
--   who wish to generate Criterion reports as well AutoBench reports.
-- * '_baseline': whether the system should produce baseline measurements.
--   These measure the time spent evaluating the /results/ of test programs
--   to normal form. This can be useful if the runtimes of test programs
--   look odd. For example, if the identity function is tested on lists of 
--   integers and test cases are evaluated to normal form, the system will 
--   approximate @id@ as linear. However, it clearly has constant time 
--   complexity. The linear factor comes from the time spent normalising each 
--   result list. This can be seen using baseline measurements. Note: the 
--   baseline setting can only be used in conjunction with the '_nf' setting.
-- * '_nf': whether test cases should be evaluated to normal form (@True@) or 
--   weak head normal form (@False@). Typically test cases should be evaluated
--   to normal form to ensure the full cost of applying each test program is 
--   reflected in runtime measurements.
-- * '_ghcFlags': any GHC compiler flags to use when compiling benchmarking 
--   files. One obvious use case is to add optimisation flags, e.g., -O2/O3.
--   Note: invalid flags are ignored but displayed to users as warnings.
--
-- All 'TestSuite' options and settings are carefully validated. All errors are 
-- reported to users and invalid 'TestSuite's cannot be run by the system.
-- 
-- The system provides the following default 'TestSuite':
--
-- @ TestSuite
--     { _progs    = []                                     -- All programs in the test file will be considered for test purposes.
--     , _dataOpts = def                                    -- See 'DataOpts'.
--     , _analOpts = def                                    -- See 'AnalOpts'.
--     , _critCfg  = Criterion.Main.Options.defaultConfig   -- See 'Criterion.Main.Options.defaultConfig'
--     , _baseline = False                                  -- No baseline measurements.
--     , _nf       = True                                   -- Evaluate test cases to normal form.
--     , _ghcFlags = []                                     -- No optimisation, i.e., -O0.
--     }
-- @
--
-- Important note: the most basic check that the system performs on every test
-- suite is to ensure that each of its record fields are initialised: please
-- ensure test suites are fully defined.
data TestSuite = 
  TestSuite
    {  
      _progs    :: [Id]              -- ^ Identifiers of programs in the input file to test: note all programs
                                     --   in the file will be considered if this list is empty.
    , _dataOpts :: DataOpts          -- ^ Test data options ('DataOpts').
    , _analOpts :: AnalOpts          -- ^ Statistical analysis options ('AnalOpts').
    , _critCfg  :: Criterion.Config  -- ^ Criterion's configuration ('Criterion.Types.Config').
    , _baseline :: Bool              -- ^ Whether the graphs of runtime results should include baseline measurements.
    , _nf       :: Bool              -- ^ Whether test cases should be evaluated to nf (@True@) or whnf (@False@).
    , _ghcFlags :: [String]          -- ^ GHC compiler flags used when compiling files compiling benchmarks.
    } deriving (Generic)

instance NFData TestSuite 

instance Default TestSuite where
  def = TestSuite
          { 
            _progs    = []                         -- All programs in the test file will be considered for test purposes.           
          , _dataOpts = def                        -- See 'DataOpts'. 
          , _analOpts = def                        -- See 'AnalOpts'.        
          , _critCfg  = Criterion.defaultConfig    -- See 'Criterion.Main.Options.defaultConfig'
          , _baseline = False                      -- No baseline measurements.
          , _nf       = True                       -- Evaluate test cases to normal form.
          , _ghcFlags = []                         -- No optimisation, i.e., -O0. 
          }





















-- ** Test data options

-- | @type UnaryTestData a = [(Int, IO a)]@.
--
-- Due to certain benchmarking requirements, test data must be specified in 
-- the IO monad. In addition, the system cannot determine the size of 
-- user-specified test data automatically. As such, for a test program 
-- @p :: a -> b@ , user-specified test data is of type @[(Int, IO a)]@, where 
-- the first element of each tuple is the size of the test input, and the second 
-- element is the input itself. 
--
-- Concrete example: test program @p :: [Int] -> [Int]@, user-specified test 
-- data @tDat@.
--
-- @ tDat :: UnaryTestData [Int]
-- tDat = 
--   [ ( 5
--     , return [1,2,3,4,5] 
--     )
--   , ( 10 
--     , return [1,2,3,4,5,6,7,8,9,10] 
--     )
--   ... ]@
-- 
-- Here the size of each @[Int]@ is determined by its number of elements 
-- (@5@ and @10@, respectively).
--
-- Note: test suites require a minimum number of /distinctly sized/ test 
-- inputs: see 'minInputs'.
-- 
-- __**Incorrectly sized test data will lead to erroneous performance results**__.
type UnaryTestData a = [(Int, IO a)]  

-- | @type BinaryTestData a b = [(Int, Int, IO a, IO b)]@
--
-- See 'UnaryTestData' for a discussion on user-specified test data and a
-- relevant example for unary test programs. This example generalises to
-- user-specified test data for binary test programs in the obvious way:
-- 
-- @tDat :: BinaryTestData [Char] [Int]
-- tDat = 
--   [ ( 5
--     , 4
--     , return [/'a/', /'b/', /'c/', /'d/', /'e/']
--     , return [0, 1, 2, 3] )
--   , ( 10 
--     , 9
--     , return [/'a/', /'b/', /'c/', /'d/', /'e/', /'f/', /'g/', /'h/', /'i/', /'j/'] )
--     , return [0, 1, 2, 3, 4, 5, 6, 7, 8]
--   ... ]@
--
-- 'TestSuite's require a minimum number of /distinctly sized/ test datums: see 
-- 'minInputs'. In the case of 'BinaryTestData', /pairs/ of sizes must be 
-- distinct. For example, @(5, 4)@ and @(10, 9)@ above are two distinct pairs of 
-- sizes.
--
-- __**Incorrectly sized test data will lead to invalid performance results**__.
type BinaryTestData a b = [(Int, Int, IO a, IO b)]

-- | Test data can either be specified by users or generated automatically by 
-- the system. Note: the default setting for 'DataOpts' is @Gen 0 5 100@.
--
-- If users choose to specify their own inputs, then the 'Manual' data option 
-- simply tells the system the name of the test data in the user input file.
-- For example: 
--
-- @ module UserInput where 
--
-- tProg :: [Int] -> [Int]
-- tProg  = ...
--
-- tDat :: UnaryTestData [Int]
-- tDat  = ...
--
-- ts :: TestSuite 
-- ts  = def { _progs = ["tProg"], _dataOpts = Manual "tDat" }
-- @
--
-- See 'UnaryTestData' and 'BinaryTestData' for details regarding the /types/ 
-- of user-specified test data. 
--
-- If test data should be generated by the system, users must specify the size 
-- of the data to be generated. This is achieved using @Gen l s u@, 
-- which specifies as a size /range/ by a lower bound @l@, an upper bound 
-- @u@, and a step @s@. This is converted to a Haskell range 
-- @[l, (l + s) .. u]@ and a test input is generated for each size in this
-- list. 
-- For example: @Gen 0 5 100@ corresponds to the range @[0, 5, 10 .. 100]@. 
--
-- 'TestSuite's require a minimum number of /distinctly sized/ inputs: see 
-- 'minInputs'.
--
-- __**Incorrectly sized test data will lead to erroneous performance results**__.
data DataOpts = 
    Manual Id           -- ^ The system should search for user-specified test data
                        --   with the given name in the user input file.
  | Gen Int Int Int     -- ^ The system should generate random test data in the given size range.
 -- | Discover          -- ^ <TO-DO>: The system should discover compatible user-specified 
                        -- data, or accept a suitable 'Gen' setting at a later time.
    deriving (Eq, Generic)

instance NFData DataOpts 

instance Show DataOpts where 
  show (Manual idt) = "Manual " ++ "\"" ++ idt ++ "\""
  show (Gen l s u)  = "Random, size range [" ++ show l ++ "," ++ show s  ++ ".." ++ show u ++ "]"

instance Default DataOpts where 
  def = Gen 0 5 100

-- ** Statistical analysis options                                                  -- ** NEEDS COMMENTS ** 

-- | The system provides a number of user options for statistical analysis, 
-- including:
--
-- * Which types of functions ('LinearType's) to consider for regression 
--   analysis. See 'LinearType'. Note that 'LinearType's can have at most @10@ 
--   predictors, for example, @Poly 9@ is the maximum polynomial supported;                    
-- * The number of iterations of Monte Carlo cross-validation to perform.
--   @100 <= x <= 500@;
-- * The percentage of test data to use as training data for cross-validation.
--   @0.5 <= x <= 0.8@;
-- * The number of models to review when selecting the best fitting model
--   from the results of regression analysis. @n > 0@. Note that @n = 1@ means 
--   the system will pick its top-ranked model on behalf of users without
--   offering to review other options;
-- * A function to discard models that \"do not\" fit a given data set based on 
--   the fitting statistics produced by the system. See 'Stats'.
-- * A function to rank models according to how they fit a given data set 
--   based on the fitting statistics produced by the system. See 'Stats';
-- * A function to calculate improvement results by comparing the runtimes 
--   of two test programs pointwise.
--
-- The system can produce a number of performance results, including:
--
-- * A PNG graph of runtime measurements with complexity estimates plotted as 
--   lines of best fit;
-- * A CSV performance report;
-- * A CSV of (input size(s), runtime) coordinates for each test program.
--
-- The default options are as follows:
--
-- @
-- AnalOpts
--   {
--     _linearModels = [ Poly 0, Poly 1, Poly 2, Poly 3, Poly 4  
--                     , Log 2 1, Log 2 2
--                     , PolyLog 2 1
--                     , Exp 2 
--                     ]
--   , _cvIters    = 100
--   , _cvTrain    = 0.7
--   , _topModels  = 1
--   , _statsFilt  = defaultStatsFilt           -- See 'defaultStatsFilt'.                                                            
--   , _statsSort  = defaultStatsSort           -- See 'defaultStatsSort'.                                 
--   , _improv     = defaultImprov              -- See 'defaultImprov'.                                                    
--   , _graphFP    = Just "./AutoBenched.png"   -- This is always a .png.
--   , _reportFP   = Nothing                    -- This is a .txt.            
--   , _coordsFP   = Nothing                    -- This is a .csv.
--   }
-- @
-- 
data AnalOpts = 
  AnalOpts
    { 
    -- Models to fit:
      _linearModels  :: [LinearType]                                     -- ^ Models for linear regression analysis.

    -- Cross-validation:
    , _cvIters       :: Int                                              -- ^ Number of cross-validation iterations.
    , _cvTrain       :: Double                                           -- ^ Percentage of data set to use for cross-validation 
                                                                         --   training. The rest is used for validation.
    -- Model comparison:
    , _topModels     :: Int                                              -- ^ The top @n@ ranked models to review when selecting the best fitting model.
    , _statsFilt     :: Stats -> Bool                                    -- ^ Function to discard models that \"do not\" fit a given data set.
    , _statsSort     :: Stats -> Stats -> Ordering                       -- ^ Function to rank models according to how they fit a given data set.
    
    -- Calculating efficiency results:
    , _improv        :: [(Double, Double)] -> Maybe (Ordering, Double)   -- ^ Function to calculate improvement results by comparing the runtimes 
                                                                         --   of two test programs pointwise.
    -- Results generated by the system:
    , _graphFP       :: Maybe FilePath                                   -- ^ PNG graph of runtime results.
    , _reportFP      :: Maybe FilePath                                   -- ^ TXT report of results.
    , _coordsFP      :: Maybe FilePath                                   -- ^ CSV of (input size(s), runtime) coordinates.
    } deriving (Generic)

instance NFData AnalOpts 

instance Default AnalOpts where
  def = AnalOpts
          {
            _linearModels = fmap Poly [0..4] ++ [Log 2 1, Log 2 2, PolyLog 2 1, Exp 2]
          , _cvIters      = 200
          , _cvTrain      = 0.7
          , _topModels    = 1
          , _statsFilt    = defaultStatsFilt                                                                   
          , _statsSort    = defaultStatsSort                                                                    
          , _improv       = defaultImprov                                                              
          , _graphFP      = Just "./AutoBenched.png"  
          , _reportFP     = Nothing                   
          , _coordsFP     = Nothing
          }

-- | The default method for discarding models that \"do not\" fit a given
-- data set. This is achieved by filtering associated fitting 'Stats'. 
--
-- For the default option we discard any model whose R^2/adjusted R^2 value 
-- is outside of the range [0..1], as this is a strong indicator that the 
-- model is a \"bad fit\" for the data.
--
-- Of course, users may disagree with this in which case they can set their 
-- own predicate in their 'TestOpts', e.g., maybe change it to @const True@.
defaultStatsFilt :: Stats -> Bool                                                  
defaultStatsFilt s = _r2 s >= 0 && _r2 s <= 1 && _a_r2 s >= 0 && _a_r2 s <= 1

-- In general we want the model with the /lowest/ predicted mean squared error 
-- '_p_mse'. However, through testing we have found that in the case where two 
-- models have '_p_mse' with relative error <= 0.15, it seems appropriate to 
-- pick the one with the /highest predicted/ R^2 value '_p_r2'.
--
-- Of course, users may disagree with this in which case they can set their 
-- own ordering in their 'TestOpts'. In order to be facilitate different 
-- methods for ordering 'Stats', we have included a range of best-fit 
-- indicators, see 'Stats' for more information.
defaultStatsSort :: Stats -> Stats -> Ordering 
defaultStatsSort s1 s2                      -- Note: we want the highest predicted R^2.
  -- | relativeError p_mse_1 p_mse_2 <= 0.15 = compare (_p_r2 s2) (_p_r2 s1)
  = compare p_mse_1 p_mse_2        
  where 
    p_mse_1 = _p_mse s1 
    p_mse_2 = _p_mse s2

-- | The default way to generate improvement results by comparing the runtimes
-- of two test programs /pointwise/.
-- 
-- It works as follows:
--
-- * First the relative error of each pair of runtimes is calculated,
--   if 95% or more pairs have relative error <= 0.15, then the system 
--   concludes the test programs are cost-equivalent (i.e., have roughly the 
--   same runtime efficiency).
-- * If not, the system 'compare's each pair of runtimes, e.g., @(d1, d2) 
--   ===> d1 `compare` d2@. If 95% of pairs are @LT@ or @GT@, then that 
--   ordering is chosen.
-- * If not, /no/ improvement result is generated.
defaultImprov :: [(Double, Double)] -> Maybe (Ordering, Double)
defaultImprov ds 
  | eqsPct >= 0.95 = Just (EQ, eqsPct) -- 95% or more test cases have relative error <= 0.15?
  | ltsPct >= 0.95 = Just (LT, ltsPct) -- 95% or more test cases 'LT'?
  | gtsPct >= 0.95 = Just (GT, gtsPct) -- 95% or more test cases 'GT'?
  | otherwise      = Nothing           -- No improvement result.

  where 
    -- Calculate total for EQ.
    eqs = filter (<= 0.15) $ fmap (uncurry relativeError) ds
    -- For each pair of runtime measurements (d1, d2), calculate d1 `compare` d2.
    -- Then calculate total for LT and GT.
    (lts, gts) = foldr f (0, 0) $ fmap (uncurry compare) ds
  
    -- Totalling function for LT and GT.
    f :: Ordering -> (Int, Int) -> (Int, Int)
    f EQ (lt, gt) = (lt    , gt)
    f LT (lt, gt) = (lt + 1, gt)
    f GT (lt, gt) = (lt    , gt + 1)

    -- Percentages for EQ, LT, GT.
    eqsPct = genericLength eqs / genericLength ds
    ltsPct = (fromIntegral lts / genericLength ds) :: Double 
    gtsPct = (fromIntegral gts / genericLength ds) :: Double












-- * Statistical analysis

-- | The system approximates the time complexity of test programs by 
-- measuring their runtimes on test data of increasing size. Input sizes 
-- and runtime measurements are then given as (x, y)-coordinates
-- (x = size, y = runtime). Regression analysis (ridge regression) is used to 
-- fit various models (i.e., different types of functions: constant, linear, 
-- quadratic etc.) to the (x, y)-coordinates. Models are then compared to 
-- determine which has the best fit. The equation of the best fitting model is 
-- used as an approximation of time complexity. 
--
-- The 'LinearType' datatype describes linear functions that are used as 
-- models. The system currently supports the following types of functions:
--
-- * Poly 0 (constant)   :=   a_0 
-- * Poly 1 (linear)     :=   a_0 + a_1 * x^2      
-- * Poly n              :=   a_0 + a_1 * x^1 + a_2 * x^2 + .. + a_n * x^n 
-- * Log  b n            :=   a_0 + a_1 * log_b^1(x) + a_2 * log_b^2(x) + .. + a_n * log_b^n(x)
-- * PolyLog b n         :=   a_0 + a_1 * x^1 * log_b^1(x) + a_2 * x^2 * log_b^2(x) + .. + a_n * x^n * log_b^n(x) 
-- * Exp n               :=   a_0 + n^x
data LinearType = 
    Poly    Int        -- ^ Polynomial functions (Poly 0 = constant, Poly 1 = linear).
  | Log     Int Int    -- ^ Logarithmic functions.
  | PolyLog Int Int    -- ^ Polylogarithmic functions.     
  | Exp     Int        -- ^ Exponential functions.
    deriving (Eq, Generic)

instance NFData LinearType

instance Show LinearType where 
  show (Poly      0) = "constant"
  show (Poly      1) = "linear"
  show (Poly      2) = "quadratic"
  show (Poly      3) = "cubic"
  show (Poly      4) = "quartic"
  show (Poly      5) = "quintic"
  show (Poly      6) = "sextic"
  show (Poly      7) = "septic"
  show (Poly      8) = "octic"
  show (Poly      9) = "nonic"
  show (Poly      n) = "n" ++ superNum n
  show (Log     b n) = "log" ++ subNum b ++ superNum n ++ "n"
  show (PolyLog b n) = "n" ++ superNum n ++ "log" ++ subNum b ++ superNum n ++ "n"
  show (Exp       n) = show n ++ "\x207F"

-- | The system provides a number of fitting statistics that can be used to 
-- compare models when deciding which model best fits a given data set.
-- Users can provide their own functions in their 'AnalOpts' to filter and 
-- compare models according to these 'Stats': see '_statsFilt' and _statsSort'.
-- Default sorting and comparison functions are 'defaultStatsFilt' and 
-- 'defaultStatsSort', respectively.
data Stats = 
  Stats 
   {
     _p_mse    :: Double   -- ^ Predicted mean squared error.
   , _p_mae    :: Double   -- ^ Predicated mean absolute error.
   , _ss_tot   :: Double   -- ^ Total sum of squares.
   , _p_ss_res :: Double   -- ^ Predicted residual sum of squares.
   , _r2       :: Double   -- ^ Coefficient of Determination.
   , _a_r2     :: Double   -- ^ Adjusted Coefficient of Determination.
   , _p_r2     :: Double   -- ^ Predicted Coefficient of Determination.
   , _bic      :: Double   -- ^ Bayesian Information Criterion.
   , _aic      :: Double   -- ^ Akaike’s Information Criterion.
   , _cp       :: Double   -- ^ Mallows' Cp Statistic.
   } deriving Eq

instance Show Stats where 
  show sts = flip bySide " " $ fmap PP.vcat $ transpose  
    [ [ PP.text "PMSE",          PP.char '=', PP.text $ printf "%.4g" (_p_mse    sts) ] 
    , [ PP.text "PMAE",          PP.char '=', PP.text $ printf "%.4g" (_p_mae    sts) ]
    , [ PP.text "SST",           PP.char '=', PP.text $ printf "%.4g" (_ss_tot   sts) ]
    , [ PP.text "PRESS",         PP.char '=', PP.text $ printf "%.4g" (_p_ss_res sts) ]
    , [ PP.text "R\x00B2",       PP.char '=', PP.text $ printf "%.4g" (_r2       sts) ]
    , [ PP.text "Adj. R\x00B2",  PP.char '=', PP.text $ printf "%.4g" (_a_r2     sts) ]
    , [ PP.text "Pred. R\x00B2", PP.char '=', PP.text $ printf "%.4g" (_p_r2     sts) ]
    , [ PP.text "BIC",           PP.char '=', PP.text $ printf "%.4g" (_bic      sts) ]
    , [ PP.text "AIC",           PP.char '=', PP.text $ printf "%.4g" (_aic      sts) ]
    , [ PP.text "CP",            PP.char '=', PP.text $ printf "%.4g" (_cp       sts) ]
    ]