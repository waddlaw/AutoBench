
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wall   #-}

{-|

  Module      : AutoBench.Types
  Description : Datatypes and associated helper functions\/defaults.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  Datatypes used throughout AutoBench's implementation and any associated 
  helper functions\/defaults.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 'DataOpts' Discover setting;
   - 
-}





module AutoBench.Types 
  (


  ) where

import           Control.Exception.Base (Exception)
import           Criterion.Types        (OutlierEffect)
import qualified Criterion.Types        as Criterion
import qualified Criterion.Main         as Criterion
import           Data.Default           (Default(..))


-- AutoBench
import AutoBench.AbstractSyntax (Id)



-- * User input

-- ** Test suites

-- | Test suites are AutoBench's principle user input datatype, and are used to 
-- structure performance tests into logical units that can be checked, verified, 
-- and executed independently. 
--
-- An advantage of this approach is that users can group multiple test 
-- suites in the same file according to some testing context, whether it be 
-- analysing the performance of the same programs subject to different levels 
-- of optimisation, or comparing different implementations under the same
-- test conditions. Another advantage is that if one or more test suites in an 
-- input file are erroneous, other, valid test suites in the same file can be 
-- executed nonetheless.
--
-- Test suites contain a significant number of user options and settings. As 
-- such, the system provides the following defaults,
--
-- @ TestSuite
--     { _progs    = []                                     -- All programs in the test file will be considered for test purposes.
--     , _dataOpts = def                                    -- See 'DataOpts'.
--     , _analOpts = def                                    -- See 'AnalOpts'.
--     , _critCfg  = Criterion.Main.Options.defaultConfig
--     , _baseline = False
--     , _nf       = True
--     , _ghcFlags = []                                     -- /No/ optimisation, i.e., -O0.
--     }
-- @
--
-- that users can override.
--
-- Important note: the most basic check that the system performs on every test
-- suite is to ensure that each of its record fields are initialised: please
-- ensure test suites are fully defined.
data TestSuite = 
  TestSuite
    {  _progs    :: [Id]             -- ^ Identifiers of programs in the input file to test: note all programs
                                     --   in the file will be considered if this list is empty.
    , _dataOpts :: DataOpts          -- ^ Test data options ('DataOpts').
    , _analOpts :: AnalOpts          -- ^ Statistical analysis options ('AnalOpts').
    , _critCfg  :: Criterion.Config  -- ^ Criterion's configuration ('Criterion.Types.Config').
    , _baseline :: Bool              -- ^ Whether the graphs of runtime results should include baseline measurements.
    , _nf       :: Bool              -- ^ Whether test cases should be evaluated to nf (@True@) or whnf (@False@).
    , _ghcFlags :: [String]          -- ^ GHC compiler flags used when compiling 'BenchSuite's.
    }

instance Default TestSuite where 
  def = TestSuite
          { _progs    = []                         -- All programs in the test file will be considered for test purposes.           
          , _dataOpts = def                        -- See 'DataOpts'. 
          , _analOpts = def                        -- See 'AnalOpts'.        
          , _critCfg  = Criterion.defaultConfig    -- See 'Criterion.Main.Options.defaultConfig'
          , _baseline = False              
          , _nf       = True
          , _ghcFlags = []                         -- /No/ optimisation, i.e., -O0. 
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
-- the system. Note: the default setting for 'DataOpts' is @Gen 5 5 100@.
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
-- tOpts :: TestOpts 
-- tOpts  = def { _progs = ["tProg"], _dataOpts = Manual "tDat" }
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
-- For example: @Gen 5 5 100@ corresponds to the range @[5, 10, 15 .. 100]@. 
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
    deriving Eq

instance Default DataOpts where 
  def = Gen 5 5 100

-- | Each test suite requires a minimum number of distinctly sized test inputs.
-- 
-- > minInputs = 20
minInputs :: Int 
minInputs  = 20










-- * Benchmarking

data BenchSuite = BenchSuite{}

-- * Statistical analysis

-- | Statistical analysis options.
data AnalOpts = AnalOpts{}

instance Default AnalOpts where
  def = undefined

   {-
     -- Models to fit:
     models  :: [LinearClass]                                           -- ^ Candidates for linear regression analysis.

     -- Cross-validation:
   , cvIters :: Int                                                     -- ^ Number of cross-validation iterations.
   , cvTrain :: Double                                                  -- ^ Percentage of data set to use for cross-validation 
                                                                        --   training; the rest is used for validation.
     -- Model comparison:
   , fFiltStats :: Stats -> Bool                                        -- ^ Function to discard models that \"do not\" fit a given data set.
   , fSortStats :: Stats -> Stats -> Ordering                           -- ^ Function to select a model that \"best fits\" a given data set.

     -- Improvement options:
   , fRuntimeComp :: Double -> Double -> Ordering                       -- ^ Function to compare runtimes of test programs.
   , fImprov      :: [Ordering] -> Maybe (Ordering, Double)             -- ^ Function to calculate an improvement ordering from ordered runtimes.
     
     -- Results generated by the system:
   , graphFP  :: Maybe FilePath                                         -- ^ Graph of runtime results.
   , repFP    :: Maybe FilePath                                         -- ^ Report of results.
   , coordsFP :: Maybe FilePath                                         -- ^ CSV of (input size(s), runtime) coordinates.
   }
   -}



-- * Errors 

-- ** Input errors

-- | Errors relating to user inputs, whether it be erroneous definitions 
-- in user input files or filepath/file access errors. In general, the system 
-- will attempt to continue with its execution for as long as possible. 
-- Therefore, unless a critical error is encountered, such as a filepath or 
-- file access error, the system will collate all user input errors, and then
-- summarise them after the user input file has been fully analysed.
data InputError = 
    FilePathErr String   -- ^ Invalid filepath.
  | FileErr     String   -- ^ File access error.
  | TestOptsErr String   -- ^ Invalid test options.
  | DataOptsErr String   -- ^ Invalid data options.
  | AnalOptsErr String   -- ^ Invalid statistical analysis options.
  | InstanceErr String   -- ^ One or more missing instance declarations.

instance Show InputError where 
  show (FilePathErr s) = "File path error: "        ++ s
  show (FileErr     s) = "File error: "             ++ s
  show (TestOptsErr s) = "Test options error: "     ++ s
  show (DataOptsErr s) = "Test data error: "        ++ s
  show (AnalOptsErr s) = "Analysis options error: " ++ s
  show (InstanceErr s) = "Instance error: "         ++ s

instance Exception InputError