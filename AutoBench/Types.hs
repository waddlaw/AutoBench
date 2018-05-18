
{-# OPTIONS_GHC -Wall #-}

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

  -- * User inputs
  -- ** Test suites
    TestSuite(..)          -- Test suites are AutoBench's principle user input datatype.
  -- ** Test data options
  , UnaryTestData          -- User-specified test data for unary test programs.
  , BinaryTestData         -- User-specified test data for binary test programs.
  , DataOpts(..)           -- Test data options.
  , minInputs              -- Minimum number of distinctly sized test inputs.
  -- ** Statistical analysis options
  , AnalOpts(..)           -- Statistical analysis options.
  -- ** Internal representation of user inputs
  , UserInputs(..)         -- A data structure maintained by the system to classify user inputs.
  , initUserInputs         -- Initialise a 'UserInputs' data structure.
  -- * Benchmarking
  , BenchSuite(..)         -- Benchmarking suites are AutoBench's principle benchmarking datatype.
  -- * Statistical analysis
  -- * Errors
  -- ** Input errors
  , InputError(..)         -- User input errors.

  ) where

import           Control.Exception.Base (Exception)
import qualified Criterion.Types        as Criterion
import qualified Criterion.Main         as Criterion
import           Data.Default           (Default(..))

import AutoBench.AbstractSyntax (HsType, Id, ModuleElem, TypeString)

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
--     , _critCfg  = Criterion.Main.Options.defaultConfig   -- See 'Criterion.Main.Options.defaultConfig'
--     , _baseline = False                                  -- No baseline measurements.
--     , _nf       = True                                   -- Evaluate test cases to normal form.
--     , _ghcFlags = []                                     --/No optimisation, i.e., -O0.
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

-- ** Statistical analysis options

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

-- ** Internal representation of user inputs

-- | While user inputs are being analysed by the system, a 'UserInputs' data
-- structure is maintained. The purpose of this data structure is to classify 
-- user inputs according to the properties they satisfy. For example, when the 
-- system first interprets a user input file, all of its definitions are added 
-- to the '_allElems' list. This list is then processed to determine which 
-- definitions have function types that are syntactically compatible with the 
-- requirements of the system (see 'AutoBench.AbstractSyntax'). Definitions that 
-- are compatible are added to the '_validElems' list, and those that aren't are 
-- added to the '_invalidElems' list. Elements in the '_validElems' list are
-- then classified according to, for example, whether they are nullary, 
-- unary, or binary functions. This process continues until all user inputs are 
-- classified according to the list headers below.
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
   , _invalidElems       :: [(ModuleElem, Maybe TypeString)]         -- ^ Syntactically invalid definitions (see 'AutoBench.AbstractSyntax').
   , _validElems         :: [(Id, HsType)]                           -- ^ Syntactically valid definitions (see 'AutoBench.AbstractSyntax').
   , _nullaryFuns        :: [(Id, HsType)]                           -- ^ Nullary function definitions.
   , _unaryFuns          :: [(Id, HsType)]                           -- ^ Unary function definitions.
   , _binaryFuns         :: [(Id, HsType)]                           -- ^ Binary function definitions.
   , _arbFuns            :: [(Id, HsType)]                           -- ^ Unary/binary function definitions whose input types are members of the Arbitrary type class.
   , _nfFuns             :: [(Id, HsType)]                           -- ^ Unary/binary function definitions whose input types are members of the NFData type class.
   , _invalidData        :: [(Id, HsType, [InputError])]             -- ^ Invalid user-specified test data. 
   , _unaryData          :: [(Id, HsType)]                           -- ^ Valid user-specified test data for unary function definitions.
   , _binaryData         :: [(Id, HsType)]                           -- ^ Valid user-specified test data for binary function definitions.
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
    , _nfFuns            = []
    , _invalidData       = []
    , _unaryData         = []
    , _binaryData        = []
    , _invalidTestSuites = []
    , _testSuites        = []
    }

-- * Benchmarking

data BenchSuite = BenchSuite{}

-- * Statistical analysis

-- * Errors 

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
    FilePathErr String   -- ^ Invalid filepath.
  | FileErr     String   -- ^ File access error.
  | TestOptsErr String   -- ^ Invalid test options.
  | DataOptsErr String   -- ^ Invalid data options.
  | AnalOptsErr String   -- ^ Invalid statistical analysis options.
  | TypeErr     String   -- ^ Invalid type signature.
  | InstanceErr String   -- ^ One or more missing instances

instance Show InputError where 
  show (FilePathErr s) = "File path error: "        ++ s
  show (FileErr     s) = "File error: "             ++ s
  show (TestOptsErr s) = "Test options error: "     ++ s
  show (DataOptsErr s) = "Test data error: "        ++ s
  show (AnalOptsErr s) = "Analysis options error: " ++ s
  show (TypeErr     s) = "Type error: "             ++ s
  show (InstanceErr s) = "Instance error: "         ++ s

instance Exception InputError