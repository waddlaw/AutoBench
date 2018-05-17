
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

import           Criterion.Types    (OutlierEffect)
import qualified Criterion.Types as Criterion
import qualified Criterion.Main  as Criterion
import           Data.Default       (Default(..))

-- AutoBench
import AutoBench.AbstractSyntax (Id)



-- * User input

-- ** Test suites

-- | Test suites are AutoBench's principle user input datatype, and are used to 
-- structure performance tests into logical units that can be checked, verified, 
-- and ultimately executed independently. 
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
-- such, the system provides sensible defaults,
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
-- suite is to ensure that each of its record fields are initialised. Please
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

-- ** Configuration

data ABCfg = ABCfg{}
  
-- ** Data options 




-- | For a unary test program @p :: a -> b@, each manual input is of type 
-- @(Int, IO a)@. 
-- In addition, each input /must/ incorporate a sensible notion of size, which 
-- is given as the first element of each @(Int, IO a)@ tuple. For example, in 
-- the case of a unary test program with @[Char]@ input type, we may provide 
-- manual test data @tDat@ as such:
--
-- @ tDat :: UnaryTestData [Char]
-- dat  = 
--   [ ( 5
--     , return [/'a/', /'b/', /'c/', /'d/', /'e/'] )
--   , ( 10 
--     , return [/'a/', /'b/', /'c/', /'d/', /'e/', /'f/', /'g/', /'h/', /'i/', /'j/'] )
--   ... ]@
-- 
-- Here the size of a list is determined by its number of elements (@5@ and @10@ 
-- respectively).
--
-- N.B. The @IO@ type is required by 'Criterion' for benchmarking purposes (see 
-- 'Criterion.Main.env'). Fittingly, it also allows users to produce test data 
-- via some impure action if appropriate.
--
-- 'TestSuite's require a minimum number of /distinctly sized/ test inputs: see 
-- 'minInputs'.
-- 
-- __Important__: Each test datatype must incorporate a /sensible/ notion of 
-- size. For manual test data, the size of each input is specified 
-- manually, and therefore, this value must be carefully considered.
-- __**Incorrectly sized test data will lead to erroneous performance results**__.
type UnaryTestData a = [(Int, IO a)]  

-- | /Sized/ test data can either be specified manually by users or generated 
-- automatically by the system. 
--
-- If given manually, then the 'Manual' data option simply tells the system 
-- which test data in the user input file to use. See 'UnaryTestData' and 
-- 'BinaryTestData' for details regarding the /types/ of manual test data. 
--
-- In the case where inputs are to be generated by the system, users must 
-- specify the /size/ of each input to be generated. The input types of test 
-- programs (which /must/ be the same) determine the /type/ of the generated 
-- data. For example, for a test program @p :: [Int] -> [Int]@, the @Gen@ option 
-- would instruct the system to generate lists of integers of different sizes. 
-- In this instance, the size of each list is determined by @Gen l s u@, which
-- specifies a size /range/ by a lower bound @l@, an upper bound @u@, and a step 
-- @s@. This is converted to the Haskell range @[l, (l + s) .. u]@ and a 
-- list of integers is generated for each size in this range. For example, 
-- @Gen 5 5 100@ corresponds to the range @[5, 10 .. 100]@. 
--
-- 'TestSuite's require a minimum number of /distinctly sized/ inputs: see 
-- 'minInputs'.
--
-- __Important__: Each test datatype must incorporate a /sensible/ notion of 
-- size, which must be codified in its corresponding 'Arbitrary' instance in
-- order to use the @Gen@ option correctly. For example, in the case of lists a 
-- natural way of encoding size is number of elements. 
-- __**Incorrectly sized test data will lead to erroneous performance results**__.
data DataOpts = 
    Manual String       -- ^ The system should search for manually specified test data
                        --   with the corrresponding identifier in the user input file.
  | Gen Int Int Int     -- ^ The system should generate random test data in the given size range.
 -- | Discover          -- ^ <TO-DO>: The system should discover compatible 'Manual' data, or 
                        -- accept a suitable 'Gen' setting at a later time. 
    deriving Show

instance Default DataOpts where 
  def = Gen 5 5 100










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




