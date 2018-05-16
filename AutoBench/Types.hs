
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wall   #-}

{-|

  Module      : AutoBench.Types
  Description : Types/data types
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module defines types/data types used throughout AutoBench and any 
  associated helper functions\/defaults.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   -
-}

module AutoBench.Types 
  (
     -- * Abstract syntax
     -- | In order to validate user input files, the system uses an abstract 
     -- representation of Haskell 98.
      parseTySig          -- Parse a string representation of a type signature to an abstract qualified type representation.
    , tyFunInps           -- Extract the input types from unary/binary function types.
    , unqualTyToTy        -- Convert an unqualified 'HsQualType' to a 'HsType'
     -- ** Syntactic checks
     -- | A number of syntactic checks are performed on the abstract
     -- representations of types to assess whether types meet the requirements 
     -- of the system.
    , hasTyVars           -- Check whether a 'HsType' contains type variables.
    , isNullaryTyFun      -- Is a 'HsType' a nullary function type?
    , isUnaryTyFun        -- Is a 'HsType' a unary function type? 
    , isBinaryTyFun       -- Is a 'HsType' a binary function type?
    , isUnqualQualTy      -- Does a 'HsQualType' meet the /unqualified/ syntactic type requirements of AutoBench?
    , isABTyFun           -- Does a 'HsType' meet the syntactic type requirements of AutoBench?
    , isABTestTyFun       -- Does a 'HsType' meet the /testable/ syntactic type requirements of AutoBench?
    , isABGenTyFun        -- Does a 'HsType' meet the /genable/ syntactic type requirements of AutoBench?




  ) where 

-- Abstract syntax 
--import Language.Haskell.Pretty (prettyPrint)
import Language.Haskell.Syntax 
  ( HsDecl(..)
  , HsModule(..)
  , HsQualType(..)
  , HsType(..)
  )

-- Parsing 
import Language.Haskell.Parser (ParseResult(..), parseModule)



-- * Abstract syntax 

-- | Parse a string representation of a type signature to an abstract 
-- qualified type representation if possible.
--
-- > prettyPrint <$> parseTypeSig "foo :: Int -> Int" = Just "Int -> Int"
parseTySig :: String -> Maybe HsQualType
parseTySig s = case parseModule s of 
  ParseOk (HsModule _ _ _ _ [HsTypeSig _ _ qTy]) -> Just qTy
  _ -> Nothing 

-- | Convert an unqualified 'HsQualType' to a 'HsType' by removing its context.
--
-- Warning: assumes the context is empty.
unqualTyToTy :: HsQualType -> HsType
unqualTyToTy (HsQualType _ ty) = ty 

-- | Extract the input types from a /unary/ or /binary/ function type. Return 
-- them as a 'HsTyTuple'.
--
-- Examples in pseudocode:
-- 
-- * Int -> Int               ===> (Int)              -- unary
-- * Int -> String -> Int     ===> (Int, String)      -- binary
-- * Int -> Int -> Int -> Int ===> ()                 -- not unary/binary 
-- * Int                      ===> ()                 -- not unary/binary 
tyFunInps:: HsType -> HsType
tyFunInps (HsTyFun t1 (HsTyFun t2 _)) = HsTyTuple [t1, t2] 
tyFunInps (HsTyFun t _) = HsTyTuple [t]
tyFunInps _ = HsTyTuple []

-- ** Syntactic checks 

-- | Check whether a 'HsType' contains one or more type variables.
hasTyVars :: HsType -> Bool 
hasTyVars HsTyVar{} = True
hasTyVars HsTyCon{} = False
hasTyVars (HsTyTuple ts)  = any hasTyVars ts
hasTyVars (HsTyFun t1 t2) = hasTyVars t1 || hasTyVars t2
hasTyVars (HsTyApp t1 t2) = hasTyVars t1 || hasTyVars t2

-- | Check whether a 'HsType' ia a nullary function type.
isNullaryTyFun :: HsType -> Bool
isNullaryTyFun HsTyFun{} = False
isNullaryTyFun _         = True

-- | Check whether a 'HsType' ia a unary function type.
isUnaryTyFun :: HsType -> Bool 
isUnaryTyFun (HsTyFun _ HsTyFun{}) = False 
isUnaryTyFun HsTyFun{} = True
isUnaryTyFun _ = False

-- | Check whether a 'HsType' ia a binary function type.
isBinaryTyFun :: HsType -> Bool 
isBinaryTyFun (HsTyFun _ (HsTyFun _ HsTyFun{})) = False
isBinaryTyFun (HsTyFun _ HsTyFun{}) = True 
isBinaryTyFun _ = False 

-- | Check whether a 'HsQualType' meets the /unqualified/ syntactic type 
-- requirements of AutoBench, i.e., has an empty context.
isUnqualQualTy :: HsQualType -> Bool 
isUnqualQualTy (HsQualType [] _) = True 
isUnqualQualTy _ = False

-- | Check whether a 'HsType' meets the syntactic type requirements of 
-- AutoBench, i.e., is a nullary, unary, or binary function type.
isABTyFun :: HsType -> Bool 
isABTyFun ty = isNullaryTyFun ty || isUnaryTyFun ty || isBinaryTyFun ty 

-- | Check whether a 'HsType' meets the /testable/ syntactic type 
-- requirements of AutoBench, i.e., is a unary, or binary function type.
isABTestTyFun :: HsType -> Bool 
isABTestTyFun ty = isUnaryTyFun ty || isBinaryTyFun ty 

-- | Check whether a 'HsType' meets the /genable/ syntactic type 
-- requirements of AutoBench, i.e., 'isABTestTyFun' and input types do not
-- contain type variables.
--
-- The latter requirement is because QuickCheck cannot generate /sized/ test 
-- data for polymorphic types because it defaults to (), which clearly doesn't 
-- have a /sensible/ notion of size. 
--
-- Examples in pseudocode:
--
-- * Int -> Int      ===> True 
-- * Int             ===> False     -- not 'isABTestTyFun'
-- * a -> Int -> Int ===> False     -- containts type variable 'a'
isABGenTyFun :: HsType -> Bool 
isABGenTyFun ty = isABTestTyFun ty && noTyVars (tyFunInps ty) 
  where 
    noTyVars (HsTyTuple [t])      = not (hasTyVars t)
    noTyVars (HsTyTuple [t1, t2]) = not (hasTyVars t1 || hasTyVars t2)
    noTyVars _ = False -- shouldn't happen






{-
module AutoBench.Types 
  (
    -- * Data types
    DataSize(..)              -- The size of test data for test programs.
  , InputType(..)             -- The input type of a testable program.
  , Type                      -- The name of a type.

    -- * Test inputs\/options\/results
  , TestDataUn                -- Manual test data for unary test programs.
  , TestDataBin               -- Manual test data for binary test programs.
  , DataOpts(..)              -- Test data options.
  , TestOpts(..)              -- Test options.
  
  , TestSuite(..)             -- Test suite generated from user input/test file.
  , Coord                     -- (Input Size, Runtime) coordinates for unary test programs.
  , Coord3                    -- (Input Size, Input Size, Runtime) coordinates for binary test programs.
  , SimpleReport(..)          -- Simplified version of Criterion's benchmarking report.

  -- ** Input errors
  , InputError(..)            -- Errors in user input/test file.

  -- ** Console output settings
  , Verbosity(..)             -- Verbosity level for printing messages to console.
  , HasVerbosity(..)          -- Configurations that have verbosity information.

  -- ** Linear regression
  , AnalOpts(..)              -- Statistical analysis options.
  , Exp                       -- Expressions with 'Double' literals.            
  , LinearClass(..)           -- Classes of linear functions for regression analysis.
  , LinearCandidate(..)       -- Each 'LinearClass' gives rise to a 'LinearCandidate' that is then fit to a given data set generating a 'LinearFit'.
  , LinearFit(..)             -- A linear function that fits a data set with a fitting error 'err'.
  , CVStats(..)               -- Statistics calculated by cross-validation.
  , Stats(..)                 -- Statistics used to compare 'LinearFit's.

  -- * Defaults
  -- | If no test options are specified by the user in their input/test file, 
  -- the following defaults will be used by the system.
  , defaultAnalOpts             -- Default statistical analysis options.
  , defaultDataOpts             -- Default data options.
  , defaultFilterStats          -- Default method for filter models that "do not" fit a data set.
  , defaultImprovementOrdering  -- Default method for calculating an improvement ordering from ordered runtimes.
  , defaultRuntimeOrdering      -- Default method for comparing runtime results of test programs.
  , defaultSortStats            -- Default method for picking a model that "best fits" a data set.
  , defaultTestOpts             -- Default test options.

  -- * Helpers
  , displayTestSuite          -- Display a 'TestSuite' as a document.
  , displayInvalidTestSuites  -- Display invalid test suites as a document.
  , displayValidTestSuites    -- Display valid test suites as a document.
  , maxPredictors             -- Maximum number of predictors for models.
  , minInputs                 -- Minimum number of inputs (i.e., test cases) for each test suite.
  , numPredictors             -- Number of predictors for each model.
  , simpleReportCoords        -- Extract the 'Coord's from a list of 'SimpleReport's.
  , simpleReportCoords3       -- Extract the 'Coord3's from a list of 'SimpleReport's.
  , toHRange                  -- Convert Gen l s u :: DataOpts to a Haskell range.

  -- ** Parsing helpers 
  , parseDataSize             -- Parse 'DataSize' from a string.


  -- ** NEW ************
  , parseTypeSig
  , isValidGenType
  , isNullaryFunType
  , isUnaryFunType
  , isBinaryFunType
  , tyFunToTyInp
  , HsTypeInp(..)
  , UsrFun(..)
  , usrFunIdt
  , usrFunType
  , usrFunHsType

  , UsrInps(..)
  , TestSuiteOpts(..)
  , TestConfig(..)
  , initUsrInps
  , defaultTestSuiteOpts

  ) where


-- NEW

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Interpreter (ModuleElem)


-- NEW 


import           Control.Arrow                    (first)
import           Control.Exception.Base           (Exception)
import           Control.Monad                    (void)
import           Criterion.Main                   (defaultConfig)
import           Criterion.Types                  (Config, OutlierEffect)
import           Data.Maybe                       (catMaybes)
import           Language.Haskell.Interpreter     (Id) 
import           Numeric.LinearAlgebra            (Vector)
import           Numeric.MathFunctions.Comparison (relativeError)
import qualified TimeCheck.Expr                   as E
import qualified Text.Megaparsec                  as MP

import Text.PrettyPrint.HughesPJ 
  ( 
    Doc
  , (<>)
  , (<+>)
  , ($$)
  , char
  , hcat
  , int
  , nest 
  , punctuate
  , text
  , vcat
  )

import TimeCheck.Utils (Parser, baseShow, integer, powShow, symbol)

-- * Data types/instances.

-- | The name of a type. When generating 'TestSuite's we must ensure that each
-- test program has the same type. We must also ensure that the type of manual 
-- test data (i.e., that which is specified by the user: 
-- 'TestDataUn' and 'TestDataBin') is compatible with the input types of test 
-- programs. As such, we must analyse and compare types frequently.
type Type = String

-- | The input type of a (testable) program.
data InputType = 
    InpTyUn Type       -- ^ The input type of a unary (test) program.
  | InpTyBin Type Type -- ^ The input type of a binary (test) program.
    deriving (Show, Eq)

-- | The size of test data for unary and binary test programs.
data DataSize = 
    SizeUn Int       -- ^ The size of test data for unary test programs.
  | SizeBin Int Int  -- ^ The size of test data for binary test programs.

instance Show DataSize where 
  show (SizeUn n)      = show n 
  show (SizeBin n1 n2) = show (n1, n2)

-- ** Test inputs\/options\/results.

-- | Manually specified test data for /unary/ test programs.
--
-- For a unary test program @P :: a -> b@, manual test data is of type @IO a@ 
-- and must incorporate a sensible notion of size. For example, in the case of 
-- a test program with @[Char]@ input type, we may provide manual test data 
-- @dat@ as such:
--
-- @ dat :: TestDataUn [Char]
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
-- N.B. The @IO@ type is required by Criterion for benchmarking (see 'env' in 
-- @Criterion.Main@). Of course we could use type @a@ and lift it into the @IO@ 
-- monad ourselves, however it seems useful to expose this monadic type to 
-- allow users to produce test data via some impure action if appropriate.
--
-- 'TestSuite's require a minimum number of /distinctly sized/ test datums: see 
-- 'minInputs'.
-- 
-- __Important__: Each test datatype must incorporate a /sensible/ notion of 
-- size. For manual test data, the size of each test datum is specified 
-- manually, therefore, this measure must be carefully considered.
-- __**Incorrectly sized test data will lead to invalid performance results**__.
type TestDataUn a = [(Int, IO a)]  

-- | Manually specified test data for /binary/ test programs.
--
-- For a binary test program @P :: a -> b -> c@, manual test data is of type 
-- @IO a@ and @IO b@ and both types must incorporate a sensible, /distinct/ 
-- notion of size.
--
-- See 'TestDataUn' for an example of unary test data. This example generalises 
-- to binary test data in the obvious way:
-- 
-- @ dat :: TestDataBin [Char] [Int]
-- dat  = 
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
-- 'minInputs'. In the case of manual binary test data, /pairs/ of sizes must be 
-- distinct. For example, above @(5, 4)@ and @(10, 9)@ are two distinct pairs of 
-- sizes.
--
-- __Important__: Each test datatype must incorporate a /sensible/ notion of 
-- size. For manual test data, the size of each test datum is specified 
-- manually, therefore, this measure must be carefully considered.
-- __**Incorrectly sized test data will lead to invalid performance results**__.
type TestDataBin a b = [(Int, Int, IO a, IO b)]

-- | Test data options: /sized/ test data can either be specified manually by 
-- the user or generated automatically by the system. 
--
-- See 'TestDataUn' and 'TestDataBin' for details regarding manual test data. 
--
-- In the case where test data is to be generated by the system, the user must 
-- specify the /size/ of each datum to be generated. The input types of test 
-- programs (which /must/ be the same) determine the /type/ of the generated 
-- data. For example, for a test program @P :: [Int] -> [Int]@, the @Gen@ option 
-- would instruct the system to generate lists of integers of different sizes. 
-- In this instance, the size of each list is determined by @Gen l s u@, which
-- specifies a size /range/ by a lower bound @l@, an upper bound @u@, and a step 
-- @s@. This is converted to the Haskell range @[l, (l + s) .. u]@ and a 
-- /random/ list of integers is generated for each size in this range. For 
-- example, @Gen 5 5 100@ corresponds to the range @[5, 10 .. 100]@. 
--
-- 'TestSuite's require a minimum number of /distinctly sized/ test datums: see 
-- 'minInputs'.
--
-- __Important__: Each test datatype must incorporate a /sensible/ notion of 
-- size, which must be codified in its corresponding 'Arbitrary' instance in
-- order to use the @Gen@ option correctly. For example, in the case of lists a 
-- natural way of encoding size is number of elements. 
-- __**Incorrectly sized test data will lead to invalid performance results**__.
data DataOpts = 
    Manual            -- ^ The system should search for manually specified test 
                      -- data in the user input/test file.
  | Gen Int Int Int   -- ^ The system should generate test data in the given size range.
    deriving Eq

instance Show DataOpts where 
  show Manual = "<manual>"
  show (Gen l s u) =
    "[" ++ show l 
        ++ ", " 
        ++ show (l + s) 
        ++ " .. " 
        ++ show u 
        ++ "]"

-- | Various test options control the functionality of the system. These test
-- options should be specified in the user input/test file. If none are 
-- given, the 'defaultTestOpts' are used.
data TestOpts = 
  TestOpts 
   { 
     -- Test inputs:
     progs     :: [Id]             -- ^ The names of the programs to test: all testable programs in the 
                                   --   user input/test file will be considered if this list is empty.
   , dat       :: DataOpts         -- ^ Test data options.

     -- Runtime measurement options:
   , nfRes     :: Bool             -- ^ Whether the results of test programs should be evaluated to nf (@True@) or whnf (@False@).
   , subNfRes  :: Bool             -- ^ NOTE: FEATURE CURRENTLY DISABLED

     -- Statistical analysis options:
   , analOpts  :: AnalOpts         -- ^ Statistical analysis options.

     -- Options passed to subsystems:
   , ghcFlags  :: [String]         -- ^ GHC compiler flags to be used when compiling 'Benchmarks' for 'TestSuite's.
   , crConfig  :: Config           -- ^ Criterion's configuration (see @Criterion.Types@).

     -- Other settings:
   , verbosity :: Verbosity        -- ^ Console's verbosity level.
   }

instance HasVerbosity TestOpts where 
  verbose = verbosity

-- | 'TestSuite's are generated by the system and comprise analysed test
-- inputs from the user input/test file. Each contains the identifiers of their 
-- respective test programs and each program's type (all test programs belonging 
-- to a test suite have the same type). Test suites that require generated test 
-- data ('Gen..Un' and 'Gen..Bin') include the size range of the data to be
-- generated, and those that use manual test data ('Man..Un' and 'Man..Bin') 
-- include the identifier of the test data to use.
--
-- In the case where multiple valid 'TestSuite's are generated from a single 
-- user input/test file, the user will be asked to select which to run. 
-- Invalid 'TestSuite's can be reviewed to determine their associated input 
-- error(s).
data TestSuite = 
    GenNfUn    [Id] Type (Int, Int, Int)  -- ^ A test suite consisting of unary test programs whereby test data 
                                          --   should be automatically generated by the system and the results of 
                                          --   test programs should be evaluated to normal form.
  | GenWhnfUn  [Id] Type (Int, Int, Int)  -- ^ A test suite consisting of unary test programs whereby test data 
                                          --   should be automatically generated by the system and the results of 
                                          --   test programs should be evaluated to weak head normal form.
  | GenNfBin   [Id] Type (Int, Int, Int)  -- ^ A test suite consisting of binary test programs, whereby test data 
                                          --   should be automatically generated by the system and the results of 
                                          --   test programs should be evaluated to normal form.
  | GenWhnfBin [Id] Type (Int, Int, Int)  -- ^ A test suite consisting of binary test programs, whereby test data 
                                          --   should be automatically generated by the system and the results of 
                                          --   test programs should be evaluated to weak head normal form.
  | ManNfUn    [Id] Type Id               -- ^ A test suite consisting of unary test programs, whereby test data 
                                          --   is specified manually and the results of test programs should be 
                                          --   evaluated to normal form.
  | ManWhnfUn  [Id] Type Id               -- ^ A test suite consisting of unary test programs, whereby test data 
                                          --   is specified manually and the results of test programs should be 
                                          --   evaluated to weak head normal form.
  | ManNfBin   [Id] Type Id               -- ^ A test suite consisting of binary test programs, whereby test data 
                                          --   is specified manually and the results of test programs should be 
                                          --   evaluated to normal form.
  | ManWhnfBin [Id] Type Id               -- ^ A test suite consisting of binary test programs, whereby test data 
                                          --   is specified manually and the results of test programs should be 
                                          --   evaluated to weak head normal form.
-- | (input size, runtime) test results as coordinates. 
--
-- For unary test programs.
type Coord = (Double, Double)

-- | (input size, input size, runtime) test results as coordinates.
--
-- For binary test programs.        
type Coord3 = (Double, Double, Double) 

-- | A simplified version of Criterion's benchmarking report. See 'Report' in 
-- @Criterion.Types@.
data SimpleReport = 
  SimpleReport 
   { 
     name       :: Id             -- ^ Name of test program.
   , size       :: DataSize       -- ^ Size of test data.
   , samples    :: Int            -- ^ Number of samples used to calculate statistics below.
   , mean       :: Double         -- ^ Estimate mean runtime.
   , stdDev     :: Double         -- ^ Estimate standard deviation.
   , outVarEff  :: OutlierEffect  -- ^ Qualitative outlier effect. 
   , outVarFrac :: Double         -- ^ Quantitative outlier effect.
   } deriving Show

-- ** Input errors

-- | Errors in user input\/test file. These are reported to the user at various 
-- stages when the input file is being parsed and test inputs analysed.
data InputError = 
    InputErr    String   -- ^ General input error.
  | InstanceErr String   -- ^ Missing instance declarations.
  | FilePathErr String   -- ^ Invalid filepath.
  | FileErr     String   -- ^ File access error.
  | AnalOptsErr String   -- ^ Invalid analysis options
  | TestOptsErr String   -- ^ Invalid test options.
  | DataOptsErr String   -- ^ Invalid data options.

instance Show InputError where 
  show (InputErr    s) = "Input error: "            ++ s
  show (InstanceErr s) = "Instance error: "         ++ s
  show (FilePathErr s) = "File path error: "        ++ s
  show (FileErr     s) = "File error: "             ++ s
  show (AnalOptsErr s) = "Analysis options error: " ++ s
  show (TestOptsErr s) = "Test options error: "     ++ s
  show (DataOptsErr s) = "Test data error: "        ++ s

instance Exception InputError

-- ** Console output settings.

-- | Verbosity level for console.
data Verbosity =
    Noisy     -- ^ Print everything.
  | Verbose   -- ^ Print verbose output.
  | Normal    -- ^ Display only important messages.
  | Quiet     -- ^ Display only critical messages.
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Configurations that have a verbosity option.
class HasVerbosity a where 
  verbose :: a -> Verbosity

instance HasVerbosity Verbosity where 
  verbose = id

-- ** Linear regression

-- | Statistical analysis options.
data AnalOpts = 
  AnalOpts 
   {
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

-- | Expressions with 'Double' literals.
type Exp = E.Expr Double

-- | Classes of linear functions for regression analysis.
data LinearClass = 
    Poly    Int        -- ^ Polynomial functions:      Poly n      := a_0 + a_1 * x^1 + a_2 * x^2 + .. + a_n * x^n 
  | Log     Int Int    -- ^ Logarithmic functions:     Log  b n    := a_0 + a_1 * log_b^1(x) + a_2 * log_b^2(x) + .. + a_n * log_b^n(x)
  | PolyLog Int Int    -- ^ Polylogarithmic functions: PolyLog b n := a_0 + a_1 * x^1 * log_b^1(x) + a_2 * x^2 * log_b^2(x) + .. + a_n * x^n * log_b^n(x)      
  | Exp     Int        -- ^ Exponential function:      Exp n       := a_0 + n^x
    deriving Eq

instance Show LinearClass where 
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
  show (Poly      n) = "n" ++ powShow n
  show (Log     b n) = "log" ++ baseShow b ++ powShow n ++ "n"
  show (PolyLog b n) = "n" ++ powShow n ++ "log" ++ baseShow b ++ powShow n ++ "n"
  show (Exp       n) = show n ++ "\x207F"

-- | Each 'LinearClass' gives rise to a 'LinearCandidate' that is then fit to 
-- a given data set generating a 'LinearFit'.
data LinearCandidate = 
  LinearCandidate 
   { 
     lcc    :: LinearClass                                         -- ^ Linear class.
   , fxs    :: Vector Double -> Vector Double                      -- ^ Transform x-coords before fitting.
   , fex    :: Vector Double -> Exp                                -- ^ Generate candidate's equation as an 'Exp'.
   , fyhat  :: Vector Double -> Vector Double -> Vector Double     -- ^ Generate model y-coords.
   , fbias  :: Double -> Double                                    -- ^ Linear fitting 'bias'.
   }

-- | For a given data set, this data type incorporates the details a linear
-- function that fits the data with a fitting error 'err'.
data LinearFit =
  LinearFit 
   { 
     lfc  :: LinearClass                      -- ^ Linear class.
   , cfs  :: Vector Double                    -- ^ Model coefficients.
   , ex   :: Exp                              -- ^ Expression.
   , yhat :: Vector Double -> Vector Double   -- ^ Model y-coords for the given set of x-coords.
   , sts  :: Stats                            -- ^ Fitting statistics.
   }

instance Show LinearFit where 
  show lf = unlines 
    [ show (lfc lf)
    , show (sts lf)
    ]

-- | Statistics used to compare 'LinearFit's.
data Stats = 
  Stats 
   {
     p_mse    :: Double        -- ^ Predicted mean squared error.
   , p_mae    :: Double        -- ^ Predicated mean absolute error.
   , ss_tot   :: Double        -- ^ Total sum of squares.
   , p_ss_res :: Double        -- ^ Predicted residual sum of squares.
   , r2       :: Double        -- ^ Coefficient of Determination.
   , a_r2     :: Double        -- ^ Adjusted Coefficient of Determination.
   , p_r2     :: Double        -- ^ Predicted Coefficient of Determination.
   , bic      :: Double        -- ^ Bayesian Information Criterion.
   , aic      :: Double        -- ^ Akaike’s Information Criterion.
   , cp       :: Double        -- ^ Mallows' Cp Statistic.
   } deriving Eq

instance Show Stats where 
  show st = unlines 
    [ 
      "p_mse    = " ++ show (p_mse    st)
    , "p_mae    = " ++ show (p_mae    st)
    , "ss_tot   = " ++ show (ss_tot   st)
    , "p_ss_res = " ++ show (p_ss_res st)
    , "r^2      = " ++ show (r2       st)
    , "a_r^2    = " ++ show (a_r2     st)
    , "p_r^2    = " ++ show (p_r2     st)
    , "bic      = " ++ show (bic      st)
    , "aic      = " ++ show (aic      st)
    , "cp       = " ++ show (cp       st)
    ]

-- | Statistics calculated for each iteration of cross-validation.
data CVStats = 
  CVStats 
   { 
     mse      :: Double     -- ^ Mean squared error.
   , mae      :: Double     -- ^ Mean absolute error.
   , ss_tot_  :: Double     -- ^ Total sum of squares.
   , ss_res_  :: Double     -- ^ Residual sum of squares.
   } deriving Eq

instance Show CVStats where 
  show st = unlines 
    [ 
      "p_mse  = " ++ show (mse     st)
    , "p_mae  = " ++ show (mae     st)
    , "ss_tot = " ++ show (ss_tot_ st)
    , "ss_res = " ++ show (ss_res_ st)
    ]

-- * Defaults

-- | Default data options.
--
-- > defaultDataOpts = Gen 5 5 100
defaultDataOpts :: DataOpts
defaultDataOpts  = Gen 5 5 100

-- | Default statistcial analysis options.
--
-- @
-- defaultAnalOpts = 
--  AnalOpts 
--   { models =
--      [
--        Poly 0        -- y = a_0
--      , Poly 1        -- y = a_0 + a_1x
--      , Poly 2        -- y = a_0 + a_1x + a_2x^2
--      , Poly 3        -- y = a_0 + a_1x + .. + a_3x^3
--      , Poly 4        -- y = a_0 + a_1x + .. + a_4x^4
--      , Log 2 1       -- y = a_0 + a_1log_2(x)
--      , Log 2 2       -- y = a_0 + a_1log_2(x) a_2log_2^2(x)
--      , PolyLog 2 1   -- y = a_0 + a_1xlog_2(x)
--      , Exp 2         -- y = a_0 + a_1 * 2^x
--      ]
--   , cvIters      = 200                          -- 200 folds for cross-validation.
--   , cvTrain      = 0.7                          -- 70% training data/30% validation data. 
--   , fFiltStats   = defaultFilterStats           -- 'defaultFilterStats'
--   , fSortStats   = defaultSortStats             -- 'defaultSortStats'
--   , fRuntimeComp = defaultRuntimeOrdering       -- 'defaultRuntimeOrdering'
--   , fImprov      = defaultImprovementOrdering   -- 'defaultImprovementOrdering'
--   , graphFP      = Just "./TimeChecked.png"     -- Filepath for graph of runtime results.
--   , repFP        = Nothing                      -- Filepath for report.
--   , coordsFP     = Nothing                      -- Filepath for coords CSV.
--   }
-- @
defaultAnalOpts :: AnalOpts 
defaultAnalOpts  = 
  AnalOpts
   {
     models       = fmap Poly [0..4] ++ [Log 2 1, Log 2 2, PolyLog 2 1, Exp 2]
   , cvIters      = 200
   , cvTrain      = 0.7
   , fFiltStats   = defaultFilterStats
   , fSortStats   = defaultSortStats
   , fRuntimeComp = defaultRuntimeOrdering
   , fImprov      = defaultImprovementOrdering
   , graphFP      = Just "./TimeChecked.png"  
   , repFP        = Nothing                   
   , coordsFP     = Nothing
   }

-- | The default method for discarding models that \"do not\" fit a given
-- data set. This is achieved by filtering associated fitting 'Stats' 
-- (produced by cross-validation).
--
-- For the default option we discard any model whose r^2/adjusted r^2 value 
-- is outside of the range (0..1), as this is a strong indicator that the 
-- model is a \"bad fit\" for the data.
--
-- Of course, users may disagree with this in which case they can set their 
-- own predicate in their 'TestOpts', e.g., maybe change it to @const True@.
defaultFilterStats :: Stats -> Bool
defaultFilterStats s = r2 s >= 0 && r2 s <= 1 && a_r2 s >= 0 && a_r2 s <= 1

-- | The default method for selecting a model that \"best fits\" a given data 
-- set. This is achieved by comparing each model's fitting 'Stats' (produced
-- by cross-validation).
-- 
-- In general we want the model with the /lowest/ predicted mean squared error 
-- 'p_mse'. However, through testing we have found that in the case where two 
-- models have 'p_mse' with relative error <= 0.15, it seems appropriate to pick 
-- the one with the /highest /predicted/ r^2 value 'pr^2'.
--
-- Of course, users may disagree with this in which case they can set their 
-- own ordering in their 'TestOpts'. In order to be facilitate different 
-- methods for ordering 'Stats', we have included a range of best-fit 
-- indicators, see 'Stats' for more information.
defaultSortStats :: Stats -> Stats -> Ordering 
defaultSortStats s1 s2                      -- Note: we want the highest predicted r^2.
  | relativeError p_mse_1 p_mse_2 <= 0.15 = compare (p_r2 s2) (p_r2 s1)
  | otherwise = compare p_mse_1 p_mse_2        
  where 
    p_mse_1 = p_mse s1 
    p_mse_2 = p_mse s2

-- | Default test options.
--
-- @ 
-- defaultTestOpts  = 
--  TestOpts 
--   { progs     = []                        -- Test all (testable) programs in user input/test file.
--   , dat       = defaultDataOpts           -- Generate test inputs of sizes: 5, 10 .. 100.
--   , nfRes     = True                      -- Ensure results of test programs are in normal form.
--   , subNfRes  = False                     -- NOTE: FEATURE CURRENTLY DISABLED.
--   , analOpts  = defaultAnalOpts           -- 'defaultAnalOpts'.
--   , ghcFlags  = []                        -- No compiler flags.
--   , crConfig  = defaultConfig             -- Use default Criterion configuration.
--   , verbosity = Normal                    -- Print important messages to console.
--   }
-- @
defaultTestOpts :: TestOpts
defaultTestOpts  = 
  TestOpts 
   { progs     = []                        -- Test all (testable) programs in user input/test file.
   , dat       = defaultDataOpts           -- Generate test inputs of sizes: 5, 10 .. 100.
   , nfRes     = True                      -- Ensure results of test programs are in normal form.
   , subNfRes  = False                     -- NOTE: FEATURE CURRENTLY DISABLED.
   , analOpts  = defaultAnalOpts           -- 'defaultAnalOpts'.
   , ghcFlags  = []                        -- No compiler flags.
   , crConfig  = defaultConfig             -- Use default Criterion configuration.
   , verbosity = Normal                    -- Print important messages to console.
   }

-- | Default runtime ordering Here we are comparing the runtimes @t1@ and @t2@ 
-- of test programs @p1@ and @p2@.
defaultRuntimeOrdering :: Double -> Double -> Ordering
defaultRuntimeOrdering t1 t2 
  | relativeError t1 t2 <= 0.15 = EQ 
  | otherwise = compare t1 t2

-- | Default improvement ordering: convert a list of orderings from runtime 
-- comparisons (see 'defaultRuntimeOrdering') into an improvement ordering 
-- and confidence indicator if appropriate.
defaultImprovementOrdering :: [Ordering] -> Maybe (Ordering, Double)
defaultImprovementOrdering [] = Nothing
defaultImprovementOrdering xs 
  | e >= 0.9 = Just (EQ, e) 
  | l >= 0.9 = Just (LT, l)
  | g >= 0.9 = Just (GT, g)
  | otherwise = Nothing
  where 
    len = fromIntegral (length xs)
    e = eqs / len
    l = lts / len
    g = gts / len
    (eqs, lts, gts)   = foldr f (0, 0, 0) xs
    f EQ (eq, lt, gt) = (eq + 1, lt    , gt)
    f LT (eq, lt, gt) = (eq    , lt + 1, gt)
    f GT (eq, lt, gt) = (eq    , lt    , gt + 1)

-- * Helpers

-- | Display a test suite as a document.
displayTestSuite :: TestSuite -> Doc
displayTestSuite (GenNfUn fs fTy (l, s, u)) =
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Gen" <+> int l <+> int s <+> int u
       , text "Eval     " <+> text "nf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]
displayTestSuite (GenWhnfUn fs fTy (l, s, u)) =
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Gen" <+> int l <+> int s <+> int u
       , text "Eval     " <+> text "whnf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]
displayTestSuite (GenNfBin fs fTy (l, s, u)) =
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Gen" <+> int l <+> int s <+> int u
       , text "Eval     " <+> text "nf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]
displayTestSuite (GenWhnfBin fs fTy (l, s, u)) =
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Gen" <+> int l <+> int s <+> int u
       , text "Eval     " <+> text "whnf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]
displayTestSuite (ManNfUn fs fTy d) = 
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Manual (" <> text d <> text ")"
       , text "Eval     " <+> text "nf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]
displayTestSuite (ManWhnfUn fs fTy d) = 
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Manual (" <> text d <> text ")"
       , text "Eval     " <+> text "whnf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]
displayTestSuite (ManNfBin fs fTy d) = 
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Manual (" <> text d <> text ")"
       , text "Eval     " <+> text "nf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]
displayTestSuite (ManWhnfBin fs fTy d) = 
  vcat [ text "Types    " <+> text fTy
       , text "Data     " <+> text "Manual (" <> text d <> text ")"
       , text "Eval     " <+> text "whnf"
       , text "Programs " <+> hcat (punctuate (text ", ") $ fmap text fs)
       ]

-- | Display valid test suites as a document.
displayValidTestSuites :: [TestSuite] -> Doc
displayValidTestSuites []   = text "No valid test suites."
displayValidTestSuites tsVs = 
  vcat $ nest 1 (text "Test Suites:")
       : fmap (\(ts, n) -> nest 1 n $$ nest 2 ts) disp
  where disp = zip (fmap displayTestSuite tsVs) 
                   (fmap (\n -> int n <> char '.') [1..])

-- | Display invalid test suites as a document.
displayInvalidTestSuites :: [(TestSuite, InputError)] -> Doc
displayInvalidTestSuites [] = text "No invalid test suites."
displayInvalidTestSuites tsIs = 
    vcat $ nest 1 (text "Invalid Test Suites:") 
         : fmap (\((ts, e), n) -> 
               nest 1 n
            $$ nest 2 ts 
            $$ nest 2 (text (show e))) disp
  where disp = zip (fmap (first displayTestSuite) tsIs) 
                   (fmap (\n -> int n <> char '.') [1..])

-- | Each test suite requires at least 20 distinctively sized test inputs.
-- 
-- > minInputs = 20
minInputs :: Int 
minInputs  = 20

-- | Maximum number of predictors for fitting models.
--
-- > maxPredictors = 10
maxPredictors :: Int 
maxPredictors  = 10

-- | Convert @Gen l s u :: DataOpts@ to a Haskell range.
toHRange :: DataOpts -> [Int]
toHRange Manual      = []
toHRange (Gen l s u) = [l, (l + s) .. u]

-- | Extract the 'Coord's from a list of 'SimpleReport's.
simpleReportCoords :: [SimpleReport] -> [Coord]
simpleReportCoords  = catMaybes . fmap f
  where f sr = case size sr of 
                 SizeUn n  -> Just (fromIntegral n, mean sr)
                 SizeBin{} -> Nothing

-- | Extract the 'Coord3's from a list of 'SimpleReport's.
simpleReportCoords3 :: [SimpleReport] -> [Coord3]
simpleReportCoords3  = catMaybes . fmap f
  where f sr = case size sr of 
                 SizeUn{}      -> Nothing 
                 SizeBin n1 n2 -> Just (fromIntegral n1, fromIntegral n2, mean sr)

-- | Number of predictors for each model.
numPredictors :: LinearClass -> Int 
numPredictors (Poly      k) = k + 1 
numPredictors (Log     _ k) = k + 1 
numPredictors (PolyLog _ k) = k + 1 
numPredictors Exp{}         = 2

-- ** Parsing helpers

-- | Parse the encoded test data size from the name of a Criterion report.
parseDataSize :: Parser DataSize 
parseDataSize  = (do 
  void $ symbol "("
  n1 <- integer
  void $ symbol ","
  n2 <- integer
  void $ symbol ")"
  return (SizeBin n1 n2)) MP.<|> (SizeUn <$> integer)




-- ************************
--- NEW
-- ************************






-- | Definitions in user input files.
data UsrFun = UsrFun Id Type HsType deriving Eq  

usrFunIdt :: UsrFun -> Id 
usrFunIdt (UsrFun idt _ _) = idt 

usrFunType :: UsrFun -> Type 
usrFunType (UsrFun _ ty _) = ty 

usrFunHsType :: UsrFun -> HsType
usrFunHsType (UsrFun _ _ hsTy) = hsTy 

-- | The input type of a unary or binary test program.
data HsTypeInp = 
    HsTyInpUn  HsType
  | HsTyInpBin HsType HsType
    deriving (Show, Eq)
 













-- | Test suites specify test inputs (i.e., test programs and test data) and a 
-- number of options /specific/ to testing those inputs. Users may define zero 
-- or more 'TestSuiteOpts' and they will be run sequentially by the system. The 
-- 'defaultTestSuiteOpts' will be used in the case where no test suites are 
-- defined by the user.
data TestSuiteOpts = 
  TestSuiteOpts 
   { 
     -- Test inputs:
     _progs     :: [Id]             -- ^ The names of the programs to test: all testable programs in the 
                                    --   user input/test file will be considered if this list is empty.
   , _dataOpts  :: DataOpts         -- ^ Test data options.

     -- Runtime measurement options:
   , _nfRes     :: Bool             -- ^ Whether the results of test programs should be evaluated to nf (@True@) or whnf (@False@).

     -- Statistical analysis options:
   , _analOpts  :: AnalOpts         -- ^ Statistical analysis options.

     -- Options passed to subsystems:
   , _ghcFlags  :: [String]         -- ^ GHC compiler flags to be used when compiling 'Benchmarks' for 'TestSuite's.
   , _crConfig  :: Config           -- ^ Criterion's configuration (see @Criterion.Types@).
   }

defaultTestSuiteOpts = TestSuiteOpts { _progs = [], _dataOpts = Gen 5 5 100, _nfRes = True, _analOpts = defaultAnalOpts, _ghcFlags = [], _crConfig = defaultConfig }

-- | The test configuration specifies a number of settings to be used for /all/
-- test suites.
data TestConfig = 
  TestConfig
   {
     _verbosity :: Verbosity        -- ^ Console's verbosity level.
   }

defaultTestConfig :: TestConfig
defaultTestConfig  = TestConfig { _verbosity = Normal }

-- 
data UsrInps = 
  UsrInps
   {
     _allElems           :: [ModuleElem]
   , _ignoredElems       :: [ModuleElem]
   , _allFuns            :: [(Id, Type)]
   , _invalidFuns        :: [(Id, Type, [InputError])]
   , _nullaryFuns        :: [UsrFun]
   , _unaryFuns          :: [UsrFun]
   , _binaryFuns         :: [UsrFun]
   , _arbFuns            :: [UsrFun]
   , _nfFuns             :: [UsrFun]
   , _invalidData        :: [(Id, Type, [InputError])]
   , _unaryData          :: [UsrFun]
   , _binaryData         :: [UsrFun]
   , _invalidTestSuites  :: [(Id, [InputError])]
   , _testSuites         :: [(Id, TestSuiteOpts)]
   , _invalidTestConfigs :: [(Id, [InputError])]
   , _testConfigs        :: [(Id, TestConfig)]
   }

instance Show UsrInps where 
  show usrInps = unlines 
    [
      "allElems ="
    , unlines $ indent 2 $ fmap show $ _allElems usrInps
    , "ignoredElems ="
    , unlines $ indent 2 $ fmap show $ _ignoredElems usrInps
    , "allFuns ="
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ ty) $ _allFuns usrInps 
    , "invalidFuns ="
    , unlines $ indent 2 $ fmap (\(idt, ty, errs) -> idt ++ " :: " ++ ty ++ "\n" ++ (unlines $ indent 4 $ fmap show errs)) $ _invalidFuns usrInps
    , "nullaryFuns =" 
    , unlines $ indent 2 $ fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) $ _nullaryFuns usrInps
    , "unaryFuns =" 
    , unlines $ indent 2 $ fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) $ _unaryFuns usrInps
    , "binaryFuns =" 
    , unlines $ indent 2 $ fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) $ _binaryFuns usrInps
    , "arbFuns =" 
    , unlines $ indent 2 $ fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) $ _arbFuns usrInps
    , "nfFuns =" 
    , unlines $ indent 2 $ fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) $ _nfFuns usrInps
    , "invalidData ="
    , unlines $ indent 2 $ fmap (\(idt, ty, errs) -> idt ++ " :: " ++ ty ++ "\n" ++ (unlines $ indent 4 $ fmap show errs)) $ _invalidData usrInps
    , "unaryData =" 
    , unlines $ indent 2 $ fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) $ _unaryData usrInps
    , "binaryData =" 
    , unlines $ indent 2 $ fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) $ _binaryData usrInps
    , "invalidTestSuites ="
    , unlines $ indent 2 $ fmap (\(idt, errs) -> idt ++ "\n" ++ (unlines $ indent 4 $ fmap show errs)) $ _invalidTestSuites usrInps
    , "testSuites ="
    , unlines $ indent 2 $ fmap fst $ _testSuites usrInps
    , "invalidTestConfigs ="
    , unlines $ indent 2 $ fmap (\(idt, errs) -> idt ++ "\n" ++ (unlines $ indent 4 $ fmap show errs)) $ _invalidTestConfigs usrInps
    , "testConfigs ="
    , unlines $ indent 2 $ fmap fst $ _testConfigs usrInps
    ]

indent :: Int -> [String] -> [String]
indent n = zipWith (++) (repeat $ replicate n ' ')

initUsrInps :: UsrInps
initUsrInps  = 
  UsrInps
   {
     _allElems           = []
   , _ignoredElems       = []
   , _allFuns            = []
   , _invalidFuns        = []
   , _nullaryFuns        = []
   , _unaryFuns          = []
   , _binaryFuns         = []
   , _arbFuns            = []
   , _nfFuns             = []
   , _invalidData        = []
   , _unaryData          = []
   , _binaryData         = []
   , _invalidTestSuites  = []
   , _testSuites         = []
   , _invalidTestConfigs = []
   , _testConfigs        = []
   }


{-


-}
-}