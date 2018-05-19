
{-# OPTIONS_GHC -Wall #-} 

{-|

  Module      : AutoBench.DynamicInstanceChecks
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
   - 
-}

module AutoBench.DynamicInstanceChecks where          -- note: we load arbitrary instances just by checking but its a catch-22.

import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary)

checkNFDataInputUn :: NFData a => (a -> b) -> () 
checkNFDataInputUn _ = ()

checkNFDataInputBin :: (NFData a, NFData b) => (a -> b -> c) -> ()
checkNFDataInputBin _ = ()

checkNFDataResultUn :: NFData b => (a -> b) -> ()
checkNFDataResultUn _ = ()

checkNFDataResultBin :: NFData c => (a -> b -> c) -> ()
checkNFDataResultBin _ = ()

checkArbitraryUn :: Arbitrary a => (a -> b) -> () 
checkArbitraryUn _ = ()

checkArbitraryBin :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> ()
checkArbitraryBin _ = ()

{-

module TimeCheck.InputValidation
 (
   -- * Validating 'TestOpts'
   -- 
   -- | In the case of 'initTestOpts', the validation process works by calling 
   -- the function on the given 'TestOpts' using Hint and seeing whether an 
   -- error occurs. If an error does occur, then one or more of its fields
   -- aren't initialised. If not, then all fields are initialised.
   initTestOpts        -- Ensure all record fields of 'TestOpts' are initialised.
 , validNfs            -- Ensure 'nfRes' and 'subNfRes' settings coincide.

   -- * Validating test programs
   --
   -- | Ensure that test programs (in a test suite) satisfy the required 
   -- instances for automatic data generation (Arbitrary) and benchmarking 
   -- (NFData). This includes evaluating the test program's results to 
   -- normal form/weak head normal form.
   -- 
   -- Note: we use the same functions for validating unary and binary test 
   -- programs by uncurrying because (NFData a, NFData b) => NFData (a, b).
   --
   -- This validation process works by calling the appropriate function on a
   -- test program using Hint and seeing whether an error occurs. If an error 
   -- does occur, then the test program does not support the required instances
   -- and the test suite overall is invalid. If no error occurs, then the
   -- test program (and all other test programs in the test suite, as they have 
   -- the same type) satisfied the required instances.
 , testGenNf           -- Arbitrary/NFData instances for generated data (nf).
 , testGenWhnf         -- Arbitrary/NFData instances for generated data (whnf).
 , testManNf           -- Arbitrary/NFData instances for manual data (nf).
 , testManWhnf         -- Arbitrary/NFData instances for manual data (whnf).

   -- * Validating test data
   --
   -- | Each test suite requires at least 20 distinctively sized test inputs.
   -- So we need to ensure that:
   --
   -- * In the case of data generation, the size range given by the 'Gen' 
   --   setting is suitably large.
   -- * In the case of manual data, there is \'enough\' data specified by the
   --   user.
 , countInputsGen      -- Count inputs for Gen DataOpts.
 , countInputsManUn    -- Count inputs for Manual DataOpts (unary)
 , countInputsManBin   -- Count inputs for Manual DataOpts (binary).

 -- *** 
 -- NEW 
 -- *******
 , testArbitraryInputUn
 , testArbitraryInputBin
 , testNFDataInputUn
 , testNFDataInputBin
 , testNFDataResultUn
 , testNFDataResultBin
 , testInitialisedTestConfig
 , testInitialisedTestSuite

 ) where 

import           Control.DeepSeq (NFData)
import qualified Criterion.Types as CR
import           Test.QuickCheck (Arbitrary)
import           Data.List       (nub)

import TimeCheck.Types 
  (
    AnalOpts(..)
  , DataOpts(..)
  , TestOpts(..)
  , TestDataUn
  , TestDataBin
  )


-- NEW 
import TimeCheck.Types (TestConfig(..), TestSuiteOpts(..))



-- * Validating 'TestOpts'

-- | A hacky way to ensure all 'TestOpts' record fields are initialised.
initTestOpts :: TestOpts -> ()
initTestOpts tOpts = 
        progs                 tOpts
  `seq` dat                   tOpts
  `seq` nfRes                 tOpts
  `seq` seqAnalOpts (analOpts tOpts)
  `seq` subNfRes              tOpts
  `seq` ghcFlags              tOpts
  `seq` seqCrConfig (crConfig tOpts)
  `seq` verbosity             tOpts
  `seq` ()
  where 
    seqAnalOpts aOpts = 
            models       aOpts 
      `seq` cvIters      aOpts 
      `seq` cvTrain      aOpts 
      `seq` fFiltStats   aOpts 
      `seq` fSortStats   aOpts 
      `seq` fRuntimeComp aOpts 
      `seq` fImprov      aOpts
      `seq` graphFP      aOpts
      `seq` repFP        aOpts
      `seq` coordsFP     aOpts
      `seq` ()
    seqCrConfig cfg   =  
            CR.confInterval cfg
      `seq` CR.timeLimit    cfg
      `seq` CR.resamples    cfg
      `seq` CR.regressions  cfg
      `seq` CR.rawDataFile  cfg
      `seq` CR.reportFile   cfg
      `seq` CR.csvFile      cfg
      `seq` CR.jsonFile     cfg
      `seq` CR.junitFile    cfg
      `seq` CR.verbosity    cfg
      `seq` CR.template     cfg
      `seq` ()

-- | Ensure test options nf settings are valid: 'subNfRes' can only be used
-- with 'nfRes', i.e., 'subNfRes' => 'nfRes'.
validNfs :: Bool -> Bool -> Bool 
validNfs True  True  = True 
validNfs False False = True 
validNfs False True  = True 
validNfs True  False = False

-- * Validating test programs

-- | The necessary instances required for test programs that use generated test 
-- data whereby their results should be evaluated to normal form during
-- benchmarking.
testGenNf :: (NFData a, NFData b, Arbitrary a) => (a -> b) -> ()
testGenNf _ = ()

-- | The necessary instances required for test programs that use generated test 
-- data whereby their results should be evaluated to weak head normal form 
-- during benchmarking.
testGenWhnf :: (NFData a, Arbitrary a) => (a -> b) -> ()
testGenWhnf _ = ()

-- | The necessary instances required for test programs that use manual test 
-- data whereby their results should be evaluated to normal form during 
-- benchmarking.
testManNf :: (NFData a, NFData b) => (a -> b) -> ()
testManNf _ = ()

-- | The necessary instances required for test programs that use manual test 
-- data whereby their results should be evaluated to weak head normal form 
-- during benchmarking.
testManWhnf :: NFData a => (a -> b) -> ()
testManWhnf _ = ()

-- 



-- * Validating test data

-- | Count test inputs to generate for 'DataOpts'. Here we only really care 
-- about the 'Gen' setting.
countInputsGen :: DataOpts -> Int
countInputsGen Manual      = 0
countInputsGen (Gen l s u) | l <= 0 || s <= 0 || u <= 0 = 0
countInputsGen (Gen l s u) = (u - l) `div` s + 1

-- | Count the number of distinctly sized inputs for user-specified (i.e., 
-- manual) test data used by unary test functions.
countInputsManUn :: TestDataUn a -> Int
countInputsManUn  = length . nub . fmap fst

-- | Count the number of distinctly sized inputs for user-specified (i.e., 
-- manual) test data used by binary test functions.
countInputsManBin :: TestDataBin a b -> Int
countInputsManBin  = length . nub . fmap (\(s1, s2, _, _) -> (s1, s2))








-- NEW 

testInitialisedTestConfig :: TestConfig -> ()
testInitialisedTestConfig cfg = _verbosity cfg `seq` ()

testInitialisedTestSuite :: TestSuiteOpts -> ()
testInitialisedTestSuite ts = 
        _progs                 ts
  `seq` _dataOpts              ts
  `seq` _nfRes                 ts
  `seq` seqAnalOpts (_analOpts ts)
  `seq` _ghcFlags              ts
  `seq` seqCrConfig (_crConfig ts)
  `seq` ()
  where 
    seqAnalOpts aOpts = 
            models       aOpts 
      `seq` cvIters      aOpts 
      `seq` cvTrain      aOpts 
      `seq` fFiltStats   aOpts 
      `seq` fSortStats   aOpts 
      `seq` fRuntimeComp aOpts 
      `seq` fImprov      aOpts
      `seq` graphFP      aOpts
      `seq` repFP        aOpts
      `seq` coordsFP     aOpts
      `seq` ()
    seqCrConfig cfg   =  
            CR.confInterval cfg
      `seq` CR.timeLimit    cfg
      `seq` CR.resamples    cfg
      `seq` CR.regressions  cfg
      `seq` CR.rawDataFile  cfg
      `seq` CR.reportFile   cfg
      `seq` CR.csvFile      cfg
      `seq` CR.jsonFile     cfg
      `seq` CR.junitFile    cfg
      `seq` CR.verbosity    cfg
      `seq` CR.template     cfg
      `seq` ()

testNFDataResultUn :: NFData b => (a -> b) -> ()
testNFDataResultUn _ = ()

testNFDataResultBin :: NFData c => (a -> b -> c) -> ()
testNFDataResultBin _ = ()

testNFDataInputUn :: NFData a => (a -> b) -> ()
testNFDataInputUn  _ = ()

testNFDataInputBin :: (NFData a, NFData b) => (a -> b -> c) -> ()
testNFDataInputBin _ = ()

testArbitraryInputUn :: Arbitrary a => (a -> b) -> () 
testArbitraryInputUn _ = ()

testArbitraryInputBin :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> ()
testArbitraryInputBin _ = ()

-}