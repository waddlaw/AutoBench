
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.Internal.DynamicChecks
  Description : Dynamically validating and classifying user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  A number of dynamic checks are used to classify user inputs according to 
  properties that cannot be checked statically (see 
  'AutoBench.Internal.StaticChecks' for static checking). For example, the 
  system cannot determine whether the input types of user-specified test 
  programs are members of the 'NFData' or 'Arbitrary' type classes by simply 
  inspecting their type signatures. Instead, dynamic checks are used to 
  determine whether these properties hold.

  Following static checking, the system determines whether the types of user 
  inputs have the following properties:

  1. NFDataInput: functions satisfying the static properties of /unaryFun/ and
     /binaryFun/ whose input types are members of the 'NFData' type class;       ==> added to '_benchFuns'
  2. NFDataResult: functions satisfying the static properties of /unaryFun/ and
     /binaryFun/ whose result types are members of the 'NFData' type class;      ==> added to '_nfFuns'
  3. Arbitrary: functions satisfying the /genable/ static property whose 
     input types are members of the 'Arbitrary' type class;                      ==> kept in '_arbFuns'

  The system all interprets a number of user inputs in order to perform a 
  number of dynamic checks on the definitions themselves: 

  4. TestSuites: functions satisfying the /nullaryFun/ static property whose     
     types are @TestSuite@;                                                      ==> interpreted, checked, and definition added to '_testSuites'
  5. FullTestSuites: interpreted 'TestSuites' whose record fields are            
     all initialised;                                                            ==> kept in '_testSuites'
  6. ValidUnaryData: functions satisfying the /unaryData/ static property whose  
     definitions are valid;                                                      ==> interpreted, checked, but only size information kept in '_unaryData'
  7. ValidBinaryData: functions satisfying the /binaryData/ static property 
     whose definitions are valid.                                                ==> interpreted, checked, but only size information in '_binaryData'
     
  User inputs that fail checks 5-7 are added to the the respective invalid
  lists, for example '_invalidTestSuites' for failing check 5.

  Example of how dynamic checking works in practice: Each function in the 
  '_unaryFuns' list of 'UserInputs' is passed to 'checkNFDataInputUn' function. 
  The result of this function call is then evaluated dynamically using the
  hint package. If an error is thrown during evaluation, then the assumption is 
  that the argument function /does not/ have an 'NFData' instance for its input 
  type. If no error results, then the assumption is that the argument function 
  /does/ have an 'NFData' instance for its input type. Thus, all functions in 
  the '_unaryFuns' list can be classified according to the /NFDataInput/ 
  property.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   -
-}

module AutoBench.Internal.DynamicChecks
  (
    checkNFDataInputUn              -- Dynamic check for 1. /NFDataInput/ for unary functions.
  , checkNFDataInputBin             -- Dynamic check for 1. /NFDataInput/ for binary functions.
  , checkNFDataResultUn             -- Dynamic check for 2. /NFDataResult/ for unary functions.
  , checkNFDataResultBin            -- Dynamic check for 2. /NFDataResult/ for binary functions.
  , checkArbitraryUn                -- Dynamic check for 3. /Arbitrary/ for unary functions.
  , checkArbitraryBin               -- Dynamic check for 3. /Arbitrary/ for binary functions.
  , checkInitialisedTestSuite       -- Dynamic check for 5. /FullTestSuites/.
  , sizeUnaryTestData               -- Dynamic check for 6. /ValidUnaryData/.
  , sizeBinaryTestData              -- Dynamic check for 7. /ValidBinaryData/.
  -- ** QuickCheck testing
  , quickCheckUn                    -- Check whether test programs are semantically equal using QuickCheck. For unary test programs.
  , quickCheckBin                   -- Check whether test programs are semantically equal using QuickCheck. For binary test programs.
  
  ) where 

import Control.DeepSeq      (NFData, rnf)
import System.IO.Unsafe     (unsafePerformIO)
import Test.QuickCheck      ( Arbitrary, Args(..), arbitrary 
                            , quickCheckWithResult, stdArgs )
import Test.QuickCheck.Gen  (sample')
import Test.QuickCheck.Test (isSuccess)

import AutoBench.Internal.Utils (allEq)
import AutoBench.Internal.Types 
  ( BinaryTestData
  , TestSuite(..)
  , UnaryTestData
  )

-- | Dynamic check for 1. /NFDataInput/ for unary functions.
checkNFDataInputUn :: NFData a => (a -> b) -> () 
checkNFDataInputUn _ = ()

-- | Dynamic check for 1. /NFDataInput/ for binary functions.
checkNFDataInputBin :: (NFData a, NFData b) => (a -> b -> c) -> ()
checkNFDataInputBin _ = ()

-- | Dynamic check for 2. /NFDataResult/ for unary functions.
checkNFDataResultUn :: NFData b => (a -> b) -> ()
checkNFDataResultUn _ = ()

-- | Dynamic check for 2. /NFDataResult/ for binary functions.
checkNFDataResultBin :: NFData c => (a -> b -> c) -> ()
checkNFDataResultBin _ = ()

-- | Dynamic check for 3. /Arbitrary/ for unary functions.
checkArbitraryUn :: Arbitrary a => (a -> b) -> () 
checkArbitraryUn _ = ()

-- | Dynamic check for 3. /Arbitrary/ for binary functions.
checkArbitraryBin :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> ()
checkArbitraryBin _ = ()
 
-- | Dynamic check for 5. /FullTestSuites/.
checkInitialisedTestSuite :: TestSuite -> ()
checkInitialisedTestSuite  = rnf

-- | Dynamic check for 6. /ValidUnaryData/.
sizeUnaryTestData :: UnaryTestData a -> [Int]
sizeUnaryTestData  = fmap fst 

-- | Dynamic check for 7. /ValidBinaryData/.
sizeBinaryTestData :: BinaryTestData a b -> [(Int, Int)]
sizeBinaryTestData  = fmap (\(s1, s2, _, _) -> (s1, s2))

-- * QuickCheck testing

-- | Check whether test programs are semantically equal using QuickCheck.
-- For unary test programs.
quickCheckUn :: (Arbitrary a, Eq b) => [a -> b] -> Bool
quickCheckUn ps = unsafePerformIO $ do                          -- I'm not sure whether hint can handle @IO Bool@?
  tDats <- sample' arbitrary                                    -- Generate some inputs.
  isSuccess <$> quickCheckWithResult stdArgs { chatty = False } -- Turn off QuickCheck output.
    (and [ allEq [ p tDat | p <- ps ] | tDat <- tDats ])        -- Check whether test programs give same results.

-- | Check whether test programs are semantically equal using QuickCheck.
-- For binary test programs.
quickCheckBin :: (Arbitrary a, Arbitrary b, Eq c) => [a -> b -> c] -> Bool 
quickCheckBin ps = unsafePerformIO $ do   
  tDats <- sample' arbitrary                                    -- Generate pairs of inputs.
  isSuccess <$> quickCheckWithResult stdArgs { chatty = False } 
    (and [ allEq [ p tDat1 tDat2 | p <- ps ] | (tDat1, tDat2) <- tDats ])                  