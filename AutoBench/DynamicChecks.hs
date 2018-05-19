
{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE BangPatterns #-}

{-|

  Module      : AutoBench.DynamicChecks
  Description : Dynamically validating and classifying user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  A number of dynamic checks are used to classify user inputs according to 
  properties that cannot be checked statically (see 'AutoBench.StaticChecks'
  for static checking). For example, the system cannot determine whether the 
  input types of user-specified test programs are members of the 'Arbitrary' or 
  'NFData' type classes by simply inspecting their type signatures. Instead,
  dynamic checks are used to determine whether these properties hold.

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
     definitions are valid;                                                      ==> interpreted, checked, but only Id kept in '_unaryData'
  7. ValidBinaryData: functions satisfying the /binaryData/ static property 
     whose definitions are valid.                                                ==> interpreted, checked, but only Id kept in '_binaryData'
     
  User inputs that fail checks 5-7 are added to the the respective invalid
  lists, for example '_invalidTestSuites' for failing check 5.

  Following dynamic checking, test suites in the '_testSuites' list are subject
  to further static checks to asses their validity (see AutoBench.StaticChecks).

-}

module AutoBench.DynamicChecks where 

import Control.Monad                (filterM, foldM, void)
import Control.Monad.Catch          (catch, catchAll)
import Language.Haskell.Interpreter (MonadInterpreter, InterpreterError, as, interpret, eval)

import AutoBench.AbstractSyntax (HsType, Id, ModuleName, prettyPrint, qualIdt)
import AutoBench.StaticChecks   (isUnaryTyFun, isBinaryTyFun, isTestSuite)
import AutoBench.Types          (InputError(..), TestSuite, UserInputs(..), minInputs)

-- Each check assumes the necessary files are already loaded
-- Instance checks are in AutoBench.DynamicInstanceChecks



catNFDataInput :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
catNFDataInput mn inps = do
  benchFunsUn  <- filterM checkUn  (_unaryFuns  inps)
  benchFunsBin <- filterM checkBin (_binaryFuns inps)
  return inps { _benchFuns = benchFunsUn ++ benchFunsBin }
  where 
    checkUn :: MonadInterpreter m => (Id, HsType) -> m Bool
    checkUn  (idt ,_) = catchIE 
      ((void . eval $ qualCheckFunUn ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
      (const $ return False)

    checkBin :: MonadInterpreter m => (Id, HsType) -> m Bool
    checkBin (idt, _) = catchIE 
      ((void . eval $ qualCheckFunBin ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
      (const $ return False)

    -- Functions to perform checks, qualified with module name.
    qualCheckFunUn  = "AutoBench.DynamicInstanceChecks.checkNFDataInputUn"
    qualCheckFunBin = "AutoBench.DynamicInstanceChecks.checkNFDataInputBin"



catNFDataResult :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
catNFDataResult mn inps = do
  nfFunsUn  <- filterM checkUn  (_unaryFuns  inps)
  nfFunsBin <- filterM checkBin (_binaryFuns inps)
  return inps { _nfFuns = nfFunsUn ++ nfFunsBin }
  where 
    checkUn :: MonadInterpreter m => (Id, HsType) -> m Bool
    checkUn  (idt ,_) = catchIE 
      ((void . eval $ qualCheckFunUn ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
      (const $ return False)

    checkBin :: MonadInterpreter m => (Id, HsType) -> m Bool
    checkBin (idt, _) = catchIE 
      ((void . eval $ qualCheckFunBin ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
      (const $ return False)

    -- Functions to perform checks, qualified with module name.
    qualCheckFunUn  = "AutoBench.DynamicInstanceChecks.checkNFDataResultUn"
    qualCheckFunBin = "AutoBench.DynamicInstanceChecks.checkNFDataResultBin"


catArbitrary :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
catArbitrary mn inps = do
  arbFuns <- filterM check (_arbFuns inps)
  return inps { _arbFuns = arbFuns }
  where 
    check :: MonadInterpreter m => (Id, HsType) -> m Bool
    check (idt , ty) 
      | isUnaryTyFun ty = catchIE 
            ((void . eval $ qualCheckFunUn ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
            (const $ return False)
      | isBinaryTyFun ty = catchIE 
            ((void . eval $ qualCheckFunBin ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
            (const $ return False)
      | otherwise = return False                                                                           -- <TO-DO>: should probably error report here?

    -- Functions to perform checks, qualified with module name.
    qualCheckFunUn  = "AutoBench.DynamicInstanceChecks.checkArbitraryUn"
    qualCheckFunBin = "AutoBench.DynamicInstanceChecks.checkArbitraryBin"


interpTestSuites :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
interpTestSuites mn inps = do 
  (valids, invalids) <- foldM loadTestSuites ([], []) (_nullaryFuns inps)
  return inps { _invalidTestSuites = invalids, _testSuites = valids } 
  where 
    loadTestSuites 
      :: MonadInterpreter m 
      => ([(Id, TestSuite)], [(Id, [InputError])])
      -> (Id, HsType)
      -> m ([(Id, TestSuite)], [(Id, [InputError])])
    loadTestSuites (vs, ivs) (idt, ty) 
      | isTestSuite ty = catchIE
          (do ts <- interpret (prettyPrint $ qualIdt mn idt) (as :: TestSuite)
              return ((idt, ts) : vs, ivs)
          ) (const $ return (vs, (idt, [interpErr idt]) : ivs))
      | otherwise = return (vs, ivs)
    
    interpErr idt = TestSuiteErr $ "could not interpret " ++ idt ++ " as a TestSuite."


checkFullTestSuites :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
checkFullTestSuites mn inps = do 
  (valids, invalids) <- foldM check ([], []) (_testSuites inps)
  return inps { _invalidTestSuites = _invalidTestSuites inps ++ invalids
              , _testSuites = valids
              }
  where 
    check 
      :: MonadInterpreter m 
      => ([(Id, TestSuite)], [(Id, [InputError])])
      -> (Id, TestSuite)
      -> m ([(Id, TestSuite)], [(Id, [InputError])])
    check (vs, ivs) (idt, ts) = catchAll 
      (do !_ <- interpret (qualCheckFun ++ " " ++ (prettyPrint $ qualIdt mn idt)) (as :: ())
          return ((idt, ts) : vs, ivs)
      ) (const $ return (vs, (idt, [inputErr]) : ivs))
    
    qualCheckFun = "AutoBench.DynamicInstanceChecks.checkInitialisedTestSuite"
    inputErr = TestSuiteErr "One or more record fields are uninitialised."


checkValidTestData :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
checkValidTestData mn inps = do 
  (validsUn,  invalidsUn)  <- foldM (check qualCheckFunUn)  ([], []) (_unaryData  inps)
  (validsBin, invalidsBin) <- foldM (check qualCheckFunBin) ([], []) (_binaryData inps)
  return inps { _invalidData = invalidsUn ++ invalidsBin
              , _unaryData   = validsUn
              , _binaryData  = validsBin
              }
  where 
    check 
      :: MonadInterpreter m 
      => String
      -> ([(Id, HsType)], [(Id, HsType, [InputError])])
      -> (Id, HsType)
      -> m ([(Id, HsType)], [(Id, HsType, [InputError])])
    check qualCheckFun (vs, ivs) (idt, ty) = catchIE 
      (do size <- interpret (qualCheckFun ++ " " ++ (prettyPrint $ qualIdt mn idt)) (as :: Int)
          if size >= minInputs
          then return ((idt, ty) : vs, ivs)
          else return (vs, (idt, ty, [sizeErr]) : ivs)
      ) (\e -> return (vs, (idt, ty, [DataOptsErr $ show e]) : ivs))
    
    qualCheckFunUn  = "AutoBench.DynamicInstanceChecks.checkSizeUnaryTestData"
    qualCheckFunBin = "AutoBench.DynamicInstanceChecks.checkSizeBinaryTestData"

    sizeErr = DataOptsErr $ "A minimum of " ++ show minInputs ++ " distinctly sized test inputs are required."







-- * Helpers 

-- | Compiler needs the error's type information.
catchIE :: MonadInterpreter m => m a -> (InterpreterError -> m a) -> m a
catchIE  = catch