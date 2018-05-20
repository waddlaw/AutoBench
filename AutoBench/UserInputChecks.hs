
{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

{-|

  Module      : AutoBench.UserInputChecks
  Description : Interpreting, validating, and classifying user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  User inputs are always passed to the system using files (Haskell modules), 
  which must be interpreted and validated. Test suites ('TestSuites') are 
  AutoBench's principle user input datatype, and are used to structure 
  performance tests into logical units. Thus, input files will typically 
  contain one or more test suites. In addition, input files will contain test 
  programs (those whose performance is to be analysed/compared) and possibly 
  user-specified test data.

  As user inputs have a number of different purposes, but are all specified
  in the same way (i.e., inside files), the system must not only validate 
  user inputs appropriately, but it must also classify them according to their
  purpose: test suites, test programs, test data, etc.

  The classification of user inputs is a non-trivial process, due to the fact 
  that, for example, any program to be tested using the system must satisfy the 
  following properties,

  * Be a unary or binary function;
  * Have an input type that is a member of the 'NFData' type class;
  * Have an input type that is a member of the 'Arbitrary' type class 
     OR
    Be associated with valid user-specified test data;
  * Be referred to by a valid test suite;

  which in themselves rely on the classification of \'unary functions\', 
  \'valid test data\', \'valid test suites\', \'functions whose input types are 
  members of the 'NFData' type class\', etc.
  
  Two different classification procedures are required. For example, a type 
  signature can be used to determine whether a function is unary, binary, or 
  otherwise. However, to the best of my knowledge, there is no generic way of 
  checking whether a function's input type is a member of the 'NFData' type 
  class aside from performing a dynamic test, because it depends on which 
  type class instances are in scope. Therefore, user inputs can be classified 
  according the property \'is a unary function\' /statically/, but only 
  /dynamic/ checks can be used to classify functions according to the property 
  \'has input types that are members of the 'NFData' type class\'.
  
  As such, the system performs static ('AutoBench.StaticChecks') and dynamic 
  validation and classification ('AutoBench.DynamicChecks') of user inputs.

  This module is responsible for coordinating the overall checking process.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.UserInputChecks (userInputCheck) where 

import           Control.Category       ((>>>))
import           Data.List              ((\\), intersect, nub)
import           Control.Monad          ((>=>), filterM, foldM, void)
import           Control.Monad.Catch    (catch, catchAll)
import qualified Criterion.Types        as Criterion

import Language.Haskell.Interpreter 
  (InterpreterError
  , MonadInterpreter
  , as
  , eval
  , interpret
  )

import AutoBench.AbstractSyntax
  ( HsType
  , Id
  , ModuleElem(..)
  , ModuleName
  , TypeString
  , prettyPrint
  , qualIdt
  , tyFunInps
  , unqualTyToTy
  )
import AutoBench.StaticChecks 
  ( isABGenTyFun
  , isABTyFun
  , isBinaryTestData
  , isBinaryTyFun
  , isNullaryTyFun
  , isTestSuite
  , isUnaryTestData
  , isUnaryTyFun
  , isUnqualQualTy
  , parseTySig
  , testDataTyFunInps
  )
import AutoBench.Hint  
  ( extractElemsAndTypes
  , loadFileSetTopLevelModule
  , loadFileSetTopLevelModuleWithHelpers
  )
import AutoBench.Types 
  ( AnalOpts(..)
  , DataOpts(..)
  , InputError(..)
  , LinearType
  , TestSuite(..)
  , UserInputs(..)
  , initUserInputs
  , maxCVIters
  , maxCVTrain
  , maxPredictors
  , minCVIters
  , minCVTrain
  , minInputs
  , numPredictors
  )
import AutoBench.Utils (allEq, filepathToModuleName, notNull)

-- makeLenses ''UserInputs

-- * Top-level 

userInputCheck :: MonadInterpreter m => FilePath -> m UserInputs 
userInputCheck fp  = do 
  let mn = filepathToModuleName fp
  -- Load the user input file.
  loadFileSetTopLevelModule fp
  -- Extract user inputs.
  inps <- extractUserInputs fp
  -- First static checks/categorising.
  let fstStInps = firstStatic inps
  loadFileSetTopLevelModuleWithHelpers fp ["AutoBench.DynamicChecks"]            -- Use AutoBench.DynamicChecks dynamically.
  -- First dynamic checks/categorising.
  fstDynInps <- firstDynamic mn fstStInps
  -- Second static checks.
  let sndStInps = secondStatic fstDynInps
  

  return sndStInps
  where 
    -- First phase of static checking.
    firstStatic = 
      catValidInvalidElems     -- 1. /Typeable/, 2. /Unqualified/, 3. /Function/.
        >>> catArityFuns       -- 4. /NullaryFun/, 5. /UnaryFun/, 6. /BinaryFun/.
        >>> catGenableFuns     -- 7. /Genable/.
        >>> catTestData        -- 8. /UnaryData/, 9. /BinaryData/.

    -- Second phase of static checking.
    secondStatic = checkValidTestSuites
                   
    -- First phase of dynamic checking.
    firstDynamic mn = 
      catNFDataInput            mn    -- 1. /NFDataInput/.
        >=> catNFDataResult     mn    -- 2. /NFDataResult/.
        >=> catArbitrary        mn    -- 3. /Arbitrary/.
        >=> interpTestSuites    mn    -- 4. /TestSuites/.
        >=> checkFullTestSuites mn    -- 5. /FullTestSuites/.
        >=> checkValidTestData  mn    -- 6. /ValidUnaryData/, 7. /ValidBinaryData/.

-- * Static checking

-- ** First phase 

-- | Categorise elements in the 'UserInputs' '_allElems' list according to 
-- whether they have types that are 1. /Typeable/, 2. /Unqualified/, and 3.
-- /Function/, in which case they are valid and put into the '_validElems' list
-- of 'UserInputs'. If not then they are '_invalidElems'.
catValidInvalidElems :: UserInputs -> UserInputs
catValidInvalidElems inps = 
  inps { _invalidElems = invalids, _validElems = valids }
  where 
    (valids, invalids) = foldr cat ([], []) (_allElems inps)
    
    cat 
      :: (ModuleElem, Maybe TypeString) 
      -> ([(Id, HsType)], [(ModuleElem, Maybe TypeString)])
      -> ([(Id, HsType)], [(ModuleElem, Maybe TypeString)])
    cat x@(Fun idt, Just tyStr) (vs, ivs) = 
      case parseTySig (idt ++ " :: " ++ tyStr) of 
        -- 1. /Typeable/, 2. /Unqualified/, 3. /Function/ => add to @valids@.
        Just ty | isUnqualQualTy ty && isABTyFun (unqualTyToTy ty) -> 
          ((idt, unqualTyToTy ty) : vs, ivs)
        -- Otherwise add to @invalids@.
        _ -> (vs, x : ivs)
    -- Anything that isn't a 'Fun' => add to @invalids@.
    cat iv (vs, ivs) = (vs, iv : ivs)

-- | Categorise elements in the '_validElems' list according to their arity,
-- i.e., 4. /NullaryFun/, 5. /UnaryFun/, or 6. /BinaryFun/.
--
-- Nullary functions are added to the '_nullaryFuns' list, unary functions to 
-- the '_unaryFuns' list and binary functions to the '_binaryFuns' list.
catArityFuns :: UserInputs -> UserInputs
catArityFuns inps = 
  inps { _nullaryFuns = nuls, _unaryFuns = uns, _binaryFuns = bins }
  where
    (nuls, uns, bins) = foldr cat ([], [], []) (_validElems inps) 

    cat 
      :: (Id, HsType)
      -> ([(Id, HsType)], [(Id, HsType)], [(Id, HsType)])
      -> ([(Id, HsType)], [(Id, HsType)], [(Id, HsType)])
    cat x@(_, ty) (ns, us, bs) 
      | isNullaryTyFun ty = (x : ns, us, bs)
      | isUnaryTyFun   ty = (ns, x : us, bs)
      | isBinaryTyFun  ty = (ns, us, x : bs)
      | otherwise         = (ns, us, bs) 

-- | Categorise elements in the '_validElems' list according to whether they 
-- are 7. /Genable/ (unary/binary functions that do not contain type variables 
-- in their input types). 
--
-- /Genable/ functions are added to the '_arbFuns' list for subsequent dynamic 
-- checking.
catGenableFuns :: UserInputs -> UserInputs
catGenableFuns inps = 
  inps { _arbFuns = filter (isABGenTyFun . snd) (_validElems inps) }

-- | Categorise elements in the '_nullaryFuns' list according to whether they 
-- are 8. /UnaryData/ or 9. /BinaryData/.
--
-- /UnaryData/ is added to the '_unaryData' list. /BinaryData/ is added to the 
-- '_binaryDat' list.
catTestData :: UserInputs -> UserInputs
catTestData inps = inps { _unaryData = uns, _binaryData = bins }
  where
    (uns, bins) = foldr cat ([], []) (_nullaryFuns inps) 

    cat 
      :: (Id, HsType)
      -> ([(Id, HsType)], [(Id, HsType)])
      -> ([(Id, HsType)], [(Id, HsType)])
    cat x@(_, ty) (us, bs) 
      | isUnaryTestData  ty = (x : us, bs)
      | isBinaryTestData ty = (us, x : bs)
      | otherwise           = (us, bs)

-- ** Second phase

-- | Validate test suites in the '_testSuites' list according to
-- 8. /ValidTestSuites/. The following checks are performed:
--
-- * No programs missing from the '_progs' list;
-- * No duplicates in the '_progs' list;
-- * Programs in the '_progs' list have the same type;
-- * Programs in the '_progs' list can be benchmarked;
-- * If '_nf' then programs in the '_progs' list have result types that can be 
--   evaluated to normal form (member of the 'NFData' type class);
-- * If 'Gen' then programs in the '_progs' list have input types that are 
--   members of the 'Arbitrary' type class;
-- * Validate 'DataOpts': for 'Manual' data, check that it is present and has 
--   the correct type; for 'Gen', check the size range is valid and gives the 
--   correct number of test inputs (>= 'minInputs');
-- * Validate 'AnalOpts': ensure linear models have <= maxParameters,
--   check values of cross-validation parameters are in correct range;
-- * Check Criterion's configuration;
-- * Check GHC flags are valid.
--
-- The '_testSuites' list is updated accordingly and test suites that are 
-- invalid are added to the '_invalidTestSuites' list with one or more 
-- 'InputError's.
checkValidTestSuites :: UserInputs -> UserInputs
checkValidTestSuites inps = 
  inps { _invalidTestSuites = _invalidTestSuites inps ++ invalids 
       , _testSuites = valids 
       }
  where
    (valids, invalids) = foldr check ([], []) (_testSuites inps)

    check 
      :: (Id, TestSuite)
      -> ([(Id, TestSuite)], [(Id, [InputError])])
      -> ([(Id, TestSuite)], [(Id, [InputError])])
    check (idt, ts) (vs, ivs) = case checkValidTestSuite ts of 
      []   -> ((idt, ts) : vs, ivs)
      errs -> (vs, (idt, errs) : ivs)

    checkValidTestSuite :: TestSuite -> [InputError]
    checkValidTestSuite ts = 
      let ps     = _progs ts
          ps'    = nub ps
          gen    = case _dataOpts ts of
                    Gen{}    -> True
                    Manual{} -> False 
          tyInps | gen = []
                 | _nf ts    = fmap (tyFunInps . snd) (benchFuns `intersect` nfFuns)
                 | otherwise = fmap (tyFunInps . snd) benchFuns

      in progsMiss ps'                                      -- Missing programs in '_progs' list?
           ++ progsDupes ps                                 -- Duplicate programs in '_progs' list?                
           ++ progsTypes ps'                                -- Same types in '_progs' list?
           ++ progsBench ps'                                -- Benchmarkable?
           ++ (if _nf ts then progsNf  ps' else [])         -- NF-able?
           ++ (if gen    then progsArb ps' else [])         -- Gen-able?
           ++ checkValidDataOpts tyInps (_dataOpts ts)      -- Valid 'DataOpts'
           ++ checkValidAnalOpts (_analOpts ts)             -- Valid 'AnalOpts'
           ++ checkCritCfg (_critCfg ts)                    -- Valid Criterion configuration?? <TO-DO>
           ++ checkGhcFlags (_ghcFlags ts)                  -- Valid GHC flags?? <TO-DO>
      where

        -- Ensure all programs specified in the '_progs' list are defined
        -- in the input file.
        progsMiss []  = []
        progsMiss idts = let diff = idts \\ fmap fst (unaryFuns ++ binaryFuns) in
          if null diff
          then []
          else [progsMissErr diff]

        -- Ensure the '_progs' list contains no duplicates.
        progsDupes idts 
          | length (nub idts) == length idts = []
          | otherwise = [progsDupesErr]

        -- Ensure programs in the '_progs' list have the same type.
        progsTypes [] = []
        progsTypes idts = 
          let tys = filter (\(idt, _) -> idt `elem` idts) (unaryFuns ++ binaryFuns) 
          in if allEq (fmap snd tys) 
             then []
             else [progsTypesErr]

        -- Ensure all programs in the '_progs' list can be benchmarked.
        progsBench []
          | null benchFuns = [progsAllBenchErr]
          | otherwise = []
        progsBench idts = let diff = idts \\ fmap fst benchFuns in
          if null diff
          then []
          else [progsBenchErr diff]

        -- If the 'nf' option is selected, ensure all programs in the '_progs'
        -- list have result types that can be evaluated to normal form.
        -- I.e., result type is a member of the NFData type class.
        progsNf []
          | null (benchFuns `intersect` nfFuns) = [progsAllNfErr]
          | otherwise = []
        progsNf idts = let diff = idts \\ fmap fst (benchFuns `intersect` nfFuns) in
          if null diff 
          then []
          else [progsNfErr diff]

        -- If the 'DataOpts' 'Gen' setting is selected, ensure all programs in 
        -- the '_progs' list have input types that are members of the 
        -- 'Arbitrary' type class.
        progsArb []
          | null (benchFuns `intersect` arbFuns) = [progsAllArbErr]
          | otherwise = []
        progsArb idts = let diff = idts \\ fmap fst (benchFuns `intersect` arbFuns) in
          if null diff 
          then []
          else [progsArbErr diff]

        -- Validate 'DataOpts':
        -- If the 'Manual' option is selected, ensure the test data is present, 
        -- and has the correct type w.r.t.  the input types of test programs.
        -- If the 'Gen' option is selected, make sure the size range is
        -- valid and specifies a sufficient number of test inputs.
        checkValidDataOpts :: [HsType] -> DataOpts -> [InputError]
        checkValidDataOpts tyInps (Manual idt) =
          case lookup idt (unaryData ++ binaryData) of 
            Nothing -> [dOptsMissErr idt]
            Just ty -> if notNull tyInps && testDataTyFunInps ty `notElem` tyInps 
                       then [dOptsWrongTyErr]
                       else []
        checkValidDataOpts _ (Gen l s u) 
          | l <= 0 || s <= 0 || u <= 0      = [dOptsParErr]
          | (u - l) `div` s + 1 < minInputs = [dOptsSizeErr]
          | otherwise = []
        
        -- Valid 'AnalOpts':
        -- Ensure the linear models have leq the maximum number of allowed 
        -- predictors. Check the '_cvIters' and '_cvTrain' values are 
        -- in the correct range.
        checkValidAnalOpts :: AnalOpts -> [InputError]
        checkValidAnalOpts aOpts = 
          checkModels (_linearModels aOpts) 
            ++ checkCVIters (_cvIters aOpts) 
            ++ checkCVTrain (_cvTrain aOpts)
          where 
            checkModels :: [LinearType] -> [InputError]
            checkModels ls 
              | maxPredictors >= maximum (fmap numPredictors ls) = []
              | otherwise = [aOptsModelErr]

            checkCVIters xs 
              | xs >= minCVIters && xs <= maxCVIters = []
              | otherwise = [aOptsCVItersErr]

            checkCVTrain xs 
              | xs >= minCVTrain && xs <= maxCVTrain = []
              | otherwise = [aOptsCVTrainErr]

        -- Check Criterion's configuration??
        checkCritCfg :: Criterion.Config -> [InputError]                                             -- <TO-DO>
        checkCritCfg  = const []  

        -- Check the GHC compiler flags??
        checkGhcFlags :: [String] -> [InputError]                                                    -- <TO-DO>
        checkGhcFlags  = const []


    -- Projections from the 'UserInputs' data structure:

    unaryFuns  = _unaryFuns inps 
    binaryFuns = _binaryFuns inps
    benchFuns  = _benchFuns inps
    nfFuns     = _nfFuns inps
    arbFuns    = _arbFuns inps

    unaryData  = _unaryData inps 
    binaryData = _binaryData inps

    -- Errors:
    
    -- '_progs' list:
    progsMissErr diff   = TestSuiteErr $ "Cannot locate programs specified in the '_progs' list: " ++ show diff ++ "."
    progsDupesErr       = TestSuiteErr "One or more duplicate programs specified in the '_progs' list."
    progsTypesErr       = TestSuiteErr "Programs specified in the '_progs' list have different types."
    progsAllBenchErr    = TestSuiteErr "None of the programs in the input file can be benchmarked."
    progsBenchErr diff  = TestSuiteErr $ "One or more programs specified in the '_progs' list cannot be benchmarked: " ++ show diff ++ "."
    progsAllNfErr       = TestSuiteErr "The results of all benchmarkable programs specified in the input file cannot be evaluated to normal form."
    progsNfErr diff     = TestSuiteErr $ "The results of one or more benchmarkable programs specified in the '_progs' list cannot be evaluated to normal form:" ++ show diff ++ "."
    progsAllArbErr      = TestSuiteErr "Test data cannot be generated for any benchmarkable programs specified in the input file."
    progsArbErr diff    = TestSuiteErr $ "Test data cannot be generated for one or more benchmarkable programs specified in the '_progs' list: " ++ show diff ++ "."
    -- 'DataOpts':
    dOptsMissErr idt    = DataOptsErr $ "Specified test data is invalid or missing: '" ++ idt ++ "'."
    dOptsWrongTyErr     = DataOptsErr "The type of the specified test data is incompatible with the types of testable programs."
    dOptsParErr         = DataOptsErr "All parameters to 'Gen' must be strictly positive." 
    dOptsSizeErr        = DataOptsErr $ "A minimum of " ++ show minInputs ++ " distinctly sized test inputs are required."
    -- 'AnalOpts':
    aOptsModelErr       = AnalOptsErr $ "Linear regression models can have a maximum of " ++ show maxPredictors ++ " predictors."
    aOptsCVItersErr     = AnalOptsErr $ "The number of cross-validation iterators must be " ++ show minCVIters ++ " <= x <= " ++ show maxCVIters 
    aOptsCVTrainErr     = AnalOptsErr $ "The percentage of cross-validation training data must be " ++ show minCVTrain ++ " <= x <= " ++ show maxCVTrain







-- * Dynamic checking

-- | Categorise functions in the '_unaryFuns' and '_binaryFuns' list according
-- to 1. /NFDataInput/, i.e., their input types are members of the 'NFData' 
-- type class, in which case they are added to the '_benchFuns' list.
catNFDataInput :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
catNFDataInput mn inps = do
  benchFunsUn  <- filterM (check qualCheckFunUn)  (_unaryFuns  inps)
  benchFunsBin <- filterM (check qualCheckFunBin) (_binaryFuns inps)
  return inps { _benchFuns = benchFunsUn ++ benchFunsBin }
  where 
    check :: MonadInterpreter m => String -> (Id, HsType) -> m Bool
    check qualCheckFun (idt ,_) = catchIE 
      ((void . eval $ qualCheckFun ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
      (const $ return False)

    -- Functions to perform checks, qualified with module name.
    qualCheckFunUn  = "AutoBench.DynamicChecks.checkNFDataInputUn"
    qualCheckFunBin = "AutoBench.DynamicChecks.checkNFDataInputBin"

-- | Categorise functions in the '_unaryFuns' and '_binaryFuns' list according
-- to 2. /NFDataResult/, i.e., their result types are members of the 'NFData' 
-- type class, in which case they are added to the '_nfFuns' list.
catNFDataResult :: MonadInterpreter m => ModuleName -> UserInputs -> m UserInputs 
catNFDataResult mn inps = do
  nfFunsUn  <- filterM (check qualCheckFunUn)  (_unaryFuns  inps)
  nfFunsBin <- filterM (check qualCheckFunBin) (_binaryFuns inps)
  return inps { _nfFuns = nfFunsUn ++ nfFunsBin }
  where 
    check:: MonadInterpreter m => String -> (Id, HsType) -> m Bool
    check qualCheckFun  (idt ,_) = catchIE 
      ((void . eval $ qualCheckFun ++ " " ++ (prettyPrint $ qualIdt mn idt)) >> return True)
      (const $ return False)

    -- Functions to perform checks, qualified with module name.
    qualCheckFunUn  = "AutoBench.DynamicChecks.checkNFDataResultUn"
    qualCheckFunBin = "AutoBench.DynamicChecks.checkNFDataResultBin"

-- | Categorise functions in the '_arbFuns' list according to 3. /Arbitrary/, 
-- i.e., their input types are members of the 'Arbitrary' type class. 
-- The '_arbFuns' list is updated accordingly.
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
    qualCheckFunUn  = "AutoBench.DynamicChecks.checkArbitraryUn"
    qualCheckFunBin = "AutoBench.DynamicChecks.checkArbitraryBin"

-- | Categorise functions in the '_nullaryFuns' list according to 
-- 4. /TestSuites/. Load and interpret test suites in this list and add them to 
-- the '_testSuites' list. Test suites that cannot be dynamically loaded
-- and throw an interpreter error are added to the '_invalidTestSuites' list.
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

-- | Validate test suites in the '_testSuites' list according to
-- 5. /FullTestSuites/, i.e., ensure that all record fields are initialised. 
-- Test suites with uninitialised record fields are added to the 
-- '_invalidTestSuites' list. The '_testSuites' list is updated accordingly.
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
    
    qualCheckFun = "AutoBench.DynamicChecks.checkInitialisedTestSuite"
    inputErr = TestSuiteErr "One or more record fields are uninitialised/undefined."

-- | Validate test data in the '_unaryData' and '_binaryData' lists according
-- to 6. /ValidUnaryData/ and 7. /ValidBinaryData/, respectively, i.e.,:
--
-- * Test data must have a minimum number of distinctly sized inputs: see
-- 'minInputs'.
--
-- Test data that fails validation is added to the '_invalidData' lists.
-- The 'unaryData' and '_binaryData' lists are updated accordingly. 
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
    
    qualCheckFunUn  = "AutoBench.DynamicChecks.sizeUnaryTestData"
    qualCheckFunBin = "AutoBench.DynamicChecks.sizeBinaryTestData"

    sizeErr = DataOptsErr $ "A minimum of " ++ show minInputs ++ " distinctly sized test inputs are required."

-- * Helpers 

-- | Extract all the definitions in a user input file and initialise the 
-- 'UserInputs' data structure.
extractUserInputs :: MonadInterpreter m => FilePath -> m UserInputs 
extractUserInputs fp = 
  initUserInputs <$> extractElemsAndTypes (filepathToModuleName fp)

-- | Compiler needs the error's type information.
catchIE :: MonadInterpreter m => m a -> (InterpreterError -> m a) -> m a
catchIE  = catch