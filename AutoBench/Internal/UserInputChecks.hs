
{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE FlexibleContexts #-}

{-|

  Module      : AutoBench.Internal.UserInputChecks
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
  
  As such, the system performs static ('AutoBench.Internal.StaticChecks') and 
  dynamic validation and classification ('AutoBench.Internal.DynamicChecks') 
  of user inputs.

  This module is responsible for coordinating the overall checking process.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   -
-}

module AutoBench.Internal.UserInputChecks 
  ( 
    userInputCheck        -- Parse, validate and classify user inputs.
  , qCheckTestPrograms    -- Check whether test programs are semantically equal using QuickCheck.

  ) where 

import           Control.Category          ((>>>))
import           Control.Monad             ((>=>), filterM, foldM, void)
import           Control.Monad.Catch       (catch, catchAll)
import qualified Criterion.Types           as Criterion
import           Data.List                 ( (\\), groupBy, intersect, nub, sort
                                           , sortBy )
import           Data.Ord                  (comparing)
import           Data.Typeable             (Typeable)
import qualified Text.PrettyPrint.HughesPJ as PP

import Language.Haskell.Interpreter 
  (InterpreterError
  , MonadInterpreter
  , as
  , eval
  , interpret
  )

import AutoBench.Internal.AbstractSyntax
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
import AutoBench.Internal.StaticChecks 
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
import AutoBench.Internal.Hint  
  ( extractElemsAndTypes
  , loadFileSetTopLevelModule
  , loadFileSetTopLevelModuleWithHelpers
  )
import AutoBench.Internal.Types 
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
import AutoBench.Internal.Utils (allEq, filepathToModuleName, notNull)


-- * Top-level 

-- | Parse, validate and classify user inputs.
userInputCheck :: MonadInterpreter m => FilePath -> m UserInputs 
userInputCheck fp  = do 
  let mn = filepathToModuleName fp
  -- Load the user input file.
  loadFileSetTopLevelModule fp
  -- Extract user inputs.
  inps <- extractUserInputs fp
  -- First static checks/categorising.
  let fstStInps = firstStatic inps
  loadFileSetTopLevelModuleWithHelpers fp ["AutoBench.Internal.DynamicChecks"]            -- Use AutoBench.Internal.DynamicChecks dynamically.
  -- First dynamic checks/categorising.
  fstDynInps <- firstDynamic mn fstStInps
  -- Second static checks.
  let sndStInps = secondStatic fstDynInps
  -- Return processed user inputs.
  return sndStInps
  where 
    -- First phase of static checking.
    firstStatic = 
      catValidInvalidElems     -- 1. /Typeable/, 2. /Unqualified/, 3. /Function/.
        >>> catArityFuns       -- 4. /NullaryFun/, 5. /UnaryFun/, 6. /BinaryFun/.
        >>> catGenableFuns     -- 7. /Genable/.
        >>> catTestData        -- 8. /UnaryData/, 9. /BinaryData/.

    -- Second phase of static checking.
    secondStatic = 
      checkTestSuites          -- 10. /ValidTestSuites/.
        >>> expandTestSuites   -- 11. /ExpandValidTestSuites/.
                   
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
-- Note: Size information cannot be recorded until the dynamic checking phase.
catTestData :: UserInputs -> UserInputs
catTestData inps = inps { _unaryData = uns, _binaryData = bins }
  where
    (uns, bins) = foldr cat ([], []) (_nullaryFuns inps) 

    cat 
      :: (Id, HsType)
      -> ([(Id, HsType, [Int])], [(Id, HsType, [(Int, Int)])])
      -> ([(Id, HsType, [Int])], [(Id, HsType, [(Int, Int)])])
    cat (idt, ty) (us, bs) 
      | isUnaryTestData  ty = ((idt, ty, []) : us, bs)
      | isBinaryTestData ty = (us, (idt, ty, []) : bs)
      | otherwise           = (us, bs)

-- ** Second phase

-- | Validate test suites in the '_testSuites' list according to
-- 10. /ValidTestSuites/. The following checks are performed:
--
-- If '_progs' list is populated:
--   * No programs missing from the '_progs' list;
--   * No duplicates in the '_progs' list;
--   * Programs in the '_progs' list have the same type;
--   * Programs in the '_progs' list can be benchmarked;
--   * If '_nf' then programs in the '_progs' list have result types that can be 
--     evaluated to normal form (member of the 'NFData' type class);
--    * If 'Gen' then programs in the '_progs' list have input types that are 
--      members of the 'Arbitrary' type class;
-- If '_progs' list is empty:
--   * Depending on test suite settings, check that at least one program is: 
--     NF-able\/Gen-able and Benchmarkable: see 'checkAllFuns'.
--
-- Remaining checks in both cases: 
--
-- * Validate 'DataOpts': for 'Manual' data, check that it is present and has 
--   the correct type; for 'Gen', check the size range is valid and gives the 
--   correct number of test inputs (>= 'minInputs');
-- * Validate 'AnalOpts': ensure linear models have <= maxParameters,
--   check values of cross-validation parameters are in correct range,
--   check number of top models is strictly positive;
-- * Check Criterion's configuration;
-- * Check GHC flags are valid.
--
-- The '_testSuites' list is updated accordingly and test suites that are 
-- invalid are added to the '_invalidTestSuites' list with one or more 
-- 'InputError's related to the above checks.
checkTestSuites :: UserInputs -> UserInputs
checkTestSuites inps = 
  inps { _invalidTestSuites = _invalidTestSuites inps ++ invalids 
       , _testSuites = valids 
       }
  where
    (valids, invalids) = foldr check ([], []) (_testSuites inps)

    -- Partition valid/invalid test suites.
    check 
      :: (Id, TestSuite)
      -> ([(Id, TestSuite)], [(Id, [InputError])])
      -> ([(Id, TestSuite)], [(Id, [InputError])])
    check (idt, ts) (vs, ivs) = case checkValidTestSuite ts of 
      []   -> ((idt, ts) : vs, ivs)
      errs -> (vs, (idt, errs) : ivs)

    -- Check a single test suite.
    checkValidTestSuite :: TestSuite -> [InputError]
    checkValidTestSuite ts = 
      let ps     = _progs ts             -- Programs in the '_progs' list.
          ps'    = nub ps                -- Programs in the '_progs' list: no duplicates.
          gen    = case _dataOpts ts of  -- Is test data to be generated? 
                    Gen{}    -> True
                    Manual{} -> False
          -- Program specified the '_progs' list that are defined in the input file.
          ps'' = ps \\ (ps \\ fmap fst (unaryFuns ++ binaryFuns))
          -- All input types of programs in user input file that satisfy the 'TestSuite's settings:
          tyInps 
            -- Empty '_progs' list => consider input types of all programs in file.
            | null ps && _nf ts = fmap (tyFunInps . snd) (benchFuns `intersect` nfFuns) 
            | null ps           = fmap (tyFunInps . snd) benchFuns
            -- Non-empty '_progs' list => consider input types of only programs in '_progs' list.
            -- Note: if programs in '_progs' list have different types, an error will be raised elsewhere.
            | _nf ts    = fmap (tyFunInps . snd) $ filter (\(idt, _) -> idt `elem` ps) $ benchFuns `intersect` nfFuns
            | otherwise = fmap (tyFunInps . snd) $ filter (\(idt, _) -> idt `elem` ps) $ benchFuns
      in      
           -- '_progs' list is not empty:
           (if notNull ps 
           then progsMiss ps'                                -- Missing programs in '_progs' list?
                  ++ progsDupes ps                           -- Duplicate programs in '_progs' list?                
                  ++ progsTypes ps'                          -- Same types in '_progs' list?
                  ++ progsBench ps''                         -- Benchmarkable in '_progs' list?
                  ++ (if _nf ts then progsNf  ps'' else [])  -- NF-able is '_progs' list?
                  ++ (if gen    then progsArb ps'' else [])  -- Gen-able is '_progs' list?
           else [])
           -- '_prog' list is empty:
           ++ (if null ps                                    -- Check all functions in input file if '_progs' list is empty:
              then checkAllFuns (_nf ts) gen                 -- checkAllFuns nf? gen?
                     (fmap fst nfFuns)                       -- NF-able.
                     (fmap fst arbFuns)                      -- Gen-able.
                     (fmap fst benchFuns)                    -- Benchmarkable.
              else [])           
           ++ checkValidDataOpts ps'' tyInps (_dataOpts ts)  -- Valid 'DataOpts'.
           ++ checkValidAnalOpts (_analOpts ts)              -- Valid 'AnalOpts'.
           ++ checkCritCfg (_critCfg ts)                     -- Valid Criterion configuration?? <TO-DO>
           ++ checkBaseLine (_baseline ts) (_nf ts)          -- Valid baseline option?
           ++ checkGhcFlags (_ghcFlags ts)                   -- Valid GHC flags?? <TO-DO>
      where
        -- Ensure all programs specified in the '_progs' list are defined
        -- in the input file.
        progsMiss idts = let diff = idts \\ fmap fst (unaryFuns ++ binaryFuns) in
          if null diff
          then []
          else [progsMissErr diff]

        -- Ensure the '_progs' list contains no duplicates.
        progsDupes idts 
          | length (nub idts) == length idts = []
          | otherwise = [progsDupesErr]

        -- Ensure programs in the '_progs' list have the same type.
        progsTypes idts = 
          let tys = filter (\(idt, _) -> idt `elem` idts) (unaryFuns ++ binaryFuns) 
          in if allEq (fmap snd tys) 
             then []
             else [progsTypesErr]

        -- Ensure all programs in the '_progs' list that are defined in the 
        -- input file can be benchmarked.
        progsBench idts = let diff = idts \\ fmap fst benchFuns in
          if null diff
          then []
          else [progsBenchErr diff]

        -- If the '_nf' option is selected, ensure all programs in the 
        -- '_progs' list that are defined in the input file have result types 
        -- that can be evaluated to normal form. I.e., result type is a member 
        -- of the NFData type class.
        progsNf idts = let diff = idts \\ fmap fst nfFuns in
          if null diff 
          then []
          else [progsNfErr diff]

        -- If the 'DataOpts' 'Gen' setting is selected, ensure all programs in 
        -- the '_progs' list that are defined in the input file have input 
        -- types that are members of the 'Arbitrary' type class.
        progsArb idts = let diff = idts \\ fmap fst arbFuns in
          if null diff 
          then []
          else [progsArbErr diff]

        -- If the '_progs' list is empty then ensure at least one program in 
        -- the input file satisfies all test suite settings:
        -- checkAll nf? gen? NF-able Gen-able Benchmarkable ...
        checkAllFuns :: Bool -> Bool -> [Id] -> [Id] -> [Id] -> [InputError]
        checkAllFuns True True nfIdts arbIdts benchIdts                          -- Need to be NF-able, Gen-able, and Benchmarkable.
          | null (nfIdts `intersect` arbIdts `intersect` benchIdts) = [checkAllNFArbErr]
          | otherwise = []
        checkAllFuns True False nfIdts _ benchIdts                               -- Need to be NF-able and Benchmarkable.
          | null (nfIdts `intersect` benchIdts) = [checkAllNfErr]
          | otherwise = []
        checkAllFuns False True _ arbIdts benchIdts                              -- Need to be Gen-able and Benchmarkable.
          | null (arbIdts `intersect` benchIdts) = [checkAllArbErr]
          | otherwise = []
        checkAllFuns False False _ _ benchIdts                                   -- Need to be Benchmarkable.
          | null benchIdts = [checkAllBenchErr]
          | otherwise = []

        -- Validate 'DataOpts':
        -- If the 'Manual' option is selected, ensure the test data is present, 
        -- and has the correct type w.r.t. the input types of testable programs.
        checkValidDataOpts :: [Id] -> [HsType] -> DataOpts -> [InputError]
        checkValidDataOpts _ tyInps (Manual idt) =
          -- Find specified test data in 'UserInputs' data structure:
          case lookup idt (unaryData ++ binaryData) of
            -- Missing:
            Nothing -> [dOptsMissErr idt]
            -- Check test data type matches input types of testable programs:
            Just ty -> if testDataTyFunInps ty `elem` tyInps 
                       then []
                       else [dOptsWrongTyErr]
        
        -- If the 'Gen' option is selected, make sure the size range parameters
        -- @l s u@ are valid. If the 'TestSuite's '_progs' list is empty, then 
        -- the system /cannot/ determine whether the size range is valid because
        -- it depends if it is being used for unary or binary test programs, 
        -- this is checked at a later stage in 'expandTestSuites'. If the 
        -- '_progs' list is populated, then can check: lookup whether the 
        -- test programs in the list are unary or binary and calculate 
        -- from there.
        checkValidDataOpts ps _ (Gen l s u) 
          | l < 0 || s <= 0 || u <= 0 = [dOptsParErr]     -- l >= 0, s > 0, u > 0.
          | notNull ps = case lookup (head ps) unaryFuns of  
          -- Just check the head because if different types in '_progs' list 
          -- then a different error will occur elsewhere.
              -- Given size range is for unary test programs.
              Just{} 
                | (u - l) `div` s + 1 < minInputs -> [dOptsSizeErr]   -- Size range >= minInputs.
                | otherwise -> []
              Nothing -> case lookup (head ps) binaryFuns of
                -- Given size range is for binary test programs.
                Just{} 
                  | ((u - l) `div` s + 1) ^ (2 :: Int) < minInputs -> [dOptsSizeErr] -- (Size range)^2 >= minInputs.
                  | otherwise -> []
                Nothing -> [dOptsMissGenErr $ head ps]
          | otherwise = []
        
        -- Valid 'AnalOpts':
        -- Ensure the linear models have <= maximum number of allowed 
        -- predictors. Check the '_cvIters', '_cvTrain', and '_topModels' values 
        -- are in the correct range.
        checkValidAnalOpts :: AnalOpts -> [InputError]
        checkValidAnalOpts aOpts = 
          checkModels (_linearModels aOpts) 
            ++ checkCVIters   (_cvIters   aOpts) 
            ++ checkCVTrain   (_cvTrain   aOpts)
            ++ checkTopModels (_topModels aOpts)
          where 
            -- Maximum number of predictors for linear models.
            checkModels :: [LinearType] -> [InputError]
            checkModels ls 
              | maxPredictors >= maximum (fmap numPredictors ls) = []
              | otherwise = [aOptsModelErr]
            
            -- 100 <= '_cvIters' 500.
            checkCVIters n 
              | n >= minCVIters && n <= maxCVIters = []
              | otherwise = [aOptsCVItersErr]
            
            -- 0.5 <= '_cvTrain' 0.8.
            checkCVTrain n 
              | n >= minCVTrain && n <= maxCVTrain = []
              | otherwise = [aOptsCVTrainErr]

            -- 'topModels' strictly positive.
            checkTopModels n 
              | n > 0 = []
              | otherwise = [aOptsTopModelsErr]

        -- Check Criterion's configuration??
        checkCritCfg :: Criterion.Config -> [InputError]                                             -- <TO-DO>
        checkCritCfg  = const []  

        -- Check baseline is only being used with nf.
        -- checkBaseLine bl? nf?
        checkBaseLine :: Bool -> Bool -> [InputError] 
        checkBaseLine True False = [tsBaselineErr]
        checkBaseLine _ _ = []

        -- Check the GHC compiler flags??
        checkGhcFlags :: [String] -> [InputError]                                                    -- <TO-DO>
        checkGhcFlags  = const []

    -- Cross-referencing fields in the 'UserInputs' data structure:
    unaryFuns  = _unaryFuns  inps 
    binaryFuns = _binaryFuns inps
    benchFuns  = _benchFuns  inps
    nfFuns     = _nfFuns     inps
    arbFuns    = _arbFuns    inps
    unaryData  = fmap (\(idt, ty, _) -> (idt, ty)) $ _unaryData  inps 
    binaryData = fmap (\(idt, ty, _) -> (idt, ty)) $ _binaryData inps

    -- Errors:
    -- '_progs' list:
    progsMissErr  diff   = TestSuiteErr $ "Cannot locate programs specified in the '_progs' list: " ++ show diff ++ "."
    progsDupesErr        = TestSuiteErr "One or more duplicate programs specified in the '_progs' list."
    progsTypesErr        = TestSuiteErr "Programs specified in the '_progs' list have different types."
    progsBenchErr diff   = TestSuiteErr $ "One or more programs specified in the '_progs' list cannot be benchmarked: " ++ show diff ++ "."
    progsNfErr    diff   = TestSuiteErr $ "The results of one or more benchmarkable programs specified in the '_progs' list cannot be evaluated to normal form:" ++ show diff ++ "."
    progsArbErr   diff   = TestSuiteErr $ "Test data cannot be generated for one or more benchmarkable programs specified in the '_progs' list: " ++ show diff ++ "."
    -- Other 'TestSuite' errors:
    checkAllNFArbErr     = TestSuiteErr "There are no benchmarkable programs specified in the input file whereby their results can be evaluated to normal and for which test data can be generated." 
    checkAllNfErr        = TestSuiteErr "The results of all benchmarkable programs specified in the input file cannot be evaluated to normal form."
    checkAllArbErr       = TestSuiteErr "Test data cannot be generated for any benchmarkable programs specified in the input file."
    checkAllBenchErr     = TestSuiteErr "None of the programs in the input file can be benchmarked."
    tsBaselineErr        = TestSuiteErr "The baseline option can only be used when test cases are being fully evaluated."
    -- 'DataOpts':
    dOptsMissErr    idt  = DataOptsErr $ "Specified test data is invalid or missing: '" ++ idt ++ "'."
    dOptsMissGenErr idt  = DataOptsErr $ "Cannot determine the size range of generated test data due to missing test programs specified in the '_progs' list: " ++ idt ++ "."
    dOptsWrongTyErr      = DataOptsErr "The type of the specified test data is incompatible with the types of testable programs."
    dOptsParErr          = DataOptsErr "Invalid values for 'Gen' bounds and/or step." 
    dOptsSizeErr         = DataOptsErr $ "A minimum of " ++ show minInputs ++ " distinctly sized test inputs are required."
    -- 'AnalOpts':
    aOptsModelErr        = AnalOptsErr $ "Linear regression models can have a maximum of " ++ show maxPredictors ++ " predictors."
    aOptsCVItersErr      = AnalOptsErr $ "The number of cross-validation iterators must be " ++ show minCVIters ++ " <= x <= " ++ show maxCVIters ++ "." 
    aOptsCVTrainErr      = AnalOptsErr $ "The percentage of cross-validation training data must be " ++ show minCVTrain ++ " <= x <= " ++ show maxCVTrain ++ "." 
    aOptsTopModelsErr    = AnalOptsErr $ "The number of models to review must be strictly positive."

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
    qualCheckFunUn  = "AutoBench.Internal.DynamicChecks.checkNFDataInputUn"
    qualCheckFunBin = "AutoBench.Internal.DynamicChecks.checkNFDataInputBin"

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
    qualCheckFunUn  = "AutoBench.Internal.DynamicChecks.checkNFDataResultUn"
    qualCheckFunBin = "AutoBench.Internal.DynamicChecks.checkNFDataResultBin"

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
    qualCheckFunUn  = "AutoBench.Internal.DynamicChecks.checkArbitraryUn"
    qualCheckFunBin = "AutoBench.Internal.DynamicChecks.checkArbitraryBin"

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
    
    interpErr idt = TestSuiteErr $ "Could not interpret " ++ idt ++ " as a TestSuite."

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
    
    qualCheckFun = "AutoBench.Internal.DynamicChecks.checkInitialisedTestSuite"
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
      :: (MonadInterpreter m, Typeable [a], Ord a)
      => String
      -> ([(Id, HsType, [a])], [(Id, HsType, [InputError])])
      -> (Id, HsType, [a])
      -> m ([(Id, HsType, [a])], [(Id, HsType, [InputError])])
    check qualCheckFun (vs, ivs) (idt, ty, _) = catchIE 
      (do sizes <- interpret (qualCheckFun ++ " " ++ (prettyPrint $ qualIdt mn idt)) as
          if (length $ nub sizes) >= minInputs
          then return ((idt, ty, sort sizes) : vs, ivs)
          else return (vs, (idt, ty, [sizeErr]) : ivs)
      ) (\e -> return (vs, (idt, ty, [DataOptsErr $ show e]) : ivs))

    qualCheckFunUn  = "AutoBench.Internal.DynamicChecks.sizeUnaryTestData"
    qualCheckFunBin = "AutoBench.Internal.DynamicChecks.sizeBinaryTestData"

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

-- Expand valid test suites input by the user that have empty '_progs' lists. 
--           
-- Background:                              
-- Users can provide empty '_progs' lists in their test suites to instruct the 
-- system to generate all valid options based on the remainder of the test 
-- suite's settings. In practice, this means the system has to generate 
-- zero or more test suites for every test suite of this form.
--
-- The main complication is ensuring that, when the '_progs' list is populated
-- in the expansion, the test programs selected must be compatible with 
-- user-specified test data, if applicable. In addition, if the user has opted 
-- for test data to be generated automatically, then the size range must be 
-- checked depending on whether the test programs are unary or binary.
expandTestSuites :: UserInputs -> UserInputs
expandTestSuites inps = 
  let (valids, invalids) = foldr expand ([], []) (_testSuites inps)
  in inps { _testSuites        = valids
          , _invalidTestSuites = _invalidTestSuites inps ++ invalids 
          }
  where 
    -- The system may generate one or more valid test suites or an invalid test 
    -- suite if expansion is not possible due to a 'DataOpts' error.
    expand 
      :: (Id, TestSuite) 
      -> ([(Id, TestSuite)], [(Id, [InputError])])
      -> ([(Id, TestSuite)], [(Id, [InputError])])
    expand (idt, ts) (valids, errs) 
      -- If '_progs' list is populated, don't expand.
      | notNull (_progs ts) = ((idt, ts) : valids, errs)
      
      -- If '_progs' list is empty, need to expand:
      
      | _nf ts && gen = case genTestSuites $                                               -- Nf, Gen, and Benchmarkable: size range match.
          matchWithGenSize (_dataOpts ts) True benchNfArbUnFunsGpd ++
          matchWithGenSize (_dataOpts ts) False benchNfArbBinFunsGpd of 
            []  -> (valids, (idt, [dOptsSizeErr]) : errs)
            tss -> (tss ++ valids, errs)
      | _nf ts = case genTestSuites $ matchWithTestData (_dataOpts ts)                     -- Nf and Benchmarkable: manual match.
          benchNfFunsGpd of 
            []  -> (valids, (idt, [dOptsWrongTyErr]) : errs)
            tss -> (tss ++ valids, errs) 
      | gen = case genTestSuites $                                                         -- Gen and Benchmarkable: gen size range match.
          matchWithGenSize (_dataOpts ts) True benchArbUnFunsGpd ++
          matchWithGenSize (_dataOpts ts) False benchArbBinFunsGpd of 
            []  -> (valids, (idt, [dOptsWrongTyErr]) : errs)
            tss -> (tss ++ valids, errs)
      | otherwise = case genTestSuites $ matchWithTestData (_dataOpts ts)                  -- All Benchmarkable: manual match.
          benchFunsGpd of 
            []  -> (valids, (idt, [dOptsWrongTyErr]) : errs)
            tss -> (tss ++ valids, errs)
      where 

        -- Whether the test suite requires generated test data.
        gen = case _dataOpts ts of  
          Gen{}    -> True
          Manual{} -> False

        -- For each /new/ '_progs' list, replicate the test suite's prior 
        -- settings. This is how we perform the expansion. 
        -- At the end every test suite will have a populated '_progs' list.
        genTestSuites :: [[Id]] -> [(Id, TestSuite)]
        genTestSuites  = fmap (\idts -> 
          ( idt
          , TestSuite 
              { _progs    = idts  -- Add a new '_progs' list.
              -- Everything else remains the same.
              , _dataOpts = _dataOpts ts 
              , _analOpts = _analOpts ts
              , _critCfg  = _critCfg  ts 
              , _baseline = _baseline ts 
              , _nf       = _nf       ts 
              , _ghcFlags = _ghcFlags ts
              }
          ))

        -- Match the Gen size range with the minimum input requirements of unary
        -- and binary test programs. 
        matchWithGenSize :: DataOpts -> Bool -> [[(Id, HsType)]] -> [[Id]]
        matchWithGenSize Manual{} _ _ = []
        matchWithGenSize (Gen l s u) True validFuns  -- True for unary test programs.
          | (u - l) `div` s + 1 < minInputs = []
          | otherwise = fmap (fmap fst) validFuns
        matchWithGenSize (Gen l s u) False validFuns -- False for binary test programs.
          | ((u - l) `div` s + 1) ^ (2 :: Int) < minInputs = []
          | otherwise = fmap (fmap fst) validFuns

        -- Match the /input type/ of each function with the type of test data.
        matchWithTestData :: DataOpts -> [[(Id, HsType)]] -> [[Id]]
        matchWithTestData Gen{} _ = [] -- Shouldn't happen.
        matchWithTestData (Manual s) validFuns = case lookup s testData of
            Nothing -> [] -- Shouldn't happen.
            Just ty -> fmap (fmap fst) $ filter (\xs -> match (snd $ head xs) ty) validFuns
          where match fTy dTy = tyFunInps fTy == testDataTyFunInps dTy

        -- Data options errors:
        dOptsSizeErr    = DataOptsErr $ "A minimum of " ++ show minInputs ++ " distinctly sized test inputs are required."
        dOptsWrongTyErr = DataOptsErr "The type of the specified test data is incompatible with the types of testable programs."

    -- Groupings by type: to match against the type of user-specified test data.
    benchNfArbUnFunsGpd  = groupBy (\x1 x2 -> snd x1 == snd x2) $ sortBy (comparing snd) benchNfArbUnFuns
    benchNfArbBinFunsGpd = groupBy (\x1 x2 -> snd x1 == snd x2) $ sortBy (comparing snd) benchNfArbBinFuns
    benchArbUnFunsGpd    = groupBy (\x1 x2 -> snd x1 == snd x2) $ sortBy (comparing snd) benchArbUnFuns
    benchArbBinFunsGpd   = groupBy (\x1 x2 -> snd x1 == snd x2) $ sortBy (comparing snd) benchArbBinFuns
    benchNfFunsGpd       = groupBy (\x1 x2 -> snd x1 == snd x2) $ sortBy (comparing snd) benchNfFuns
    benchFunsGpd         = groupBy (\x1 x2 -> snd x1 == snd x2) $ sortBy (comparing snd) benchFuns

    -- Cross-referencing from the 'UserInputs':
    benchNfArbUnFuns  = benchFuns `intersect` nfFuns `intersect` arbFuns `intersect` unaryFuns
    benchNfArbBinFuns = benchFuns `intersect` nfFuns `intersect` arbFuns `intersect` binaryFuns
    benchArbUnFuns    = benchFuns `intersect` arbFuns `intersect` unaryFuns
    benchArbBinFuns   = benchFuns `intersect` arbFuns `intersect` binaryFuns
    benchNfFuns       = benchFuns `intersect` nfFuns
    
    -- Projections from 'UserInputs':
    benchFuns  = _benchFuns  inps
    nfFuns     = _nfFuns     inps
    arbFuns    = _arbFuns    inps
    unaryFuns  = _unaryFuns inps 
    binaryFuns = _binaryFuns inps 
    testData   = fmap (\(idt, ty, _) -> (idt, ty)) (_unaryData inps) ++ 
                 fmap (\(idt, ty, _) -> (idt, ty)) (_binaryData inps)

-- ** QuickCheck testing 

-- | Check whether test programs are semantically equal using QuickCheck.
-- Note: this check is only available for 'TestSuite's that use generated test 
-- data.
qCheckTestPrograms 
  :: MonadInterpreter m 
  => FilePath   -- User input file.
  -> [String]   -- Names of test programs.
  -> UserInputs -- User inputs data structure (for cross-referencing).
  -> m Bool 
qCheckTestPrograms _ [] _ = return False
qCheckTestPrograms fp ps inps = do 
  let mn  = filepathToModuleName fp
      ps' = fmap (prettyPrint . qualIdt mn) ps -- Qualify the programs with module name to avoid ambiguity.
  -- Load the user input file with dynamic check helper module.
  loadFileSetTopLevelModuleWithHelpers fp ["AutoBench.Internal.DynamicChecks"]
  catchIE ((interpret $ qCheck ps') (as :: Bool))
  -- If anything goes wrong, just assume the programs aren't semantically equal.
          (const $ return False) 
  where 
    qCheck ps'
    -- Check whether the test programs are unary or binary by 
    -- cross-referencing the 'UserInputs' data structure.
    -- Construct the appropriate function call to do the checking.
      | (head ps) `elem` unaryFuns  = PP.render $ qCheckUn  PP.<+> ppList (fmap PP.text ps') -- Unary test programs.
      | (head ps) `elem` binaryFuns = PP.render $ qCheckBin PP.<+> ppList (fmap PP.text ps') -- Binary test programs.
      | otherwise = "False" -- Shouldn't happen at this stage due to prior validation checks.

    -- *** Don't edit these function names ***
    qCheckUn  = PP.text "AutoBench.Internal.DynamicChecks.quickCheckUn"
    qCheckBin = PP.text "AutoBench.Internal.DynamicChecks.quickCheckBin"

    -- Pretty print a comma-separated list.
    ppList :: [PP.Doc] -> PP.Doc
    ppList docs = PP.hcat $ 
      PP.char '[' : (PP.punctuate (PP.text ", ") docs) ++ [PP.char ']']

    -- Projections: names of unary/binary functions in user input file.
    unaryFuns  = fmap fst $ _unaryFuns  inps
    binaryFuns = fmap fst $ _binaryFuns inps