
{-# OPTIONS_GHC -Wall   #-} 
{-# LANGUAGE LambdaCase #-}


{-|

  Module      : AutoBench.Hint
  Description : Dynamically interpret user input files.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module is responsible for dynamically interpreting user input files.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.Hint
  (
    loadFileSetTopLevelModule  -- Load a file and set the top level module.
  , extractElemsAndTypes       -- From a previously loaded file, extract all definitions 
                               -- and their corresponding types if appropriate.
  ) where

import Control.Monad.Catch (throwM)
import Language.Haskell.Interpreter 
  (
    MonadInterpreter
  , getLoadedModules
  , getModuleExports
  , loadModules
  , setTopLevelModules
  , typeOf
  )

import AutoBench.AbstractSyntax (Id, ModuleElem(..), ModuleName, TypeString)
import AutoBench.Types (InputError(..))
import AutoBench.Utils (fpToModuleName)

-- | Load a file and set the top level module. The module name is calculated
-- from the file name by simply dropping the \'.hs\' extension. For example, if
-- the given file is named \'Input.hs\', then module name is assumed to be
-- \'Input\'.
-- 
-- If the file doesn't have a module name that matches the file name, an
-- 'InputError' is thrown.
loadFileSetTopLevelModule :: MonadInterpreter m => FilePath -> m ()
loadFileSetTopLevelModule fp = do
  let mn = fpToModuleName fp
  loadModules [fp]
  mods <- getLoadedModules
  if mn `elem` mods
  then setTopLevelModules [mn]        
  else throwM (FileErr "Invalid module name (module name must be same as file name).")

-- | From a previously loaded file, extract all definitions and their 
-- corresponding types if appropriate.
extractElemsAndTypes
  :: MonadInterpreter m 
  => ModuleName 
  -> m [(ModuleElem, Maybe TypeString)] 
extractElemsAndTypes mn = do
  defs <- getModuleExports mn
  tys  <- mapM (\case
    Nothing  -> return Nothing 
    Just idt -> Just <$> typeOf idt) (fmap funIdt defs)
  return (zip defs tys)
  where 
    -- Extract the identifiers from functions.
    funIdt :: ModuleElem -> Maybe Id
    funIdt (Fun idt) = Just idt 
    funIdt _         = Nothing



























{-


module TimeCheck.Hint (interp', interp) where 

import Control.Monad              (void)
import Control.Monad.Catch        (SomeException, Exception, MonadMask, catch, throwM, catchAll)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Either                (partitionEithers)
import Data.List                  ((\\), groupBy, isInfixOf, partition, sortBy)
import Data.List.Split            (splitOn)
import Data.Ord                   (comparing)
import Data.Tuple.Select          (sel1, sel2, sel3)
import Language.Haskell.Interpreter
  ( 
    Id
  , InterpreterError(..)
  , MonadInterpreter(..)
  , ModuleElem(..)
  , ModuleName
  , as
  , eval
  , getLoadedModules
  , getModuleExports
  , interpret
  , kindOf
  , loadModules
  , reset
  , setTopLevelModules
  , typeOf
  , InterpreterT
  )

import TimeCheck.InputValidation (countInputsGen, validNfs)
import TimeCheck.Utils           ((.*), (**>), (==>), allEq, fpToModuleName, notNull, strip)
import TimeCheck.Types           
  (
    AnalOpts(..)
  , DataOpts(..)
  , InputError(..)
  , InputType(..)
  , Type
  , TestOpts(..)
  , TestSuite(..)
  , defaultTestOpts
  , maxPredictors
  , minInputs
  , numPredictors

  -- * NEW 
  , parseTypeSig
  , UsrFun(..)
  , isNullaryFunType
  , isUnaryFunType
  , isBinaryFunType
  , usrFunIdt 
  , usrFunType 
  , usrFunHsType
  , HsTypeInp(..)
  , tyFunToTyInp
  , isValidGenType
  , UsrInps(..)
  , TestSuiteOpts(..)
  , TestConfig(..)
  , initUsrInps
  )

import TimeCheck.IO

-- ************************
--- NEW ---
-- ************************

import Control.Arrow ((&&&))
import Language.Haskell.Syntax
import System.IO
import Control.Monad ((>=>))
import Control.Monad.Trans.State
import Control.Lens.Setter
import Control.Lens.Getter
import Control.Lens
import Control.Monad (foldM, filterM)
import Control.Monad.Extra (partitionM)
import Data.Maybe (catMaybes)
import Data.List

import qualified Control.Exception.Base as BE

makeLenses ''UsrInps

-- Using Reader to pass around the test options ('TestOpts'), which are used to 
-- guide the checking of the user input file.
instance MonadInterpreter m => MonadInterpreter (ReaderT c m) where 
  fromSession        = lift . fromSession
  modifySessionRef f = lift . modifySessionRef f 
  runGhc f           = lift (runGhc f)








-- * Top-level

-- ******************************
-- ** REDO THESE COMMENTS **
-- ******************************
--
-- | Top-level function for \'interpreting\' the user input/test file. 
--
-- Given a user input/test filepath, load, parse, and analyse the file. 
-- Return: 
-- 
-- * The name of the 'TestOpts';
-- * The 'TestOpts';
-- * Zero or more valid 'TestSuite's;
-- * Zero or more invalid 'TestSuite's and their corresponding input errors.
--
-- Or fail and return an exception, for example:
-- 
-- * A compile error if the input file cannot be loaded by Hint;
-- * An input error if no test programs are included in the file;
-- * A test options error if the given test options are invalid.
--
-- Note: if the user fails to specify any 'TestOpts', the 'defaultTestOpts' will
-- be used by the system.
--
interp 
  :: MonadInterpreter m 
  => FilePath 
  -> m (Id, TestOpts, [TestSuite], [(TestSuite, InputError)])
interp fp = do 
   
  let mn = fpToModuleName fp

  -- (1) Load input file and helper modules used to perform validation checks.
  loadUserInputAndHelpers fp 
  
  -- (2) Discover test options.
  (tOptsName, tOpts) <- discoverTestOpts mn

  -- tsIs: Invalid test suites with input errors.
  -- tsVs: Valid test suites.
  (tsIs, tsVs) <- runReaderT 
    (do 
       -- (3) Discover test inputs.
       (tProgs, mtDats) <- discoverTestInputs mn
       -- (4) Generate test suites.
       ts <- generateTestSuites tProgs mtDats
       -- (5) Validate test suites.
       partitionEithers <$> validateTestSuites ts
    ) tOpts
  
  -- Return interpreted results.
  return (tOptsName, tOpts, tsVs, tsIs)




-- * Load the user input file

-- | Load the user input file and a number of 'helperModules' that will be 
-- used to perform /dynamic/ validation checks on test inputs. (Note: hint 
-- requires that all modules be loaded at the same time.)
--
loadUserInputAndHelpers :: MonadInterpreter m => FilePath -> m ()
loadUserInputAndHelpers fp = do
  -- (1) Try to load user input file.
  --     Also load helper modules so can validate test inputs.
  -- (2) Try and set top-level module name. If the user input file doesn't have
  --     a module name that matches the file name, throw an error as can't 
  --     guess it.
  loadModules (fp : helperModules)
  mods <- getLoadedModules
  -- E.g., if the test file is named 'Input.hs', then module name must be 
  -- 'Input'.
  if | mn `elem` mods -> setTopLevelModules (mn : helperModules)          
     | otherwise      -> throwM (FileErr "Input file has an invalid module \
         \name (module name must be same as file name).")
    where mn = fpToModuleName fp








-- * Discover test options

-- | Discover test options in the user input file. 
--
-- * If /multiple/ test options are given, then an error is thrown. 
-- * If none are given, then the default options will be used. See
--   'defaultTestOpts'.
--
discoverTestOpts 
  :: MonadInterpreter m 
  => ModuleName 
  -> m (Id, TestOpts)
discoverTestOpts mn = do

  -- (1) Get functions (cf. classes/data types) from the user input file.
  defs <- filterFuns <$> getModuleExports mn
  tys  <- mapM typeOf defs
  
  -- (2) Filter nullary types.
  let nonFs = filter (not . ("->" `isInfixOf`) . snd) (zip defs tys)
  
  -- (3) From the non-functions, find @x :: TestOpts@ or use 'defaultTestOpts'
  tOpts <- filtTestOpts nonFs
  -- (a) Check all 'TestOpts' fields are initialised.
  checkInit (fst tOpts)
  -- (b) Ensure 'TestOpts' settings are valid.
  checkValid (snd tOpts)
 
  -- (4) Return test options.
  return tOpts
  
  where 
    -- Note: Will error if multiple TestOpts are found. 
    --       If no TestOpts are found, will use default options.
    filtTestOpts xs = case (filter $ ((== testOptsType) . strip) . snd) xs of 
      []       -> return (defaultTestOptsName, defaultTestOpts)
      [(x, _)] -> (x, ) <$> interpret x (as :: TestOpts)
      _        -> throwM (TestOptsErr "Duplicate options found.")
      
    -- Ensure all record fields in TestOpts are initialised.
    checkInit tOptsName = catch' (interpret (interp_checkInitTestOpts tOptsName) 
      (as :: ())) (const . throwM $ TestOptsErr "One or more uninitialised record fields.")

    -- Ensure all TestOpts settings are valid.
    checkValid tOpts
      | not validNf = throwM (TestOptsErr "Conflicting nf settings: \
         \subNfRes can only be used with nfRes.")
      -- We only allow models to have 'maxPredictors' predictors.
      | not validModels = throwM (AnalOptsErr $ "Linear regression models can \
        \have a maximum of " ++ show maxPredictors ++ " predictors.")
      | otherwise = return ()
      where
        -- Valid subNfRes/nfRes settings.
        validNf = validNfs (subNfRes tOpts) (nfRes tOpts)
        -- Valid 'AnalOpts' 'models'
        validModels = maxPredictors >= maximum (fmap numPredictors $ models $ analOpts tOpts)





















-- * Discover test inputs

-- | Discover test inputs, including test programs and any manually specified
-- test data if applicable.
discoverTestInputs 
  :: MonadInterpreter m
  => ModuleName
  -> ReaderT TestOpts m ( [([Id], Type, InputType)]   -- Testable programs.
                        , Maybe [(Id, InputType)] )   -- Test data.
discoverTestInputs mn = do 
  
  -- Get TestOpts.
  tOpts <- ask 

  -- (1) Get function (cf. classes/data types) definitions/types from the 
  -- input file.
  defs <- filterFuns <$> getModuleExports mn
  tys  <- mapM typeOf defs

  -- (2) Split the function types @a -> b@ from non-function types.
  let (fs, nonFs) = partition (("->" `isInfixOf`) . snd) (zip defs tys)

  -- (4) Filter programs in input file, ensuring they are:
  -- (a) In the 'TestOpts' 'progs' list (unless its empty).
  -- (b) Compatible with each other (i.e., have the same type).
  -- (c)(i)   Of type a -> b.
  -- (c)(ii)  Of type a -> b -> c.
  -- (c)(iii) Monomorphic/polymorphic depending on test data settings.
  tProgs <- discoverTestableProgs fs

  -- (5) Find TestData if applicable and return test inputs.
  if dat tOpts == Manual 
  then (tProgs, ) . Just <$> discoverTestData (fmap sel3 tProgs) nonFs
  else return (tProgs, Nothing)

-- ** Discover testable programs

-- Discover definitions of testable programs:
-- (a) Find programs in 'TestOpts' 'progs' list, unless its empty, in which case
--     all programs in input file are considered.
-- (b) Group programs by their types, only accepting unary/binary programs of 
--     the /same/ type.
-- (c)(i)  If automatic test data generation, only accept monomorphic programs.
-- (c)(ii) If manual test data, also accept polymorphic programs.
--
-- Note: this function will error if:
-- (i)   No n-ary | n > 0 programs in input file.
-- (ii)  Missing programs from 'TestOpts' 'progs' list.
-- (iii) Programs in 'TestOpts' 'progs' list have different types.
-- (iv)  Non-unary/non-binary programs in 'TestOpts' 'progs' list.
-- (v)   Only non-unary/non-binary programs in input file. 
-- (vi)  Test data generation selected and polymorphic programs in 'TestOpts' 
--       'progs' list.
-- (vii) Test data generation selected and only polymorphic unary/binary
--       programs in input file.
--
-- All testable programs are returned with their types and expected input types. 
-- 
-- At this point the 'test' programs have /not/ been tested to see if they 
-- satisfy the required instances for data generation and benchmarking, i.e., 
-- Arbitrary and NFData. This comes later so that we can notify the user should 
-- errors arise.
discoverTestableProgs 
  :: MonadInterpreter m 
  => [(Id, Type)]                                  -- Function definitions and their types from input file.
  -> ReaderT TestOpts m [([Id], Type, InputType)]  -- (idts, ty, inpTys) 
discoverTestableProgs [] = 
  throwM (InputErr "No testable programs in input file.")
discoverTestableProgs xs = do 

  -- 'TestOpts' 'progs' list.
  ps <- progs <$> ask 

  -- Automatically generate test data?
  autoTData <- (/= Manual) . dat <$> ask

  -- (1) Filter out programs not in 'TestOpts' 'progs' list, unless it's empty.
  let ps'  = filterProgs (fmap strip ps) xs
  -- (2) Sort and group programs by their type.
      gs   = groupBy (\x y -> snd x == snd y) $ sortBy (comparing snd) ps'
  -- (3) Filter out any non-unary and non-binary programs.
      arys = filter ( ( \x -> let l = length (fSplit $ snd x)
                              in  l == 2 || l == 3
                      ) . head
                    ) gs
  -- (4) Filter out any polymorphic programs.
  mArys <- filterNonPolys arys
  
  -- Perform checks:
  -- (ii)
  if | notNull ps && length ps /= length ps' -> throwM (TestOptsErr "Missing \
         \programs specified in TestOpts' progs list.")
  -- (iii)
     | notNull ps && length gs > 1 -> throwM (TestOptsErr "Programs in TestOpts' \
         \progs list have different types.")
  -- (iv)
     | notNull ps && length ps > length (concat arys) -> throwM (TestOptsErr 
         "n-ary | n > 2 programs in TestOpts' progs list.")
  -- (v)
     | null arys -> throwM (InputErr "Only n-ary | n > 2 programs in \
         \input file.")
  -- (vi)
     | autoTData && notNull ps && length ps > length (concat mArys) -> throwM
         (TestOptsErr "Test data generation selected but polymorphic \ 
         \programs in TestOpts' progs list.")
  -- (vii)
     | autoTData && null mArys -> throwM (InputErr "Test data generation \
         \selected but no monomorphic unary/binary programs in input file.")
  -- OK: auto test data => monos only.
     | autoTData -> return $ fmap (\g -> 
        (fmap fst g, snd $ head g, inputTy . snd $ head g )) mArys
  -- OK: manual test data => monos and polys.
     | otherwise -> return $ fmap (\g -> 
        (fmap fst g, snd $ head g, inputTy . snd $ head g )) arys 

  where 
    -- Programs only in 'TestOpts' 'progs' list.
    filterProgs [] = id
    filterProgs ps = filter ((`elem` ps) . strip . fst)
   
    -- Monomorphic programs.
    filterNonPolys :: MonadInterpreter m => [[(a, Type)]] -> m [[(a, Type)]]
    filterNonPolys []         = return []
    filterNonPolys (ys : yss) = catch' ((kindOf . strip . snd $ head ys) >> 
      (ys :) <$> filterNonPolys yss) (const $ filterNonPolys yss)

    -- Test program input types.
    -- For unary:  a -> b       ==>  InpTyUn a
    -- For binary: a -> b -> c  ==>  InpTyBin a b
    -- This type will then be matched against manually specified test data.
    inputTy ty 
      | length tys == 2 = InpTyUn (strip $ head tys)
      -- Otherwise length /must/ be 3.
      | otherwise       = InpTyBin (strip $ head tys) (strip $ tys !! 1)
      where tys = fSplit ty

-- ** Discover test data

-- Discover definitions relating to test data. Note: if this function is called, 
-- then manual test data /is/ required, so will error if none is defined in 
-- the input file.
discoverTestData 
  :: MonadInterpreter m 
  => [InputType]                          -- Input types to testable programs.
  -> [(Id, Type)]                         -- All non-function definitions and their types from input file.
  -> ReaderT TestOpts m [(Id, InputType)] -- (name of tData, inpTy matching tData).
discoverTestData _ [] = throwM (InputErr "Manual test data selected but no test \
  \data in input file.")
discoverTestData inpTys xs = case matchDTys xs of 
  []  -> throwM (InputErr "Manual test data selected but no (compatible) test \
  \data in input file.")
  xs' -> return xs'

  where 
    -- Match the types of (non-function) definitions with the input types of 
    -- test programs.
    --
    -- E.g., a unary program Int -> Int has input type Int, hence 
    -- check if any (non-function) definitions in the input file have type 
    -- TestDataUn Int/[(Int, IO Int)]. If so, match the identifier
    -- of the definition with the input type.
    matchDTys [] = []
    matchDTys ((idt, ty) : yss) = 
      case filter (\x -> ty `elem` fst x) tySigs of
        []      -> matchDTys yss
        (x : _) -> (idt, snd x) : matchDTys yss

    -- Generate type signatures of compatible test data.
    tySigs :: [([Type], InputType)]
    tySigs  = fmap (\case
                     InpTyUn ty       -> ( testDataTySigUn ty      
                                         , InpTyUn ty) 
                     InpTyBin ty1 ty2 -> ( testDataTySigBin ty1 ty2
                                         , InpTyBin ty1 ty2 )
                   ) inpTys

-- * Generate test suites

-- | Given discovered test programs and test data, generate test suites. 
-- For manual test data, ensure the type of test data matches input 
-- types of test programs. E.g., for test program @p :: Int -> ?@, make 
-- sure test data is of type @TestDataUn Int@.
-- Note: at this point test programs have /not/ been checked to see if they 
-- satisfy the necessary instances, i.e., Arbitrary and NFData. Also haven't 
-- validated 'DataOpts':
-- (i)  For test data generation, minimum size range.
-- (ii) For manual test data, minimum quantity of input data (of distinct 
--      sizes).
-- The above checks are done later so the user can be notified about errors on 
-- an individual test suite basis.
generateTestSuites
  :: MonadInterpreter m
  => [([Id], Type, InputType)]      -- (idts, ty, inpTys)
  -> Maybe [(Id, InputType)]        -- (tData, inpTys)
  -> ReaderT TestOpts m [TestSuite] -- 'TestSuite's not yet validated.
-- @Nothing@ ==> generate test data.
generateTestSuites ps Nothing =
  dat <$> ask >>= \case 
    -- Shouldn't happen but just in case...
    Manual    -> return []
    Gen l s u -> do
      -- nf or whnf?
      nf <- nfRes <$> ask
      -- Separate unary and binary.
      let (uns, bins) = partition (\x -> length (fSplit $ sel2 x) == 2) ps
      -- Generate test suites.
      return $ fmap (\(idts, ty, _) -> if nf
                                       then GenNfUn   idts ty (l, s, u)
                                       else GenWhnfUn idts ty (l, s, u)) uns 
            ++ fmap (\(idts, ty, _) -> if nf 
                                       then GenNfBin   idts ty (l, s, u)
                                       else GenWhnfBin idts ty (l, s, u)) bins 
-- @Just dats@ ==> manual data. 
generateTestSuites ps (Just dats) = do
  -- Check if 'TestOpts' 'progs' list is not empty.
  progs_ <- notNull . progs <$> ask 
  -- nf or whnf?
  nf <- nfRes <$> ask
  -- Match input type of program against type of test data.
  let choices = (,) <$> ps <*> dats
      matches = filter (\(p, d) -> sel3 p == snd d) choices
  if | null matches && progs_ -> throwM (DataOptsErr "Manual test data selected \
        \but no compatible test data found for programs in TestOpts' progs list.")
     | null matches -> throwM (InputErr "Manual test data selected but no \
        \(compatible) test data in input file.")
     | otherwise -> do 
         -- Group programs with compatible test data in case multiple test data
         -- definitions.
         let ss  = sortBy (comparing sel2) $ fmap (\((idts, ty, _), d) ->
                     (idts, ty, fst d)) matches
             gs  = groupBy (\(idts1, ty1, _) (idts2, ty2, _) -> 
                     idts1 == idts2 && ty1 == ty2) ss
             ps' = fmap (\xs -> ( sel1 $ head xs
                                , sel2 $ head xs
                                , fmap sel3 xs )) gs
             -- Separate unary and binary.
             (uns, bins) = partition (\x -> length (fSplit $ sel2 x) == 2) ps'
         -- Generate test suites.
         return $ concatMap (\(idts, ty, ds) -> fmap (if nf 
                                                      then ManNfUn   idts ty
                                                      else ManWhnfUn idts ty) ds) uns
               ++ concatMap (\(idts, ty, ds) -> fmap (if nf 
                                                      then ManNfBin   idts ty
                                                      else ManWhnfBin idts ty) ds) bins

-- * Validate test suites

-- | Validate test suites. For each test suite:
-- (a) Check programs satisfy necessary instances (i.e., Arbitrary and NFData).
-- (b) Check 'DataOpts', ensuring that each test suite has the minimum number of
--     distinctively sized test inputs.
--
-- Returns valid test suites and invalid ones with relevant input errors. 
-- 
-- I think at this point it is better to allow validation access to the test
-- options in case future checks require them.
validateTestSuites 
  :: MonadInterpreter m 
  => [TestSuite] 
  -> ReaderT TestOpts m [Either (TestSuite, InputError) TestSuite]
validateTestSuites tcs = mapM validTestSuite tcs
  where
    -- Per test suite.
    validTestSuite
      :: MonadInterpreter m 
      => TestSuite
      -> ReaderT TestOpts m (Either (TestSuite, InputError) TestSuite)
    validTestSuite ts@(GenNfUn ps _ _) = catch' (do 
      -- (1) Check that the program satisfies the Arbitrary and NFData 
      -- instances required for testing.
      void $ eval $ eval_testGenNfUn $ head ps
      -- (2) Check the DataOpts size range is sufficient to generate the
      -- minimum number of distinctly sized inputs.
      --                 ... Doesn't satisfy required instance
      validTestData ts) (const $ return $ Left (ts, InstanceErr errGenNfUn)) 
      -- Remainder similar to above but using different settings.
    validTestSuite ts@(GenWhnfUn ps _ _) = catch' (do 
      void $ eval $ eval_testGenWhnfUn $ head ps
      validTestData ts) (const $ return $ Left (ts, InstanceErr errGenWhnfUn))
    validTestSuite ts@(GenNfBin ps _ _) = catch' (do 
      void $ eval $ eval_testGenNfBin $ head ps
      validTestData ts) (const $ return $ Left (ts, InstanceErr errGenNfBin)) 
    validTestSuite ts@(GenWhnfBin ps _ _) = catch' (do 
      void $ eval $ eval_testGenWhnfBin $ head ps
      validTestData ts) (const $ return $ Left (ts, InstanceErr errGenWhnfBin))
    validTestSuite ts@(ManNfUn ps _ _) = catch' (do 
      void $ eval $ eval_testManNfUn $ head ps
      validTestData ts) (const $ return $ Left (ts, InstanceErr errManNfUn)) 
    validTestSuite ts@(ManWhnfUn ps _ _) = catch' (do
      void $ eval $ eval_testManWhnfUn $ head ps
      validTestData ts) (const $ return $ Left (ts, InstanceErr errManWhnfUn))
    validTestSuite ts@(ManNfBin ps _ _) = catch' (do 
      void $ eval $ eval_testManNfBin $ head ps
      validTestData ts) (const $ return $ Left (ts, InstanceErr errManNfBin)) 
    validTestSuite ts@(ManWhnfBin ps _ _) = catch' (do 
      void $ eval $ eval_testManWhnfBin $ head ps
      validTestData ts) (const $ return $ Left (ts, InstanceErr errManWhnfBin))
    
     -- Validate DataOpts:
    -- (a) For auto generation, validate Gen size range.
    -- (b) For manual input data, check number of inputs of /distinct/ sizes.
    validTestData 
      :: MonadInterpreter m 
      => TestSuite
      -> ReaderT TestOpts m (Either (TestSuite, InputError) TestSuite)
    validTestData ts@(GenNfUn _ _ (l, s, u)) =
      -- Measure size range of Gen l s u, i.e., (u - l) / s.
      -- Make sure size range >= minInputs.
      if countInputsGen (Gen l s u) >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
    validTestData ts@(GenWhnfUn _ _ (l, s, u)) =
      -- Measure size range of Gen l s u, i.e., (u - l) / s.
      -- Make sure size range >= minInputs.
      if countInputsGen (Gen l s u) >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
    validTestData ts@(GenNfBin _ _ (l, s, u)) =
      if countInputsGen (Gen l s u) >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
    validTestData ts@(GenWhnfBin _ _ (l, s, u)) =
      if countInputsGen (Gen l s u) >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
    -- Similar to above, but for manual data. We do a nub on size to make sure
    -- distinct input sizes >= minInputs.
    validTestData ts@(ManNfUn _ _ d) = catch' (do 
      size <- interpret (interp_countManInputsUn d) (as :: Int)
      if size >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
      ) (const $ return $ Left (ts, DataOptsErr dataOptsErr))
    validTestData ts@(ManWhnfUn _ _ d) = catch' (do 
      size <- interpret (interp_countManInputsUn d) (as :: Int)
      if size >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
      ) (const $ return $ Left (ts, DataOptsErr dataOptsErr))
    validTestData ts@(ManNfBin _ _ d) = catch' (do 
      size <- interpret (interp_countManInputsBin d) (as :: Int)
      if size >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
      ) (const $ return $ Left (ts, DataOptsErr dataOptsErr))
    validTestData ts@(ManWhnfBin _ _ d) = catch' (do 
      size <- interpret (interp_countManInputsBin d) (as :: Int)
      if size >= minInputs
         then return $ Right ts
         else return $ Left (ts, DataOptsErr numInpErr)
      ) (const $ return $ Left (ts, DataOptsErr dataOptsErr))

    -- Error messages:
    -- Nf:
    errGenNfUn    = "(NFData a, NFData b, Arbitrary a) => a -> b"
    errGenNfBin   = "(NFData a, NFData b, NFData c, Arbitrary a, \
                      \Arbitrary b) => a -> b -> c"
    errManNfUn    = "(NFData a, NFData b) => a -> b"
    errManNfBin   = "(NFData a, NFData b, NFData c) => a -> b -> c"
    -- Whnf:
    errGenWhnfUn  = "(NFData a, Arbitrary a) => a -> b"
    errGenWhnfBin = "(NFData a, NFData b, Arbitrary a, \
                      \Arbitrary b) => a -> b -> c"
    errManWhnfUn  = "NFData a => a -> b"
    errManWhnfBin = "(NFData a, NFData b) => a -> b -> c"
    -- DataOpts:
    numInpErr     = "A minimum of " ++ show minInputs ++ " distinctly sized \
                     \test inputs are required."
    dataOptsErr   = "Unexpected error while validating test data." 

-- * Helpers

-- | Split a function type into its constituent types. This currently nulls 
-- out any function types containing class constraints i.e., @=>@ in type sig.
fSplit :: Type -> [Type]
fSplit s | "=>" `isInfixOf` s = []
         | otherwise          = "->" `splitOn` s

-- | Compiler needs this type information.
catch' :: MonadInterpreter m => m a -> (InterpreterError -> m a) -> m a
catch'  = catch

catch'' :: MonadInterpreter m => m a -> (BE.SomeException -> m a) -> m a
catch''  = catch

-- | Filter only function names, ignoring classes and data.
filterFuns :: [ModuleElem] -> [Id]
filterFuns []             = []
filterFuns (Fun idt : xs) = idt : filterFuns xs
filterFuns (_       : xs) = filterFuns xs

-- | Given a unary input type (i.e., one type), generate the types signatures 
-- of compatible test data.
--
-- -- __Editing these strings may break the functionality of the system__.
--
-- > testDataTySigUn \"Int\" = [ \"TestDataUn Int\", \"[(Int, IO Int)]\" ]
testDataTySigUn :: Type -> [Type]
testDataTySigUn ty = [ "TestDataUn " ++ ty
                     , "[(Int, IO " ++ ty ++ ")]" ]

-- | Given a binary input type (i.e., two types), generate the type signature 
-- of compatible test data.
--
-- -- __Editing these strings may break the functionality of the system__.
--
-- > testDataTySigBin \"Int\" \"Char\" = [ \"TestDataBin Int Char\", \"[(Int, Int, IO Int, IO Char)]\" ]
testDataTySigBin :: Type -> Type -> [Type]
testDataTySigBin ty1 ty2 = 
  [ "TestDataBin " ++ ty1 ++ " " ++  ty2
  , "[(Int, Int, IO " ++ ty1 ++ ", IO " ++ ty2 ++ ")]" ]

-- ** Generate statements to interpret/evaluate using Hint.
--
-- Involves constructing statements that will execute functions from 
-- 'TimeCheck.InputValidation' on the given test program.
-- 
-- __Editing these strings may break the functionality of the system__.

-- | Ensure all 'TestOpts' record fields are initialised.
interp_checkInitTestOpts :: String -> String
interp_checkInitTestOpts s = "seq (initTestOpts " ++ s ++ ") ()"

-- | Count number of distinctly sized test inputs for unary test programs.
interp_countManInputsUn :: String -> String 
interp_countManInputsUn  = ("countInputsManUn " ++)

-- | Count number of distinctly sized test inputs for binary test programs.
interp_countManInputsBin :: String -> String 
interp_countManInputsBin  = ("countInputsManBin " ++)

-- | Ensure unary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and evaluating results to normal form.
eval_testGenNfUn :: String -> String 
eval_testGenNfUn  = ("testGenNf " ++)

-- | Ensure unary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and evaluating results to weak head 
-- normal form.
eval_testGenWhnfUn :: String -> String 
eval_testGenWhnfUn  = ("testGenWhnf " ++)

-- | Ensure binary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and benchmarking, including evaluating 
-- results to normal form.
eval_testGenNfBin :: String -> String 
eval_testGenNfBin  = ("testGenNf $ uncurry " ++)

-- | Ensure binary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and benchmarking, including evaluating 
-- results to weak head normal form.
eval_testGenWhnfBin :: String -> String 
eval_testGenWhnfBin  = ("testGenWhnf $ uncurry " ++)

-- | Ensure unary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and benchmarking, including evaluating 
-- results to normal form.
eval_testManNfUn :: String -> String 
eval_testManNfUn  = ("testManNf " ++)

-- | Ensure unary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and benchmarking, including evaluating 
-- results to weak head normal form.
eval_testManWhnfUn :: String -> String 
eval_testManWhnfUn  = ("testManWhnf " ++)

-- | Ensure binary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and benchmarking, including evaluating 
-- results to normal form.
eval_testManNfBin :: String -> String 
eval_testManNfBin  = ("testManNf $ uncurry " ++)

-- | Ensure binary test programs satisfy necessary 'Arbitrary' and 'NFData' 
-- instances for test data generation and benchmarking, including evaluating 
-- results to weak head normal form.
eval_testManWhnfBin :: String -> String 
eval_testManWhnfBin  = ("testManWhnf $ uncurry " ++)

-- ** Constants
-- 
-- __Editing these strings may break the functionality of the system__.

testOptsType :: String
testOptsType  = "TestOpts"

defaultTestOptsName :: String
defaultTestOptsName  = "defaultTestOpts" 

-- | Modules used to validate test inputs in the user input/test file.
helperModules :: [ModuleName]
helperModules  = ["TimeCheck.InputValidation"]






-- ************************
--- NEW ---
-- ************************

instance MonadInterpreter m => MonadInterpreter (StateT s m) where 
  fromSession        = lift . fromSession
  modifySessionRef f = lift . modifySessionRef f 
  runGhc f           = lift (runGhc f)

-- | The 'HsType' (i.e., parsed representation) of 'TestOpts'.
testOptsHsType :: HsType 
testOptsHsType  = HsTyCon (UnQual (HsIdent "TestOpts"))

-- | The 'HsType' (i.e., parsed representation) of 'TestSuiteOpts'.
testSuiteHsType :: HsType 
testSuiteHsType  = HsTyCon (UnQual (HsIdent "TestSuiteOpts"))

-- | The 'HsType' (i.e., parsed representation) of 'TestConfig'.
testConfigHsType :: HsType 
testConfigHsType  = HsTyCon (UnQual (HsIdent "TestConfig"))

-- | The 'HsType' (i.e., parsed representation) of 'TestDataUn'.
testDataUnHsType :: HsType 
testDataUnHsType  = HsTyCon (UnQual (HsIdent "TestDataUn"))

-- | The 'HsType' (i.e., parsed representation) of 'TestDataBin'.
testDataBinHsType :: HsType 
testDataBinHsType  = HsTyCon (UnQual (HsIdent "TestDataBin"))




eval_testArbitraryInputUn :: String -> String 
eval_testArbitraryInputUn  = ("testArbitraryInputUn " ++)

eval_testNFDataInputUn :: String -> String 
eval_testNFDataInputUn  = ("testNFDataInputUn " ++)

eval_testNFDataInputBin :: String -> String 
eval_testNFDataInputBin  = ("testNFDataInputBin " ++)

eval_testNFDataResultUn :: String -> String 
eval_testNFDataResultUn  = ("testNFDataResultUn " ++)

eval_testNFDataResultBin :: String -> String 
eval_testNFDataResultBin  = ("testNFDataResultBin " ++)

interp_testInitialisedTestConfig :: String -> String
interp_testInitialisedTestConfig s = "testInitialisedTestConfig " ++ s

interp_testInitialisedTestSuite :: String -> String
interp_testInitialisedTestSuite s = "testInitialisedTestSuite " ++ s

interp' :: MonadInterpreter m => FilePath -> StateT UsrInps m ()
interp' fp = do 
   
  let mn = fpToModuleName fp

  -- Set up: ------------------------------------------------------------------

  -- Attempt to dynamically load the user input file and set it as the 
  -- top-level module. Will throw an error if the file can't be loaded or if 
  -- the top-level module cannot be set.
  loadModules [fp]
  mods <- getLoadedModules
  -- If the test file is named 'Input.hs', then module name must be 'Input'.
  if | mn `elem` mods -> setTopLevelModules [mn]       
     | otherwise      -> throwM (FileErr "Input file has an invalid module \
         \name (module name must be same as file name).")
  
  -- Discovery: ---------------------------------------------------------------

  -- (1a) Get all the module elements and store them in the 'UsrInps'.
  modElems <- getModuleExports mn
  allElems .= modElems
  -- (1b) Filter the function elements, store the remainder as ignored elements
  --      in the 'UsrInps'.
  let (funElems, _ignoredElems) = partition isFunElem modElems
  ignoredElems .= _ignoredElems
  -- (2a) Get the types of the functions and store them in the 'UsrInps'.
  let funIdts = fmap funId funElems
  _allFuns <- zip funIdts <$> mapM typeOf funIdts
  allFuns .= _allFuns
  -- (2b) Discover all the functions: check their types conform to Haskell 98 
  --      standard, and ensure they are nullary/unary/binary.
  discoverTestFuns
  -- (3) Discover test test data.
  discoverTestData'
  -- (4) Discover test configurations.
  discoverTestConfigs
  -- (5) Discover test suites.
  discoverTestSuites

  -- Validation: --------------------------------------------------------------

  -- Reload the user's module along with helper modules for performing 
  -- validation checks.
  reset
  loadModules        (fp : helperModules)
  setTopLevelModules (mn : helperModules)     

  validateTestFuns 
  validateTestConfigs
  validateTestSuites' 
  validateTestData 

  s <- get 
  error $ show s
 
  undefined

  where 
    -- Check if a module element is a function.
    isFunElem :: ModuleElem -> Bool
    isFunElem (Fun _) = True 
    isFunElem _       = False

    -- Get a function's identifier.
    funId :: ModuleElem -> Id 
    funId (Fun idt) = idt 
    funId _ = error "shouldn't happen: funId"

-- * Discover test functions

discoverTestFuns :: Monad m => StateT UsrInps m ()
discoverTestFuns = do 
  -- (1) Get all the functions from the 'UsrInps'.
  _allFuns <- use allFuns
  -- (2) Parse their types according to Haskell 98 standard.
  let parsedTys = (parseTypeSig .* (++) . (++ " :: ")) **> _allFuns
  -- (3) Split the types that could be parsed from those that couldn't be. 
  --     I.e., those that conform to Haskell 98 and those that do not.
      (nonTypeables, typeables) = foldr splitTys ([], []) parsedTys
  -- (4) Filter typeables by nullary/unary/binary function types.
      (invalidArities, testFuns) = foldr splitArys ([], []) typeables
  -- (5) Update the 'UsrInps' accordingly:
  --  Invalids.
  let invalidsTys = [InputErr "Invalid type signature."] ==> nonTypeables
      invalidArs  = [InputErr "Invalid arity."]          ==> invalidArities
      _invalidFuns = invalidsTys ++ invalidArs
  invalidFuns .= _invalidFuns
  -- Valids. 
  nullaryFuns .= filter (isNullaryFunType . usrFunHsType) testFuns
  unaryFuns   .= filter (isUnaryFunType   . usrFunHsType) testFuns 
  binaryFuns  .= filter (isBinaryFunType  . usrFunHsType) testFuns

  where 
    -- Split typable/non-typeable functions.
    splitTys (idt, ty , Just hsTy) (e, v) = (e, (idt, ty, hsTy) : v)
    splitTys (idt, ty, _)          (e, v) = ((idt, ty) : e, v)

    -- Split functions with valid/invalid arity.
    splitArys (idt, ty, hsTy) (e, v) 
      | isNullaryFunType hsTy 
          || isUnaryFunType hsTy 
          || isBinaryFunType hsTy = (e, UsrFun (strip idt) ty hsTy : v)
      | otherwise = ((idt, ty) : e, v)

-- * Discover test data

discoverTestData' :: Monad m => StateT UsrInps m ()
discoverTestData' = do 
  -- (1) Get all the nullary functions from the 'UsrInps'.
  nullarys <- use nullaryFuns
  -- (2) Find all @x :: TestDataUn a@ or @x :: TestDataBin a b@.
  let _unaryData  = filter (matchTestDataUn  . usrFunHsType) nullarys 
      _binaryData = filter (matchTestDataBin . usrFunHsType) nullarys 
  -- (3) Update the 'UsrInps' accordingly.
  unaryData  .= _unaryData
  binaryData .= _binaryData
  where 
    matchTestDataUn :: HsType -> Bool
    matchTestDataUn (HsTyApp hsTy _) = hsTy == testDataUnHsType
    matchTestDataUn _ = False

    matchTestDataBin :: HsType -> Bool 
    matchTestDataBin (HsTyApp (HsTyApp hsTy _) _) = hsTy == testDataBinHsType
    matchTestDataBin _ = False

-- * Discover test configurations 

discoverTestConfigs :: MonadInterpreter m => StateT UsrInps m ()
discoverTestConfigs  = do 
  -- (1) Get all the nullary functions from the 'UsrInps'.
  nullarys <- use nullaryFuns
  -- (2) Find all @x :: TestConfig@.
  let testConfigIdts = fmap usrFunIdt $ filter ((== testConfigHsType) . usrFunHsType) nullarys
  -- (3) Dynamically load the 'TestConfig's.
  _testConfigs <- zip testConfigIdts <$> 
    mapM (flip interpret (as :: TestConfig)) testConfigIdts
  -- (4) Update the 'UsrInps' accordingly.
  testConfigs .= _testConfigs 

-- * Discover test suites

discoverTestSuites :: MonadInterpreter m => StateT UsrInps m ()
discoverTestSuites  = do
  -- (1) Get all the nullary functions from the 'UsrInps'.
  nullarys <- use nullaryFuns
  -- (2) Find all @x :: TestSuiteOpts@.
  let testSuiteIdts = fmap usrFunIdt $ filter ((== testSuiteHsType) . usrFunHsType) nullarys 
  -- (3) Dynamically load the 'TestSuiteOpts'
  _testSuites <- zip testSuiteIdts <$> 
    mapM (flip interpret (as :: TestSuiteOpts)) testSuiteIdts
  -- (4) Update the 'UsrInps' accordingly.
  testSuites .= _testSuites

-- * Validate the test functions

validateTestFuns :: forall m. MonadInterpreter m => StateT UsrInps m ()
validateTestFuns = sortUnarys >> sortBinarys
  where 
    sortUnarys :: MonadInterpreter m => StateT UsrInps m ()
    sortUnarys  = do 
      -- (1) Get all the unary functions from the 'UsrInps'.
      unarys <- use unaryFuns
      -- (2) Partition functions by those whose input type can be nf'd.
      --     This is mandatory for all programs to be tested using the system.
      (valids, invalids) <- partitionM testNFDataInputUn unarys
      -- (3) Update the 'UsrInps' accordingly.
      unaryFuns .= valids 
      invalidFuns %= (++ [nfDataInpError] ==> fmap (usrFunIdt &&& usrFunType) invalids)
      -- (4) Update 'arbFuns' in 'UsrInps'.
      _arbFuns <- filterM testArbitraryInputUn valids
      arbFuns .= _arbFuns
      -- (5) Update 'nfFuns' in 'UsrInps'.
      _nfFuns <- filterM testNFDataResultUn valids
      nfFuns .= _nfFuns
      
      where 
        -- Check whether a function's input type is a member of the NFData 
        -- type class.
        testNFDataInputUn = testUsrDef eval_testNFDataInputUn

        -- Check whether a function's input type is a member of the Arbitrary
        -- type class.
        testArbitraryInputUn = testUsrDef eval_testArbitraryInputUn

        -- Check whether a function's result type is a member of the NFData 
        -- type class.
        testNFDataResultUn = testUsrDef eval_testNFDataResultUn

        -- Errors:
        nfDataInpError = InstanceErr "NFData a => a -> b"

    sortBinarys :: MonadInterpreter m => StateT UsrInps m ()
    sortBinarys  = do 
      -- (1) Get all the binary functions from the 'UsrInps'.
      binarys <- use binaryFuns
      -- (2) Partition functions by those whose input type can be nf'd.
      --     This is mandatory for all programs to be tested using the system.
      (valids, invalids) <- partitionM testNFDataInputBin binarys
      -- (3) Update the 'UsrInps' accordingly.
      binaryFuns .= valids 
      invalidFuns %= (++ [nfDataInpError] ==> fmap (usrFunIdt &&& usrFunType) invalids)
      -- (4) Update 'arbFuns' in 'UsrInps'.
      _arbFuns <- filterM testArbitraryInputBin valids
      arbFuns %= (++ _arbFuns)
      -- (5) Update 'nfFuns' in 'UsrInps'.
      _nfFuns <- filterM testNFDataResultBin valids
      nfFuns %= (++ _nfFuns)
      
      where 
        -- Check whether a function's input types are a member of the NFData 
        -- type class.
        testNFDataInputBin = testUsrDef eval_testNFDataInputBin
        -- Check whether a function's input types are a member of the Arbitrary
        -- type class.
        testArbitraryInputBin = testUsrDef eval_testNFDataInputBin
        -- Check whether a function's result type is a member of the NFData 
        -- type class.
        testNFDataResultBin = testUsrDef eval_testNFDataResultBin

        -- Errors:
        nfDataInpError = InstanceErr "(NFData a, NFData b) => a -> b -> c"
    
    -- Test a UsrDef by checking if its type satisfies a given instance 
    -- requirement. Note that the requirements relate to functions called in 
    -- the 'InputValidation' module.
    testUsrDef 
      :: MonadInterpreter m 
      => (String -> String)
      -> UsrFun 
      -> StateT UsrInps m Bool
    testUsrDef test usrFun = 
      catch' ((void . eval . test $ usrFunIdt usrFun) >> return True)
             (const $ return False)

-- * Validate test data

validateTestData :: forall m. MonadInterpreter m => StateT UsrInps m ()
validateTestData  = sortUnarys >> sortBinarys
  where 

    -- Unary data: ------------------------------------------------------------

    sortUnarys :: MonadInterpreter m => StateT UsrInps m ()
    sortUnarys  = do 
      -- (1) Get all the unary data from the 'UsrInps'.
      _unaryData <- use unaryData
      -- (2) Perform a number of validation checks on the data.
      --     Partition data according to whether /all/ checks succeed.
      (invalids, valids) <- foldM check ([], []) _unaryData
      -- (3) Update 'UsrInps' accordingly.
      unaryData   .= valids
      invalidData .= invalids 

      where 
        check 
          :: MonadInterpreter m 
          => ([(Id, Type, [InputError])], [UsrFun])
          -> UsrFun
          -> StateT UsrInps m ([(Id, Type, [InputError])], [UsrFun])
        check (e, v) uf@(UsrFun idt ty _) =  
          catMaybes <$> sequence [ minInps idt ] >>= \case
            []   -> return (e, uf : v)
            errs -> return ((idt, ty, errs) : e, v)
        
        -- Validations: -------------------------------------------------------

        minInps 
          :: MonadInterpreter m 
          => Id 
          -> StateT UsrInps m (Maybe InputError)
        minInps idt = catch' 
          (do size <- interpret (interp_countManInputsUn idt) (as :: Int)
              if size >= minInputs
              then return Nothing
              else return $ Just numInpsError
          ) (const $ return $ Just unexpError) 

        -- Error messages: ----------------------------------------------------

        numInpsError = InputErr $ "A minimum of " ++ show minInputs ++ 
          " distinctly sized test inputs are required."
        unexpError = InputErr "An unexpected error occurred while validating \
          \test data." 

    -- Binary data: -----------------------------------------------------------

    sortBinarys :: MonadInterpreter m => StateT UsrInps m ()
    sortBinarys  = do 
      -- (1) Get all the binary data from the 'UsrInps'.
      _binaryData <- use binaryData
      -- (2) Perform a number of validation checks on the data.
      --     Partition data according to whether /all/ checks succeed.
      (invalids, valids) <- foldM check ([], []) _binaryData
      -- (3) Update 'UsrInps' accordingly.
      binaryData  .= valids
      invalidData %= (++ invalids) 

      where 
        check 
          :: MonadInterpreter m 
          => ([(Id, Type, [InputError])], [UsrFun])
          -> UsrFun
          -> StateT UsrInps m ([(Id, Type, [InputError])], [UsrFun])
        check (e, v) uf@(UsrFun idt ty _) =  
          catMaybes <$> sequence [ minInps idt ] >>= \case
            []   -> return (e, uf : v)
            errs -> return ((idt, ty, errs) : e, v)
        
        -- Validations: -------------------------------------------------------

        minInps 
          :: MonadInterpreter m 
          => Id 
          -> StateT UsrInps m (Maybe InputError)
        minInps idt = catch' 
          (do size <- interpret (interp_countManInputsBin idt) (as :: Int)
              if size >= minInputs
              then return Nothing
              else return $ Just numInpsError
          ) (const $ return $ Just unexpError) 

        -- Error messages: ----------------------------------------------------

        numInpsError = InputErr $ "A minimum of " ++ show minInputs ++ 
          " distinctly sized test inputs are required."
        unexpError = InputErr "An unexpected error occurred while validating \
          \test data." 


-- * Validate test configurations

validateTestConfigs :: MonadInterpreter m => StateT UsrInps m ()
validateTestConfigs = do 
  -- (1) Get all the test configurations from the 'UsrInps'.
  _testConfigs <- use testConfigs
  -- (2) Partition test configurations by those whose record fields are all 
  --     initialised.
  (valids, invalids) <- lift $ partitionM testInitialisedTestConfig _testConfigs
  -- (3) Update the 'UsrInps' accordingly.
  invalidTestConfigs .= (fmap (\(idt, _) -> (idt, [initError])) invalids)
  testConfigs .= valids 

  where
    -- Check whether all record fields of a 'TestConfig' are initialised.
    testInitialisedTestConfig :: MonadInterpreter m => (Id, TestConfig) -> m Bool
    testInitialisedTestConfig (idt, _) = 
      -- Note: need this to be strict for so error can be caught properly.
      catchAll (do !_ <- interpret (interp_testInitialisedTestConfig idt) (as :: ())
                   return True
               ) (const $ return False)
    
    -- Errors:
    initError = InstanceErr "One or more record fields are uninitialised."




-- * Validate test suites

validateTestSuites' :: MonadInterpreter m => StateT UsrInps m ()
validateTestSuites' = do 
  -- (0) Get the 'UsrInps'
  _usrInps <- get
  -- (1) Get all the test suites from the 'UsrInps'.
  _testSuites <- use testSuites
  -- (2) Partition test suites by those whose record fields are all 
  --     initialised.
  (valids, invalids) <- lift $ partitionM testInitialisedTestSuite _testSuites
  -- (3) Update the 'UsrInps' accordingly for the invalid test suites.
  invalidTestSuites .= (fmap (\(idt, _) -> (idt, [initError])) invalids)
  -- (4) Perform additional checks on the test suites whose record fields 
  --     are all initialised to ensure their settings are valid.
  let (invalids', valids') = foldr (check _usrInps) ([], []) valids
  -- (5) Update the 'UsrInps' accordingly.
  testSuites .= valids' 
  invalidTestSuites %= (++ invalids')
  
  where 

    -- Initialisation check: --------------------------------------------------

    -- Check whether all record fields of a 'TestConfig' are initialised.
    testInitialisedTestSuite (idt, _) = 
      catchAll (do !_ <- interpret (interp_testInitialisedTestSuite idt) (as :: ())
                   return True
               ) (const $ return False)
    
    -- Additional checks: -----------------------------------------------------

    check _usrInps arg@(idt, ts) (e, v) =
      case catMaybes $ [ missProgs, tyProgs, nfProgs, arbProgs, manProgs
                       , minInps, maxPreds ] of 
        []   -> (e, arg : v)
        errs -> ((idt, errs) : e, v)
      where 

        -- Projections: -------------------------------------------------------

        progsIdts     = _progs ts
        progsTys      = catMaybes $ fmap lookupUsrFunTy progsIdts
        progsHsTys    = catMaybes $ fmap lookupUsrFunHsTy progsIdts
        progsInpTys   = fmap tyFunToTyInp progsHsTys
        funIdts       = unaryFunIdts ++ binaryFunIdts
        nfFunIdts     = fmap usrFunIdt (_nfFuns  _usrInps)
        arbFunIdts    = fmap usrFunIdt (_arbFuns _usrInps)
        funIdtsTys    = fmap (usrFunIdt &&& usrFunType)   (_unaryFuns _usrInps ++ _binaryFuns _usrInps)
        funIdtsHsTys  = fmap (usrFunIdt &&& usrFunHsType) (_unaryFuns _usrInps ++ _binaryFuns _usrInps)
        unaryFunIdts  = fmap usrFunIdt $ _unaryFuns _usrInps
        binaryFunIdts = fmap usrFunIdt $ _binaryFuns _usrInps

        unaryDataHsTys     = fmap usrFunHsType $ _unaryData  _usrInps
        unaryDataHsTyInps  = catMaybes $ fmap genDataTyInp unaryDataHsTys
        binaryDataHsTys    = fmap usrFunHsType $ _binaryData _usrInps
        binaryDataHsTyInps = catMaybes $ fmap genDataTyInp binaryDataHsTys
        dataHsTyInps       = unaryDataHsTyInps ++ binaryDataHsTyInps

        -- Helpers: -----------------------------------------------------------

        lookupUsrFunTy :: Id -> Maybe Type
        lookupUsrFunTy = flip lookup funIdtsTys

        lookupUsrFunHsTy :: Id -> Maybe HsType 
        lookupUsrFunHsTy = flip lookup funIdtsHsTys

        genDataTyInp :: HsType -> Maybe HsTypeInp
        genDataTyInp (HsTyApp tDatUn hsTy) 
          | tDatUn == testDataUnHsType = Just (HsTyInpUn hsTy)
        genDataTyInp (HsTyApp (HsTyApp tDatBin hsTy1) hsTy2) 
          | tDatBin == testDataBinHsType = Just (HsTyInpBin hsTy1 hsTy2) 
        genDataTyInp _ = Nothing

        --- Validations: ------------------------------------------------------

        -- Check for missing programs specified in the 'progs' list.
        missProgs 
          | null progsIdts = Nothing 
          | progsIdts == (progsIdts `intersect` funIdts) = Nothing 
          | otherwise = Just missProgsError
        -- Check that all programs specified in the 'progs' list have the same 
        -- type. 
        tyProgs
          | null progsIdts = Nothing 
          | allEq progsTys = Nothing
          | otherwise = Just tyProgsError
        -- If nfRes then ensure the results of all programs specified in the 
        -- 'progs' list can be evaluated to normal form.
        nfProgs 
          | null progsIdts = Nothing
          | not (_nfRes ts) = Nothing
          | progsIdts == (progsIdts `intersect` nfFunIdts) = Nothing 
          | otherwise = Just nfProgsError
        -- If test data is to be generated, ensure inputs for programs in 
        -- 'progs' list can be randomly generated.
        arbProgs
          | null progsIdts = Nothing 
          | _dataOpts ts == Manual = Nothing
          | progsIdts == (progsIdts `intersect` arbFunIdts) = Nothing 
          | otherwise = Just arbProgsError
        -- If test data is manually specified, ensure some valid test data 
        -- exists in the file.
        manProgs 
          | null progsIdts = Nothing 
          | _dataOpts ts /= Manual = Nothing
          | progsInpTys == (progsInpTys `intersect` dataHsTyInps) = Nothing 
          | otherwise = Just manProgsError
        -- If test data is to be generated, ensure the minimum number of 
        -- inputs ('minInputs') will be generated.
        minInps
          | _dataOpts ts == Manual = Nothing
          | countInputsGen (_dataOpts ts) >= minInputs = Nothing
          | otherwise = Just minInpsError
        -- Make sure models in analysis options have leq the maximum number of 
        -- permitted predictors.
        maxPreds
         | maxPredictors >= maximum (fmap numPredictors $ models $ _analOpts ts) = Nothing 
         | otherwise = Just maxPredsError  

    -- Error messages: --------------------------------------------------------

    initError      = InstanceErr "One or more record fields are uninitialised."
    missProgsError = InputErr "Missing programs specified in the 'progs' list."
    tyProgsError   = InputErr "Programs specified in the 'progs' list have \
      \different types."
    nfProgsError   = InputErr "One or more programs specified in the 'progs' \
      \list has a result type that is not a member of the NFData type class."
    arbProgsError  = InputErr "One or more programs specified in the 'progs' \
      \list has a result type that is not a member of the Arbitrary type class."
    manProgsError  = InputErr "One or more programs specified in the 'progs' \
      \list have no corresponding test data."
    minInpsError   = InputErr $ "A minimum of " ++ show minInputs ++ 
      " distinctly sized test inputs are required." 
    maxPredsError = AnalOptsErr $ "Linear regression models can have a maximum \
      \of " ++ show maxPredictors ++ " predictors."















  {-

  -- Verbose output.
  outputVerbose "Discovering potential test data..."

  case usrFuns of 
    [] -> throwM (InputErr "Manual test data selected but no valid test data specified.")
    _  -> do 

     -- (1) Sort and group functions by their type.
     let usrFunss = groupBy (\uf1 uf2 -> usrFunType uf1 == usrFunType uf2) $ 
                    sortBy (comparing usrFunType) usrFuns
     -- (2) Match types with TestDataUn/Bin and generate appropriate input types if applicable.
         userFunssInps = zip usrFunss $ fmap (matchTestDataHsType . usrFunHsType . head) usrFunss
     -- (3) Split by matches/non-matches.
         (nonMatchingTDataTys, matchingTDataTys) = foldr splitTestDataMatch ([], []) userFunssInps
     -- (4) Partition by matching input types with those in 'inpTys'.
         (nonMatchingInpTys, matchingInpTys) = partition ((`elem` inpTys) . snd) matchingTDataTys

     -- Verbose output. 
     outputVerbose 
       $ unlines 
       $ "Invalid functions (incorrect type):" 
       : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (concat nonMatchingTDataTys)
     outputVerbose 
       $ unlines 
       $ "Invalid functions (no matching test program):" 
       : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (concat $ fmap fst nonMatchingInpTys)
     outputVerbose 
       $ unlines 
       $ "Accepted functions:" 
       : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (concat $ fmap fst matchingInpTys)
     
     -- (4) Return test data.
     case matchingInpTys of 
       [] -> throwM (InputErr "Manual test data selected but no valid/compatible test data specified.")
       _  -> return matchingInpTys

    where 
      -- Match the typeof functions with TestDataUn/Bin and generation 
      -- appropriate input type.
      matchTestDataHsType :: HsType -> Maybe HsTypeInp
      matchTestDataHsType (HsTyApp tDatUn hsTy) 
        | tDatUn == testDataUnHsType = Just (HsTyInpUn hsTy)
      matchTestDataHsType (HsTyApp (HsTyApp tDatBin hsTy1) hsTy2) 
        | tDatBin == testDataBinHsType = Just (HsTyInpBin hsTy1 hsTy2) 
      matchTestDataHsType _ = Nothing

      -- Split functions whose type matches/doesn't match with TestDataUn/Bin.
      splitTestDataMatch (ufs, Just hyTypeInp) (e, v) = (e, (ufs, hyTypeInp) : v)
      splitTestDataMatch (ufs, Nothing)        (e, v) = (ufs : e, v)

-}
    









  


    
-- The first thing to do when interpreting the user input file is to process 
-- the module elements ('ModuleElem's). According to hint, module elements have 
-- three distinct forms:
--
-- * Functions;
-- * Class declarations;
-- * Data declarations.
--
-- For the purposes of the system, functions are the only module element of 
-- concern. In addition, the system is only compatible with functions whose type 
-- conforms to Haskell 98 standard. Finally, functions are only considered if 
-- they have nullary, unary or binary function types.
--
-- This function processes function ('Fun') module elements and filters 
-- them according to the above requirements. The functions that do not comply 
-- with the requirements are returned to be output via the console in 'Verbose'
-- mode. Note that they /cannot/ be output here, because verbosity is a  
-- setting in the test options, which have yet to be discovered.


-- | Step (1):
-- The first thing to do when interpreting the user input file is to process 
-- the module elements ('ModuleElem's). According to hint, module elements have 
-- three distinct forms:
--
-- * Functions;
-- * Class declarations;
-- * Data declarations.
--
-- For the purposes of the system, functions are the only module element of 
-- concern. In addition, the system is only compatible with functions whose type 
-- conforms to Haskell 98 standard. Finally, functions are only considered if 
-- they have nullary, unary or binary function types.
--
-- This function processes function ('Fun') module elements and filters 
-- them according to the above requirements. The functions that do not comply 
-- with the requirements are returned to be output via the console in 'Verbose'
-- mode. Note that they /cannot/ be output here, because verbosity is a  
-- setting in the test options, which have yet to be discovered.

-- | Step (2):
-- Second is to analyse the functions in the user input file that have been 
-- previously previously processed to determine whether a user has defined 
-- custom test options ('TestOpts'):
--
-- * If /multiple/ test options are given, then an error is thrown. 
-- * If none are given, then the default options will be used. See
--   'defaultTestOpts'.
--
-- As well as locating user-defined test options, they are also validated at the 
-- to ensure:
--
-- * All record fields in 'TestOpts' are initialised.
-- * 'TestOpts' settings are valid. (E.g., subNfRes is only used with nfRes 
--   etc.)
-- The test options are required at this stage because they guide the remainder
-- of the user input file interpreting/checking procedure.


{-
  -- (1) Filter nullary types.
  let nullarys = filter (isNullaryFunType . usrFunHsType) _testFuns
  -- (2) Find @x :: TestOpts@ or use 'defaultTestOpts'.
  tOpts <- filtTestOpts nullarys 
  -- (a) Ensure all 'TestOpts' record fields are initialised.
  initTOpts (fst tOpts)
  -- (b) Ensure 'TestOpts' settings are valid.
  validTOpts (snd tOpts)
  -- (4) Return test options.
  return tOpts

  where 
    -- Find test options among nullary 'UsrFun's:
    -- * Will error if multiple test options are found;
    -- * If no 'TestOpts' are found, will use default options.
    filtTSOpts xs = case filter isTestOptsUsrFun xs of 
      [] -> return (defaultTestOptsName, defaultTestOpts)
      [UsrFun idt _ _] -> (idt, ) <$> interpret idt (as :: TestOpts)
      _ -> throwM (TestOptsErr "Multiple test options specified.")

    -- Check if a 'UsrFun' is a set of test options.
    isTestOptsUsrFun = (== testOptsHsType) . usrFunHsType 

    -- Ensure all record fields in 'TestOpts' are initialised.
    initTOpts tOpsIdt = catch' (interpret (interp_checkInitTestOpts tOpsIdt) 
      (as :: ())) (const . throwM $ TestOptsErr "One or more uninitialised record fields.")

    -- Ensure all 'TestOpts' settings are valid.
    validTOpts tOpts
      -- subNfRes must imply nfRes.
      | not validNf = throwM (TestOptsErr "Conflicting settings: \
         \subNfRes can only be used with nfRes.")
      -- We only allow models to have 'maxPredictors' predictors.
      | not validModels = throwM (AnalOptsErr $ "Regression models can \
        \have a maximum of " ++ show maxPredictors ++ " predictors.")
      | otherwise = return ()
      where
        -- Valid subNfRes/nfRes settings.
        validNf = validNfs (subNfRes tOpts) (nfRes tOpts)
        -- Valid 'AnalOpts' 'models'
        validModels = maxPredictors >= maximum (fmap numPredictors $ models $ analOpts tOpts)
-}





{-
-- * Discover potential test inputs

-- | Step (3):
-- Third is to disover the potential test inputs. The test options have 
-- already been disovered, or the defaults used, so the test programs and test 
-- data are now considered.
-- 
-- This process is delegated two two helper functions:
--
-- * 'discoverPotentialTestPrograms' determines which functions in the user 
--   input file can be tested. If the user has specified which programs 
--   to test in the 'TestOpts' 'progs' list, then it is simply a case of 
--   ensuring these functions are present and performing a number of checks 
--   on them (e.g., to make sure them have the same type). If not, then all 
--   functions in the file have to be considered. In either case, only the 
--   functions previously processed are considered. 
-- * In the case where a user has chosen to provide manual test data, 
--   'discoverPotentialTestData' will locate any test data present in the user 
--   input file from the list of previously processed functions.
discoverPotentialTestInputs
  :: MonadInterpreter m
  => [UsrFun]                                        -- ^ Functions previously processed from the user input file in step (1).
  -> ReaderT TestOpts m ( [([UsrFun], HsTypeInp)]    -- ^ Potential test programs, grouped by their types, with their input types.
                        , [([UsrFun], HsTypeInp)] )  -- ^ Potential test data.
discoverPotentialTestInputs usrFuns = do 

  -- Verbose output.
  outputVerbose "Discovering potential test inputs..."
  
  -- Get 'TestOpts'.
  tOpts <- ask 

  -- (1) Partition functions: nullary vs. unary/binary.
  let (nullarys, nonNullarys) = partition (isNullaryFunType . usrFunHsType) usrFuns

  -- (2) Process the nonNullary functions:
  -- (a) Ensure they are in the 'TestOpts' 'progs' list (unless its empty).
  -- (b) Group them according to their types.
  -- (c) Filter them by monomorphic/polymorphic input types depending on
  --     whether test data is to be generated or is manually specified.
  pUsrFunss <- discoverPotentialTestPrograms nonNullarys

  -- (3) Find 'TestData' if applicable, then return test inputs.
  if dat tOpts == Manual 
  then (pUsrFunss, ) <$> discoverPotentialTestData (fmap snd pUsrFunss) nullarys 
  else do 
    -- Verbose output.
    outputVerbose "Test data to be generated... Test data not being discovered..."
    -- Return potential test programs only.
    return (pUsrFunss, [])


-- ** Discover potential test programs

-- | Step (3a):
-- Considering only the functions previously processed in step (1), discover 
-- potential test programs in the user input file:
-- (a) Find functions in 'TestOpts' 'progs' list, unless its empty, in which 
--     case all functions are considered.
-- (b) Group functions by their types.
-- (c)(i)  If generated test data, only accept functions with monomorphic 
--         input types.
-- (c)(ii) If manual test data, also accept functions with polymorphic input
--         types.
--
-- Note: this function will error if:
-- (i)   No valid unary/binary functions in input file.
-- (ii)  Missing functions specified in 'TestOpts' 'progs' list.
-- (iii) Functions in 'TestOpts' 'progs' list have different types.
-- (iv)  Test data generation selected and polymorphic functions in 'TestOpts' 
--       'progs' list.
-- (v)   Test data generation selected and only polymorphic unary/binary
--       functions in input file. 
-- 
-- At this point the potential test programs have /not/ been tested to see if 
-- they satisfy the required instances for data generation and benchmarking, 
-- i.e., Arbitrary and NFData. This comes later so that we can notify users 
-- should errors arise.
discoverPotentialTestPrograms
  :: MonadInterpreter m 
  => [UsrFun]                                    -- ^ Unary/binary functions previously processed from the user input file in step (1).
  -> ReaderT TestOpts m [([UsrFun], HsTypeInp)]  -- ^ Potential test programs, grouped by their types, with their associated input types.
-- Check (i).
discoverPotentialTestPrograms usrFuns = do 

  -- Verbose output.
  outputVerbose "Discovering potential test programs..."

  case usrFuns of 
    [] -> throwM (InputErr "No valid unary/binary programs specified.")
    _  -> do 

      -- 'TestOpts' 'progs' list.
      ps <- progs <$> ask

      -- Verbose output.
      outputVerbose $ "TestOpts 'progs' list is " ++ 
         if null ps 
         then "empty... All valid functions will be considered..."
         else "not empty... Only functions in the list will be considered..." 

      -- Generate test data?
      genTData <- (/= Manual) . dat <$> ask

      -- (1) Filter functions according to test programs in 'TestOpts' 'progs' list, 
      --     unless it's empty.
      let usrFuns' = filterUsrFuns (fmap strip ps) usrFuns
      -- (2) Sort and group functions by their type.
          usrFunss = groupBy (\uf1 uf2 -> usrFunType uf1 == usrFunType uf2) $ 
                     sortBy (comparing usrFunType) usrFuns'
      -- (3) Filter monomorphic functions.
          genUsrFunss = filter (isValidGenType . usrFunHsType . head) usrFunss

      -- (4) Perform checks:
      -- (ii)
      if | notNull ps && length ps /= length usrFuns' -> throwM (TestOptsErr "Missing \
             \or invalid programs specified in TestOpts 'progs' list.")
      -- (iii)
         | notNull ps && length usrFunss > 1 -> throwM (TestOptsErr "Programs in TestOpts \
             \'progs' list have different types.")
      -- (iv)
         | genTData && notNull ps && length ps > length (concat genUsrFunss) -> throwM
             (TestOptsErr "Test data generation selected but polymorphic \ 
             \programs specified in TestOpts 'progs' list.")
      -- (v)
         | genTData && null genUsrFunss -> throwM (InputErr "Test data generation \
             \selected but no valid monomorphic unary/binary programs specified.")
      -- OK: auto test data, return monos only.
         | genTData -> do 
             
             -- Verbose output.
             outputVerbose "Test data to be generated... Test programs with monomorphic \
               \input types will be considered..."
             outputVerbose 
               $ unlines 
               $ "Invalid functions (not in TestOpts 'progs' list):" 
               : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (usrFuns' \\ usrFuns) 
             outputVerbose 
               $ unlines 
               $ "Invalid functions (polymorphic input types):" 
               : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (concat usrFunss \\ concat genUsrFunss)          
             outputVerbose 
               $ unlines 
               $ "Accepted functions:" 
               : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (concat genUsrFunss)
             
             -- Return potential test programs grouped by their types.
             return 
               $ zip genUsrFunss 
               $ fmap (tyFunToTyInp . usrFunHsType . head) (genUsrFunss)
      -- OK: manual test data, return monos and polys.
         | otherwise -> do 
             
             -- Verbose output.
             outputVerbose "Test data to be specified manually... Test programs with monomorphic and \
               \polymorphic input types will be considered..."
             outputVerbose 
               $ unlines 
               $ "Invalid functions (not in TestOpts 'progs' list):" 
               : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (usrFuns' \\ usrFuns) 
             outputVerbose 
               $ unlines 
               $ "Accepted functions:" 
               : fmap (\(UsrFun idt ty _) -> idt ++ " :: " ++ ty) (concat usrFunss)

             -- Return potential test programs grouped by their types.
             return 
               $ zip usrFunss 
               $ fmap (tyFunToTyInp . usrFunHsType . head) usrFunss

  where 
    -- Functions only in 'TestOpts' 'progs' list.
    filterUsrFuns [] = id
    filterUsrFuns ps = filter ((`elem` ps) . usrFunIdt)

-}
-}