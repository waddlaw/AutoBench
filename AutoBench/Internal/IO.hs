
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall   #-} 

{-|

  Module      : AutoBench.Internal.IO
  Description : AutoBench's IO.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module deals with all AutoBench's IO, including:

  * Outputting messages to the console;
  * Saving to/loading from files;
  * Generating/compiling/executing benchmarking files;
  * Cleaning up temporary files created for/during benchmarking;
  * Handling user interactions.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - generateBenchmarks error handling?
   - It would be nice for the generating benchmarking file to be nicely
     formatted;
   - comment: compileBenchmarkingFile 
-}

module AutoBench.Internal.IO 
  ( 

  -- * User interactions
    selTestSuiteOption                  -- Select a test suite to run from validated 'UserInputs'.
                                        -- Note: in some cases no valid test suites will be available due to
                                        -- input errors, in this case users can review the 'UserInputs'
                                        -- data structure /using this function/.
  -- * IO for benchmarking files
  , generateBenchmarkingFile            -- Generate a benchmarking file to benchmark all the test programs in a given test suite
  , compileBenchmarkingFile             -- Compile benchmarking file using zero or more user-specified compiler flags.
  -- * Helper functions
  , discoverInputFiles                  -- Discover potential input files in the working directory.
  , genBenchmarkingFilename             -- Generate a valid filename for the benchmarking file from the filename of the user input file.
  , printGoodbyeMessage                 -- Say goodbye.

  ) where

import           Control.Monad.Catch       (throwM)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Char                 (toLower)
import           System.Console.Haskeline  (InputT, MonadException, getInputLine)
import           System.Directory          (doesFileExist, getDirectoryContents)
import           System.FilePath.Posix     ( takeBaseName, takeDirectory
                                           , takeExtension )
import qualified Text.PrettyPrint.HughesPJ as PP

import AutoBench.Internal.Utils          (strip)
import AutoBench.Internal.AbstractSyntax (Id, ModuleName, prettyPrint, qualIdt)
import AutoBench.Internal.Types 
  ( DataOpts(..)
  , SystemError(..)
  , TestSuite(..)
  , UserInputs(..)
  , docTestSuite
  , docUserInputs
  )


-- * User interactions 

-- | Select which test suite to run from the 'UserInputs' data structure:
-- 
-- * If one test suite is valid, then this is automatically selected;
-- * If two or more test suites are valid, then users must pick;
-- * If no test suites are valid, then users can review the 'UserInput's 
--   data structure.
--
-- In all cases, users can also review the 'UserInput's data structure.
selTestSuiteOption 
  :: (MonadIO m, MonadException m) 
  => UserInputs 
  -> InputT m [(Id, TestSuite)]    -- Note: to be generalised to one or more 
                                   -- test suites running sequentially.
selTestSuiteOption inps = case _testSuites inps of 
  -- No valid test suites:
  []   -> do
    liftIO $ putStr "\n\n"
    liftIO (putStrLn "  No valid test suites.")
    let go = do 
               liftIO $ putStrLn ""
               liftIO $ putStrLn $ unlines [ "  * View parse results [P]" 
                                           , "  * Exit               [E]" ]
               fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case 
                 Nothing  -> return []
                 Just "e" -> return [] 
                 Just "p" -> liftIO (putStrLn "\n" >> showUserInputs >> putStrLn "\n") >> go
                 Just _   -> inpErr >> go
    go
  -- One valid test suite: automatically select.
  [ts] -> return [ts]
  -- Two or more test suites: user picks /one for the time being/.
  -- This will be generalised to picking multiple for sequential executing.
  _  -> do 
    liftIO $ putStr "\n\n"
    liftIO (putStrLn "  Multiple valid test suites:")
    liftIO (showTestSuites $ _testSuites inps)
    let go = do 
               liftIO $ putStrLn ""
               liftIO $ putStrLn $ unlines [ "  * Run a test suite   [1" ++ endRange
                                           , "  * View test suites   [V]"
                                           , "  * View parse results [P]" 
                                           , "  * Exit               [E]" ]
               fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case 
                 Nothing  -> return []
                 Just "e" -> return [] 
                 Just "p" -> liftIO (putStrLn "\n" >> showUserInputs >> putStrLn "\n") >> go
                 Just "v" -> liftIO (showTestSuites $ _testSuites inps) >> go
                 Just inp -> case reads inp :: [(Int, String)] of 
                   []         -> inpErr >> go
                   (n, _) : _ -> if n >= 1 && n <= l
                                 then return [_testSuites inps !! (n - 1)]
                                 else inpErr >> go
    go
 
  where 
    -- How many test suites are valid?
    l        = length (_testSuites inps)
    endRange = if l > 1
               then ".." ++ show (l :: Int) ++ "]"
               else "]"
    -- Invalid user input message.
    inpErr   = liftIO $ putStrLn "\n Error: invalid choice.\n"

    -- A simplified pretty printing for 'TestSuite's.
    showTestSuites tss = do 
      putStrLn ""
      print $ PP.nest 4 $ PP.vcat $ (PP.punctuate (PP.text "\n") $ 
        fmap (uncurry showTestSuite) $ zip [1..] tss)
      where
        showTestSuite :: Int -> (Id, TestSuite) -> PP.Doc
        showTestSuite idx (idt, ts) = PP.vcat 
          [ PP.text $ "" ++ show idx ++ ") " ++ idt
          , PP.nest 10 $ docTestSuite ts ]

    -- Use the 'docUserInputs' but nest 2.
    showUserInputs = print $ PP.nest 2 $ docUserInputs inps


-- * IO for benchmarking files:

-- | Generate a benchmarking file to benchmark all the test programs in a 
-- given test suite. This includes generating/supplying necessary test data.
generateBenchmarkingFile
  :: FilePath       -- ^ Filepath to save benchmarking file.
  -> ModuleName     -- ^ User input file's module name. 
  -> UserInputs     -- ^ Parsed/categorised user inputs (to cross-reference).
  -> Id             -- ^ The chosen test suite's identifier.
  -> TestSuite      -- ^ The chosen test suite.
  -> IO ()    
generateBenchmarkingFile fp mn inps tsIdt ts = do 
  -- Generate functional call.
  gFunc <- genFunc gen nf unary
  -- Generate file contents.

  -----------------------------------------------------------------------------
  -- ** CHANGING THE CONTENTS WILL BREAK THE SYSTEM **
  ----------------------------------------------------------------------------- 

  let contents = PP.vcat 
                  [ PP.text "" 
                  , PP.text "module Main (main) where"
                  , PP.text ""
                  , PP.text "import qualified AutoBench.Internal.Benchmarking"       -- Import all generation functions.
                  , PP.text "import qualified" PP.<+> PP.text mn                     -- Import user input file.
                  , PP.text ""
                  , PP.text "main :: IO ()"                                          -- Generate a main function.
                  , PP.text "main  = AutoBench.Internal.Benchmarking.runBenchmarks"  -- Run benchmarks.
                      PP.<+> PP.char '(' PP.<> gFunc PP.<>  PP.char ')'              -- Generate benchmarks.
                      PP.<+> PP.text (prettyPrint . qualIdt mn $ tsIdt)              -- Identifier of chosen test suite (for run cfg).
                  ]
  -- Write to file.
  writeFile fp (PP.render contents)

  where 
    ---------------------------------------------------------------------------
    -- ** CHANGING THE NAMES OF THESE FUNCTIONS WILL BREAK THE SYSTEM **
    --------------------------------------------------------------------------- 
     
    -- Generate benchmarking function call.
    -- genFunc gen? nf? unary?
    genFunc :: Bool -> Bool -> Bool -> IO PP.Doc
    -- genBenchmarksGenNfUn:    
    -- Generated test data, results to nf, unary test programs.
    genFunc True True True = return 
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenNfUn")                                     
    -- genBenchmarksGenWhnfUn:    
    -- Generated test data, results to whnf, unary test programs.
    genFunc True False True = return 
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenWhnfUn")             
     -- genBenchmarksGenNfBin:
     -- Generated test data, results to nf, binary test programs.                                     
    genFunc True  True  False = return 
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenNfBin")
    -- genBenchmarksGenWhnfBin:
    -- Generated test data, results to whnf, binary test programs.                                                                
    genFunc True  False False = return 
      (genGenFunc "AutoBench.Internal.Benchmarking.genBenchmarksGenWhnfBin")
    -- genBenchmarksManNfUn:
    -- User-specified test data, results to nf, unary test programs.                                      
    genFunc False True True = 
      genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManNfUn"
    -- genBenchmarksManWhnfUn:
    -- User-specified test data, results to whnf, unary test programs.                                          
    genFunc False False True = 
      genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManWhnfUn"
    -- genBenchmarksManNfBin:
    -- User-specified test data, results to nf, binary test programs.
    genFunc False True False = 
       genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManNfBin"
    -- genBenchmarksManWhnfBin:
    -- User-specified test data, results to whnf, binary test programs.                                 
    genFunc False False False = 
       genManFunc "AutoBench.Internal.Benchmarking.genBenchmarksManWhnfBin"

    -- Generate function call for benchmarks requiring automatically generated 
    -- test data.
    genGenFunc :: Id -> PP.Doc   
    genGenFunc func = PP.hsep $ 
        [ PP.text func
        , ppList $ fmap ppTuple qualProgs
        , PP.text (prettyPrint . qualIdt mn $ tsIdt)
        ]    

    -- Generate function call for benchmarks using user-specified test data.
    genManFunc :: Id -> IO PP.Doc
    genManFunc func = do
      dat <- getManualDatIdt (_dataOpts ts)
      return $ PP.hsep
        [ PP.text func
        , ppList $ fmap ppTuple qualProgs
        , PP.text (prettyPrint . qualIdt mn $ tsIdt)
        , PP.text (prettyPrint . qualIdt mn $ dat)
        ] 

    -- Pretty print a (identifier, program) tuple.
    ppTuple :: Id -> PP.Doc
    ppTuple idt = PP.char '('
      PP.<> PP.text (show idt)
      PP.<> PP.text ", "
      PP.<> PP.text idt
      PP.<> PP.char ')'

    -- Pretty print a comma-separated list.
    ppList :: [PP.Doc] -> PP.Doc
    ppList docs = PP.hcat $ 
      PP.char '[' : (PP.punctuate (PP.text ", ") docs) ++ [PP.char ']']

    -- Helpers 
    
    -- Classifiers:
    unary = head (_progs ts) `elem` fmap fst (_unaryFuns inps)  -- Unary test programs?
    nf    = _nf ts                                              -- NF test results?
    gen   = case _dataOpts ts of                                -- Generate test data?
      Manual{} -> False 
      Gen{}    -> True

    -- All test programs are qualified with the module name.
    qualProgs = fmap (prettyPrint . qualIdt mn) (_progs ts)
    
    -- Get the identifier of User-specified test data from 'DataOpts'.
    -- Questionable throwM error handling here/better than a partial function.
    getManualDatIdt :: DataOpts -> IO Id
    getManualDatIdt (Manual s) = return s 
    getManualDatIdt Gen{} = 
      throwM (InternalErr $ "generateBenchmarks: unexpected 'Gen' setting.")




                                                                                  -- ** COMMENT ** 
compileBenchmarkingFile 
  :: FilePath     -- ^ Benchmarking filepath.
  -> [String]     -- ^ GHC compiler flags.
  -> FilePath     -- ^ User input filepath.
  -> IO ()   
compileBenchmarkingFile = undefined

-- * Helper functions 

-- | Generate a valid filename for the benchmarking file from the filename of 
-- the user input file.
genBenchmarkingFilename :: String -> IO String 
genBenchmarkingFilename s = do 
  b1 <- doesFileExist s'
  b2 <- doesFileExist (addSuffix s')
  if b1 || b2
  then go s' 0
  else return (addSuffix s')
  where 
    go :: String -> Int -> IO String
    go s_ i = do 
      let s_' = s_ ++ show i
      b1 <- doesFileExist s_'
      b2 <- doesFileExist (addSuffix s_')
      if b1 || b2
      then go s_ (i + 1)
      else return (addSuffix s_')

    addSuffix = (++ ".hs")
    s'        = takeDirectory s ++ "/Bench" ++ takeBaseName s

-- | Discover potential input files in the working directory.
discoverInputFiles :: IO [FilePath]
discoverInputFiles  = filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

-- Say goodbye.
printGoodbyeMessage :: IO () 
printGoodbyeMessage  = putStrLn "Leaving AutoBench."