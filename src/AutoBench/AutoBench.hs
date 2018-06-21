 
{-# OPTIONS_GHC -Wall   #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Module      : AutoBench.AutoBench
-- Description : Main file for executing AutoBench
-- Copyright   : (c) 2018 Martin Handley
-- License     : BSD-style
-- Maintainer  : martin.handley@nottingham.ac.uk
-- Stability   : Experimental
-- Portability : GHC
--
-- Main file for executing AutoBench.
--
-- See the system's GitHub page for a Quick Start manual:
-- https://github.com/mathandley/AutoBench
--  
-- A paper introducing the system, "AutoBench: comparing the time performance 
-- of Haskell programs", is also available: 
-- http://www.cs.nott.ac.uk/~psxmah/autobench.pdf
--
-- To AutoBench a file containing test inputs, use @AutoBench <filename>@.
--

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------
-- * Sanitise runtimes;
-- * QuickCheck testing for test suites that use manual test data;

module Main (main) where

import           Control.Exception.Base       ( SomeException, catch, finally 
                                              , fromException, throwIO )
import           Control.Monad                (unless)
import           Criterion.Types              (reportFile)
import           Data.List                    ((\\), nub)
import           Data.Maybe                   (fromMaybe, isNothing)
import           Language.Haskell.Interpreter ( InterpreterError(..)
                                              , errMsg, runInterpreter )
import qualified Options.Applicative          as OPTS
import           System.Console.Haskeline     (defaultSettings, runInputT)
import           System.Directory             (doesFileExist)
import           System.FilePath.Posix        (dropExtension)
import qualified Text.PrettyPrint.HughesPJ    as PP

import           AutoBench.Internal.Analysis        (analyseWith)
import qualified AutoBench.Internal.Configuration   as Config
import qualified AutoBench.Internal.PrettyPrinting  as PPLib
import           AutoBench.Internal.Types           ( DataOpts(..)
                                                    , InputError(..)
                                                    , TestSuite(..), UserInputs )
import           AutoBench.Internal.UserInputChecks ( quickCheckTestPrograms
                                                    , userInputCheck )
import           AutoBench.Internal.UserIO          ( printGoodbyeMessage
                                                    , selTestSuiteOption )
import           AutoBench.Internal.Utils           ( CLArgs(..), clArgsParser
                                                    , filepathToModuleName )

import AutoBench.Internal.IO              
  ( anyKeyExit
  , compileBenchmarkingFile
  , deleteAllFiles
  , deleteBenchmarkingFiles
  , deleteTemporarySystemFiles
  , execute
  , generateBenchmarkingFile
  , generateBenchmarkingFilename 
  , generateTestReport
  , nestPutStr
  , nestPutStrLn
  , spacer
  )

-- * Top-level 

-- | Top-level function for the AutoBench executable. To AutoBench a file
-- containing test inputs, use @AutoBench <filename>@.
main :: IO () 
main  = flip catch catchSomeException $ do -- Catch and handle all exceptions.

  -- Process command line arguments.
  args <- OPTS.customExecParser (OPTS.prefs OPTS.showHelpOnError) clArgsParser
  
  -- User input file, e.g., "./Input.hs".
  let fp = _userInputFile args  

  -- Check whether user input file exists.
  -- If not throw a filepath error.
  exists <- doesFileExist fp
  unless exists $ throwIO $ FilePathErr fp

  -- Right now the only option is to benchmark the test inputs and then 
  -- immediately analyse performance results. In the future additional options 
  -- will be added, e.g., benchmark only, analyse only.
  benchmarkAndAnalyse fp

-- * Benchmarking and analysis
  
-- | Benchmark test inputs and immediately analyse time performance results.
benchmarkAndAnalyse :: FilePath -> IO ()
benchmarkAndAnalyse fp = flip catch catchSomeException $ do -- Catch and handle all exceptions.
  
  -- Module name from filepath, e.g., "Input.hs" => "Input".
  let mn = filepathToModuleName fp  

  -----------------------------------------------------------------------------
  -- Step 1: Process the user input file.
  -----------------------------------------------------------------------------
  spacer 1
  nestPutStr 2 $ PPLib.arrowBullet "Processing" PP.<+> PPLib.italic fp                 
  inps <- processUserInputFile fp
  nestPutStrLn 1 PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 2: Have the user select which test suite to run. If there's only
  --         one valid test suite it will be automatically selected.
  -----------------------------------------------------------------------------
  runInputT defaultSettings (selTestSuiteOption inps) >>= \case                       
    [(idt, ts)] -> do
    -- Note: 'idt' is the identifier of the selected test suite and 'ts' is the
    -- test suite ('TestSuite') itself.
      nestPutStrLn 2 $ PPLib.arrowBullet "Running" PP.<+> PPLib.italic idt

  -----------------------------------------------------------------------------
  -- Step 3: Check whether test programs are denotationally equal using 
  --         QuickCheck.
  -- Note: this currently only works for test suites that use randomly generated 
  -- test data.
  -----------------------------------------------------------------------------
      nestPutStr 5 $ PPLib.circleBullet "QuickChecking test programs"
      eql <- quickCheck ts inps                                                       
      if eql                                                                           
        then nestPutStrLn 1 PPLib.tick    -- Denotationally equal.
        else nestPutStrLn 1 PPLib.cross   -- Not denotationally equal.         
  
  -----------------------------------------------------------------------------
  -- Step 4: Generate benchmarking file.
  -----------------------------------------------------------------------------
      nestPutStr 5 $ PPLib.circleBullet "Generating benchmarking file"  
      benchFP <- generateBenchmarkingFilename fp                                        
      finally (do generateBenchmarkingFile benchFP mn inps idt ts  
                  nestPutStrLn 1 PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 5: Compile benchmarking file. Print any invalid compiler flags 
  --         specified by the user.
  -----------------------------------------------------------------------------
                  nestPutStrLn 5 $ PPLib.circleBullet "Compiling benchmarking file..."    
                  invalidFlags <- compileBenchmarkingFile benchFP fp (_ghcFlags ts) 
                  spacer 1
                  nestPutStr 5 $ PPLib.circleBullet "Compiled benchmarking file" 
                  if null invalidFlags
                    then nestPutStrLn 1 PPLib.tick                  -- No invalid flags.
                    else do nestPutStrLn 1 PPLib.questionMark       -- One ore more invalid flags.
                            printInvalidCompilerFlags invalidFlags

  -----------------------------------------------------------------------------
  -- Step 6: Execute benchmarking file.
  -----------------------------------------------------------------------------
                  nestPutStrLn 5 $ PPLib.circleBullet "Executing benchmarking file..."            
                  spacer 1
                  execute (dropExtension benchFP)
                  spacer 1                                                                          -- <TO-DO>: need a spacer here?
                  nestPutStrLn 5 $ PPLib.circleBullet "Executed benchmarking file" 
                    PP.<+> PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 7: Delete benchmarking files created by the system (including .o/.hi 
  --         files associated with user input file).
  -----------------------------------------------------------------------------
                  deleteBenchmarkingFiles benchFP fp                                      

  -----------------------------------------------------------------------------
  -- Step 8: Generate test report.
  -----------------------------------------------------------------------------
                  nestPutStr 5 $ PPLib.circleBullet "Generating test report" 
                  -- Note: update compiler flags to remove any invalid ones.            
                  let validFlags = nub (_ghcFlags ts \\ invalidFlags)                            
                  testRep <- generateTestReport mn ts { _ghcFlags = validFlags } 
                    (benchReportFilepath ts) eql
                  nestPutStrLn 1 PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 9: Analyse test report.
  -----------------------------------------------------------------------------
                  nestPutStrLn 5 $ PPLib.circleBullet "Analysing performance results..."  
                  spacer 1        
                  analyseWith (_analOpts ts) testRep 

  -----------------------------------------------------------------------------
  -- Step 10: Delete the remaining temporary files created by the system.
  -----------------------------------------------------------------------------
                  deleteTemporarySystemFiles (temporarySystemFiles ts)                     

  -----------------------------------------------------------------------------
  -- Step 11: Exit the system.
  ----------------------------------------------------------------------------- 
                  spacer 1
                  anyKeyExit 

  -----------------------------------------------------------------------------
  -- Finally: Delete all files created by the system.
  -----------------------------------------------------------------------------                                                                
              ) (deleteAllFiles benchFP fp $ temporarySystemFiles ts)  

  -----------------------------------------------------------------------------
  --  In no test suite selected, print goodbye message.
  -----------------------------------------------------------------------------  
    _ -> printGoodbyeMessage

  where 

    -- Process the user input file and parse/categorise test inputs into the
    -- 'UserInputs' data structure. This process requires considerable 
    -- validation. (See 'AutoBench.Internal.UserInputChecks').
    -- Uses the runner for the 'hint' monad and 'throwIO's any errors.
    processUserInputFile :: FilePath -> IO UserInputs
    processUserInputFile  = 
      (either throwIO return =<<) . runInterpreter . userInputCheck

    -- If '_critCfg' in 'TestSuite' doesn't contain a JSON benchmarking report 
    -- file, then use AutoBench's default to interface with Criterion.
    benchReportFilepath :: TestSuite -> FilePath
    benchReportFilepath  = 
      fromMaybe Config.defaultBenchmarkReportFilepath . reportFile . _critCfg

    -- Print invalid user-specified compiler flags (in '_ghcFlags') to warn                         -- <TO-DO>: should we pause on invalid flags?
    -- users.    
    printInvalidCompilerFlags :: [String] -> IO () 
    printInvalidCompilerFlags [] = return ()
    printInvalidCompilerFlags xs =
      putStrLn $ PP.renderStyle (PPLib.lineStyle 80) $ PP.nest 9 $ PP.sep 
        [ PP.text "Warning, invalid compiler flags:"
        , PP.nest 1 $ PPLib.wrappedList $ fmap PPLib.italic xs ]

    -- Temporary system files to delete after benchmarking.
    temporarySystemFiles :: TestSuite -> [FilePath]
    temporarySystemFiles ts
     | isNothing (reportFile $ _critCfg ts) = [Config.defaultBenchmarkReportFilepath]
     | otherwise = []

    -- Use QuickCheck to check if test programs are denotationally equal. 
    -- Note: currently only supports test suites that use random test data.
    quickCheck :: TestSuite -> UserInputs -> IO Bool 
    quickCheck ts inps = case _dataOpts ts of 
      Gen{} -> (either throwIO return =<<) . runInterpreter $ 
        quickCheckTestPrograms fp (_progs ts) inps
      -- Can't check test suites using manual test data.                                            -- <TO-DO>: Allow manual data testing here.
      Manual{} -> return False

-- * Helper functions

-- | Catch all exceptions: try to match against a specific exception handler. 
-- If not then just 'print' the exception.
catchSomeException :: SomeException -> IO ()
catchSomeException e = do 
  spacer 1
  maybe (do print e 
            putStr "Testing cancelled. "
            anyKeyExit) 
        (\m -> do m 
                  putStr "Testing cancelled. "
                  anyKeyExit)
        catchInterpreterError
  where 
    -- Exception handling for hint 'InterpreterError's.
    catchInterpreterError :: Maybe (IO ())
    catchInterpreterError  = fromException e >>= \case
      (UnknownError s) -> Just $ putStrLn s 
      (WontCompile es) -> Just . putStrLn . unlines . nub . map errMsg $ es
      (NotAllowed   s) -> Just $ putStrLn s  
      (GhcException s) -> Just $ putStrLn s  