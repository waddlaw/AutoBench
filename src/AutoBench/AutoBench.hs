 
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
-- * Sanitise runtimes

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
import           AutoBench.Internal.Configuration   (defaultBenchmarkReportFilepath)
import qualified AutoBench.Internal.PrettyPrinting  as PPLib
import           AutoBench.Internal.UserInputChecks ( quickCheckTestPrograms
                                                    , userInputCheck )
import           AutoBench.Internal.UserIO          ( selTestSuiteOption
                                                    , printGoodbyeMessage )
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

import AutoBench.Internal.Types          
  ( DataOpts(..)
  , InputError(..)
  , TestSuite(..)
  , UserInputs
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

  -- Check whether user input file exists: if not throw a filepath error.
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
  nestPutStr 2 $ PPLib.arrowBullet "Processing" PP.<+> PPLib.toItalic fp                 
  inps <- processUserInputFile fp
  nestPutStrLn 1 PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 2: Have the user select which test suite to run. If there's only
  --         one valid test suite, then it will be automatically selected.
  -----------------------------------------------------------------------------
  runInputT defaultSettings (selTestSuiteOption inps) >>= \case                       
    [(idt, ts)] -> do
    -- Note: 'idt' is the identifier of the selected test suite and 'ts' is the
    -- test suite ('TestSuite') itself.
      nestPutStrLn 2 $ PPLib.arrowBullet "Running" PP.<+> PPLib.toItalic idt

  -----------------------------------------------------------------------------
  -- Step 3: Check whether test programs are denotationally equal using 
  --         QuickCheck.
  -----------------------------------------------------------------------------
      nestPutStr 5 $ PPLib.circleBullet "QuickChecking test programs"
      eql <- quickCheck ts inps                                                       
      if eql                                                                           
        then nestPutStrLn 1 PPLib.tick
        else nestPutStrLn 1 PPLib.cross           
  
  -----------------------------------------------------------------------------
  -- Step 4: Generate benchmarking file.
  -----------------------------------------------------------------------------
      nestPutStr 5 $ PPLib.circleBullet "Generating benchmarking file"  
      benchFP <- generateBenchmarkingFilename fp                                        
      finally (do generateBenchmarkingFile benchFP mn inps idt ts  
                  nestPutStrLn 1 PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 5: Compile benchmarking file. Print any invalid compiler flags 
  --         given by the user.
  -----------------------------------------------------------------------------
                  nestPutStrLn 5 $ PPLib.circleBullet "Compiling benchmarking file..."    
                  invalidFlags <- compileBenchmarkingFile benchFP fp (_ghcFlags ts) 
                  spacer 1
                  nestPutStr 5 $ PPLib.circleBullet "Compiled benchmarking file" 
                  if null invalidFlags
                    then nestPutStrLn 1 PPLib.tick
                    else do nestPutStrLn 1 PPLib.questionMark
                            printInvalidCompilerFlags invalidFlags

  -----------------------------------------------------------------------------
  -- Step 6: Execute benchmarking file.
  -----------------------------------------------------------------------------
                  nestPutStrLn 5 $ PPLib.circleBullet "Executing benchmarking file..."            
                  spacer 1
                  execute (dropExtension benchFP)
                  nestPutStrLn 5 $ PPLib.circleBullet "Executed benchmarking file" 
                    PP.<+> PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 7: Delete benchmarking files created by the system.
  -----------------------------------------------------------------------------
                  deleteBenchmarkingFiles benchFP fp                                      

  -----------------------------------------------------------------------------
  -- Step 8: Generate test report.
  -----------------------------------------------------------------------------
                  nestPutStr 5 $ PPLib.circleBullet "Generating test report" 
                  -- Note: update compiler flags to remove any invalid ones.            
                  let newFlags = _ghcFlags ts \\ invalidFlags                            
                  testRep <- generateTestReport mn ts { _ghcFlags = newFlags } 
                    (benchRepFilename ts) eql
                  nestPutStrLn 1 PPLib.tick

  -----------------------------------------------------------------------------
  -- Step 9: Analyse test report.
  -----------------------------------------------------------------------------
                  nestPutStrLn 5 $ PPLib.circleBullet "Analysing performance results..."          
                  analyseWith (_analOpts ts) testRep 
                  spacer 1

  -----------------------------------------------------------------------------
  -- Step 10: Delete the remaining temporary files created by the system.
  -----------------------------------------------------------------------------
                  deleteTemporarySystemFiles (tempSystemFiles ts)                     

  -----------------------------------------------------------------------------
  -- Step 11: Exit the system.
  ----------------------------------------------------------------------------- 
                  anyKeyExit 

  -----------------------------------------------------------------------------
  -- Finally: Delete all files created by the system.
  -----------------------------------------------------------------------------                                                                
              ) (deleteAllFiles benchFP fp $ tempSystemFiles ts)  

  -----------------------------------------------------------------------------
  --  In no test suite selected, print goodbye message.
  -----------------------------------------------------------------------------  
    _ -> printGoodbyeMessage

  where 

    -- Runner for the 'hint' monad. 'throwIO' any errors.
    processUserInputFile :: FilePath -> IO UserInputs
    processUserInputFile = 
      (either throwIO return =<<) . runInterpreter . userInputCheck

    -- If '_critCfg' in 'TestSuite' doesn't contain a JSON benchmarking report 
    -- file, then use AutoBench's default to interface with Criterion.
    benchRepFilename :: TestSuite -> FilePath
    benchRepFilename  = 
      fromMaybe defaultBenchmarkReportFilepath . reportFile . _critCfg

    -- Print invalid compiler flags to warn users.    
    printInvalidCompilerFlags :: [String] -> IO () 
    printInvalidCompilerFlags [] = return ()
    printInvalidCompilerFlags xs =
      putStrLn $ PP.renderStyle (PPLib.lineStyle 80) $ PP.nest 9 $ PP.sep 
        [ PP.text "Warning, invalid compiler flags:"
        , PP.nest 1 $ PP.fcat $ PP.punctuate (PP.text ", ") $ 
            fmap PPLib.toItalic xs ]

    -- Temporary system files to delete after benchmarking.
    tempSystemFiles :: TestSuite -> [FilePath]
    tempSystemFiles ts
     | isNothing (reportFile $ _critCfg ts) = [defaultBenchmarkReportFilepath]
     | otherwise = []

    -- Use QuickCheck to check if test programs are denotationally equal. 
    -- Note: doesn't work for manual test data currently.
    quickCheck :: TestSuite -> UserInputs -> IO Bool 
    quickCheck ts inps = case _dataOpts ts of 
      Gen{} -> (either throwIO return =<<) . runInterpreter $ 
        quickCheckTestPrograms fp (_progs ts) inps
      -- Can't check test programs using manual test data.                                          -- <TO-DO>: Allow manual data testing here.
      Manual{} -> return False

-- * Helper functions

-- | Catch all exceptions: try to match against a specific exception handler. 
-- If not then just 'print' the exception.
catchSomeException :: SomeException -> IO ()
catchSomeException e = do 
  spacer 1
  case catchInterpreterError of 
    Nothing -> do 
      print e
      putStr "Testing cancelled. "
      anyKeyExit
    Just m -> do 
      m
      putStr "Testing cancelled. "
      anyKeyExit
  where 
    -- Exception handling for hint 'InterpreterError's.
    catchInterpreterError :: Maybe (IO ())
    catchInterpreterError  = case fromException e of 
      Just (UnknownError  s) -> Just $ putStrLn s 
      Just (WontCompile  es) -> 
        Just . putStrLn . unlines . nub . map errMsg $ es
      Just (NotAllowed    s) -> Just $ putStrLn s  
      Just (GhcException  s) -> Just $ putStrLn s  
      _ -> Nothing