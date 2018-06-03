 
{-# OPTIONS_GHC -Wall   #-}
{-# LANGUAGE LambdaCase #-}

{-|

  Module      : AutoBench.AutoBench
  Description : Main file for AutoBenching.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  Main file for AutoBenching.

-}

-- import qualified Options.Applicative as OPTS

module Main (main) where

import           Control.Exception.Base       ( SomeException, catch, finally 
                                              , fromException, throwIO )
import           Control.Monad                (void)
import           Criterion.Types              (reportFile)
import           Data.List                    ((\\), nub)
import           Data.Maybe                   (fromMaybe)
import           Language.Haskell.Interpreter ( InterpreterError(..)
                                              , errMsg, runInterpreter )
import           System.Console.Haskeline     (defaultSettings, runInputTg)
import           System.FilePath.Posix        (dropExtension)
import qualified Text.PrettyPrint.HughesPJ    as PP

import AutoBench.Internal.Analysis        (analyseWith)
import AutoBench.Internal.UserInputChecks ( quickCheckTestPrograms
                                          , userInputCheck )
import AutoBench.Internal.UserIO          ( selTestSuiteOption
                                          , printGoodbyeMessage )
import AutoBench.Internal.Utils           (filepathToModuleName, wrapPPList)

import AutoBench.Internal.IO              
  ( compileBenchmarkingFile
  , deleteBenchmarkingFiles
  , execute
  , generateBenchmarkingFilename 
  , generateBenchmarkingFile
  , generateTestReport
  )

import AutoBench.Internal.Types          
  ( DataOpts(..)
  , TestSuite(..)
  , UserInputs
  , defBenchRepFilename
  )


{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - Sanitise runtimes?
   - Use PP here;
   -
-}

-- | To AutoBench a file containing test inputs, use @./AutoBench <filename>@
main :: IO () 
main  = flip catch catchSomeException $ do 

  --args <- OPTS.customExecParser (OPTS.prefs OPTS.showHelpOnError) $ clArgsParser

  let fp = "./Input.hs"
      mn = filepathToModuleName fp
  
  putStrLn ""
  putStr $ poorNest 2 $ "\9656 Processing \ESC[3m" ++ fp ++ "\ESC[0m"                     -- (1) Process user input file.
  inps <- processUserInputFile fp
  putStrLn $ poorNest 1 "\10004"
  (runInputT defaultSettings $ selTestSuiteOption inps) >>= \case                         -- (2) Select test suite.
    [(idt, ts)] -> do 
      putStrLn $ poorNest 2 $ "\9656 Running \ESC[3m" ++ idt ++ "\ESC[0m"      
      putStr   $ poorNest 5 $ "\8226 QuickChecking test programs"
      eql <- quickCheck fp ts inps                                                        -- (3) Check whether test programs are semantically
      if eql                                                                              --     equal using QuickCheck, if applicable.
      then putStrLn $ poorNest 1 "\10004"
      else putStrLn $ poorNest 1 "\10007"                                                            
      putStr   $ poorNest 5 $ "\8226 Generating benchmarking file"  
      benchFP <- generateBenchmarkingFilename fp                                          -- (4) Generate benchmarking file.
      finally (do generateBenchmarkingFile benchFP mn inps idt ts                        
                  putStrLn $ poorNest 1 "\10004"
                  putStrLn $ poorNest 5 "\8226 Compiling benchmarking file..."            -- (5) Compile benchmarking file.
                  invalidFlags <- compileBenchmarkingFile benchFP fp (_ghcFlags ts) 
                  printInvalidFlags invalidFlags
                  putStrLn $ poorNest 5 "\8226 Executing benchmarking file..."            -- (6) Execute benchmarking file.
                  putStrLn ""
                  execute (dropExtension benchFP)
                  putStrLn $ poorNest 5 "\8226 Executed benchmarking file \10004"                    
                  putStr $ poorNest 5 "\8226 Generating test report"                      -- (7) Generate test report.
                  let newFlags = _ghcFlags ts \\ invalidFlags                             --     Note: update compiler flags to remove any invalid ones.
                  testRep <- generateTestReport mn ts { _ghcFlags = newFlags } 
                    (benchRepFilename ts) eql
                  putStrLn $ poorNest 1 "\10004"
                  putStr $ poorNest 5 "\8226 Analysing results..."                        -- (8) Analyse test results.
                  putStrLn ""
                  analyseWith (_analOpts ts) testRep 
                  putStrLn ""
                  anyKeyExit                                                              -- (9) Any key to exit and goodbye.
              ) (deleteBenchmarkingFiles benchFP fp $ tempSysFiles ts)                    -- (X) Finally delete temporary files.
    _ -> printGoodbyeMessage

  where 

    -- Exception handling: ----------------------------------------------------

    -- Catch any exception and try and match it against a specific 
    -- exception handler. If not then just 'print' the error.
    catchSomeException :: SomeException -> IO ()
    catchSomeException e = do 
      putStrLn "\n"
      case catchInterpreterError e of 
        Nothing -> do 
          print e
          putStr "Testing cancelled. "
          anyKeyExit
        Just m -> do 
          m
          putStr "Testing cancelled. "
          anyKeyExit

    -- Exception handling for hint 'InterpreterError's.
    catchInterpreterError :: SomeException -> Maybe (IO ())
    catchInterpreterError e = case fromException e of 
      Just (UnknownError  s) -> Just $ putStrLn s 
      Just (WontCompile  es) -> 
        Just . putStrLn . unlines . nub . map errMsg $ es
      Just (NotAllowed    s) -> Just $ putStrLn s  
      Just (GhcException  s) -> Just $ putStrLn s  
      _ -> Nothing

    -- IO helpers: ------------------------------------------------------------

    -- Any key to exit.
    anyKeyExit :: IO ()
    anyKeyExit  = do 
      putStr "Press any key to exit... "
      -- Want to use Haskeline here but can't.
      -- https://github.com/judah/haskeline/issues/74
      void getChar
      printGoodbyeMessage

    -- Use QuickCheck to check if test programs are semantically 
    -- equal. Note: doesn't work for manual test data currently.
    quickCheck :: FilePath -> TestSuite -> UserInputs -> IO Bool 
    quickCheck fp ts inps = case (_dataOpts ts) of 
      Gen{} -> (either throwIO return =<<) . runInterpreter $ 
        quickCheckTestPrograms fp (_progs ts) inps
      -- Can't check test programs using manual test data.                                          -- <TO-DO>: Allow manual data testing here.
      Manual{} -> return False

    -- Print invalid compiler flags to warn users.    
    printInvalidFlags :: [String] -> IO () 
    printInvalidFlags [] = do
      putStrLn ""
      putStrLn $ poorNest 5 "\8226 Compiled benchmarking file \10004"
    printInvalidFlags xs = do 
      putStrLn ""
      putStrLn $ poorNest 5 "\8226 Compiled benchmarking file \63"
      putStr $ poorNest 9 $ "Warning, invalid compiler flags:"
      let output1 = wrapPPList 40  ", " xs'
          output2 = wrapPPList 100 ", " xs'   
      if sum (fmap length xs) > 25                                                                  -- Hacky!
      then do 
        putStrLn ""
        putStrLn $ PP.render $ PP.nest 11 output2
      else putStrLn $ poorNest 1 $ PP.render output1
      where xs' = fmap (\x -> "\ESC[3m" ++ x ++ "\ESC[0m") xs

    -- Runner for the 'hint' monad. 'throwIO' any errors.
    processUserInputFile  = 
      (either throwIO return =<<) . runInterpreter . userInputCheck

    -- Other helpers: ---------------------------------------------------------

    -- Poor pretty printing.
    poorNest :: Int -> String -> String 
    poorNest n = (replicate n ' ' ++)
      
    -- Temporary system files to delete after benchmarking.
    tempSysFiles ts
      | reportFile (_critCfg ts) == Nothing = [defBenchRepFilename]
      | otherwise = []

    -- If '_critCfg' in 'TestSuite' doesn't contain a JSON benchmarking report 
    -- file, then use AutoBench's default to interface with Criterion.
    benchRepFilename :: TestSuite -> String
    benchRepFilename ts = fromMaybe defBenchRepFilename (reportFile $ _critCfg ts)