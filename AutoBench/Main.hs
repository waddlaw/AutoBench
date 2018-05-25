
{-# OPTIONS_GHC -Wall   #-} 
{-# LANGUAGE LambdaCase #-}

-- import qualified Options.Applicative as OPTS

import Control.Exception.Base       (throwIO)
import System.Console.Haskeline     (defaultSettings, runInputT)
import Language.Haskell.Interpreter (runInterpreter)

import AutoBench.Internal.Types           (UserInputs)
import AutoBench.Internal.UserInputChecks (userInputCheck)
import AutoBench.Internal.Utils           (filepathToModuleName)
import AutoBench.Internal.IO              
  ( genBenchmarkingFilename 
  , generateBenchmarkingFile
  , printGoodbyeMessage
  , selTestSuiteOption 
  )



main :: IO () 
main = do
  --args <- OPTS.customExecParser (OPTS.prefs OPTS.showHelpOnError) $ clArgsParser

  let fp = "./Input.hs"
      mn = filepathToModuleName fp

  putStr "\n  \9656 Processing input file "                                      -- (1) Process user input file.
  inps <- processUserInputFile fp
  putStrLn "\10004"
  (runInputT defaultSettings $ selTestSuiteOption inps) >>= \case                -- (2) Select test suite.
    [(idt, ts)] -> do 
      putStrLn $ "  \9656 Running test suite '" ++ idt ++ "'"
      putStr   $ "     Generating benchmarking file "  
      benchFP <- genBenchmarkingFilename fp                                      -- (3) Generate benchmarking file.
      generateBenchmarkingFile benchFP mn inps idt ts                            
      putStrLn "\10004"
      putStr   $ "     Compiling benchmarking file "                             -- (4) Compile benchmarking file.

    _ -> printGoodbyeMessage

  where 

    processUserInputFile :: FilePath -> IO UserInputs
    processUserInputFile  = 
      (either throwIO return =<<) . runInterpreter . userInputCheck

