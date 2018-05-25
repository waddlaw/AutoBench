
{-# LANGUAGE LambdaCase #-}

import qualified Options.Applicative as OPTS
import Language.Haskell.Interpreter
import Control.Exception.Base

import AutoBench.Internal.Hint 
import AutoBench.Internal.UserInputChecks
import AutoBench.Internal.AbstractSyntax
import AutoBench.Internal.Types
import AutoBench.Internal.Utils
import AutoBench.Internal.IO 


import qualified Text.PrettyPrint.HughesPJ as PP


import System.Directory           ( doesFileExist, getDirectoryContents
                                  , removeFile )


import System.Console.Haskeline hiding (throwIO)


runAndHandle :: Interpreter a -> IO a
runAndHandle  = (either throwIO return =<<) . runInterpreter


main :: IO () 
main = do
  --args <- OPTS.customExecParser (OPTS.prefs OPTS.showHelpOnError) $ clArgsParser

  let fp = "Input.hs"
      mn = filepathToModuleName fp
  
  putStr "\n  \9656 Processing input file "
  inps <- processUserInputFile fp
  putStrLn "\10004"
  (runInputT defaultSettings $ selTestSuiteOption inps) >>= \case 
    [(idt, ts)] -> do 
      putStrLn $ "  \9656 Running test suite '" ++ idt ++ "'"
      putStr   $ "     Generating benchmarking file "  
      benchFP <- genBenchmarkingFilename fp
      generateBenchmarkingFile benchFP mn inps idt ts 
      putStrLn "\10004"






    _ -> printGoodbyeMessage





processUserInputFile :: FilePath -> IO UserInputs
processUserInputFile  = (either throwIO return =<<) . runInterpreter . userInputCheck


