
{-# LANGUAGE LambdaCase #-}

import qualified Options.Applicative as OPTS
import Language.Haskell.Interpreter
import Control.Exception.Base

import AutoBench.Hint 
import AutoBench.UserInputChecks
import AutoBench.AbstractSyntax
import AutoBench.Types
import AutoBench.Utils
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

  let fp = "./Input.hs"
      mn = filepathToModuleName fp

  
  putStr "\n  \9656 Processing input file "
  inps <- processUserInputFile fp
  putStrLn "\10004"
  (runInputT defaultSettings $ selOption inps) >>= \case 
    [(idt, ts)] -> do 
      putStrLn $ "  \9656 Running test suite '" ++ idt ++ "'"
      putStr   $ "     Generating benchmarking file "  
      generateBenchmarks "./TestBENCH.hs" mn inps idt ts 
      putStrLn "\10004"






    _ -> printGoodbyeMessage





processUserInputFile :: FilePath -> IO UserInputs
processUserInputFile  = (either throwIO return =<<) . runInterpreter . userInputCheck



-- | Generate a valid filename for the benchmarking file from the filename of 
-- the user input/test file by adding integer suffixes if necessary.
genBenchFilename :: String -> IO String 
genBenchFilename s = do 
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
    s'        = "Bench" ++ s