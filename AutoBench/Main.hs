
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

import System.Console.Haskeline hiding (throwIO)


runAndHandle :: Interpreter a -> IO a
runAndHandle  = (either throwIO return =<<) . runInterpreter


main :: IO () 
main = do
  --args <- OPTS.customExecParser (OPTS.prefs OPTS.showHelpOnError) $ clArgsParser
  putStr "\n  \9656 Processing input file "
  inps <- processUserInputFile "./Input.hs"
  putStrLn "\10004"
  (runInputT defaultSettings $ selOption inps) >>= \case 
    [(idt, ts)] -> do 
      putStrLn $ "  \9656 Running test suite '" ++ idt ++ "'"




    _ -> printGoodbyeMessage








processUserInputFile :: FilePath -> IO UserInputs
processUserInputFile  = 
    (either throwIO return =<<) . runInterpreter . userInputCheck


