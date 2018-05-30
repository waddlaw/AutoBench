
{-# OPTIONS_GHC -Wall   #-} 
{-# LANGUAGE LambdaCase #-}

-- import qualified Options.Applicative as OPTS

import           Control.Exception.Base       (finally, throwIO)
import           Criterion.Types              (reportFile)
import           Data.Maybe                   (fromMaybe)
import           Language.Haskell.Interpreter (runInterpreter)
import           System.Console.Haskeline     (defaultSettings, runInputT)
import           System.FilePath.Posix        (dropExtension)
import qualified Text.PrettyPrint.HughesPJ    as PP

import AutoBench.Internal.Types          
  ( DataOpts(..)
  , TestSuite(..)
  , UserInputs
  , defBenchRepFilename
  , docTestReport
  )
import AutoBench.Internal.UserInputChecks (qCheckTestPrograms, userInputCheck)
import AutoBench.Internal.Utils           (filepathToModuleName)
import AutoBench.Internal.IO              
  ( compileBenchmarkingFile
  , deleteBenchmarkingFiles
  , execute
  , generateBenchmarkingFilename 
  , generateBenchmarkingFile
  , generateTestReport
  , printGoodbyeMessage
  , selTestSuiteOption 
  )

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - Which system files to delete?
   - QuickCheck semantic equality checks are missing;
-}


main :: IO () 
main = do
  
  --args <- OPTS.customExecParser (OPTS.prefs OPTS.showHelpOnError) $ clArgsParser

  let fp = "./Input.hs"
      mn = filepathToModuleName fp
  putStrLn ""



  putStr $ poorNest 2 $ "\9656 Processing input file"                                                -- (1) Process user input file.
  inps <- processUserInputFile fp
  putStrLn $ poorNest 1 "\10004"
  (runInputT defaultSettings $ selTestSuiteOption inps) >>= \case                                    -- (2) Select test suite.
    [(idt, ts)] -> do 
      putStrLn $ poorNest 2 $ "\9656 Running test suite \ESC[3m" ++ idt ++ "\ESC[0m:"      
      putStr   $ poorNest 5 $ "\8226 QuickChecking test programs"
      eql <- qCheck fp ts inps                                                                       -- (3) Check whether test programs are semantically
      if eql                                                                                         --     equal using QuickCheck, if applicable.
      then putStrLn $ poorNest 1 "\10004"
      else putStrLn $ poorNest 1 "\10007"                                                            
      putStr   $ poorNest 5 $ "\8226 Generating benchmarking file"  
      benchFP <- generateBenchmarkingFilename fp                                                     -- (4) Generate benchmarking file.
      finally (do generateBenchmarkingFile benchFP mn inps idt ts                        
                  putStrLn $ poorNest 1 "\10004"
                  putStrLn $ poorNest 5 "\8226 Compiling benchmarking file..."                       -- (5) Compile benchmarking file.
                  invalidFlags <- compileBenchmarkingFile benchFP fp (_ghcFlags ts) 
                  printInvalidFlags invalidFlags
                  putStrLn $ poorNest 5 "\8226 Executing benchmarking file..."                       -- (6) Execute benchmarking file.
                  putStrLn ""
                  execute (dropExtension benchFP)
                  putStrLn $ poorNest 5 "\8226 Executed benchmarking file \10004"                    
                  putStr $ poorNest 5 "\8226 Generating test report"                                 -- (7) Generate test report.
                  testRep <- generateTestReport mn ts (benchRepFilename ts) eql
                  putStrLn $ poorNest 1 "\10004"
                  print $ docTestReport testRep



              ) (deleteBenchmarkingFiles benchFP fp $ tempSysFiles ts)                               -- (X) Finally delete benchmarking files.  *** WHICH SYS FILES? ***



        

    _ -> printGoodbyeMessage

  where 
    -- Runner for the 'hint' monad but throw any errors in IO.
    processUserInputFile :: FilePath -> IO UserInputs
    processUserInputFile  = 
      (either throwIO return =<<) . runInterpreter . userInputCheck

    qCheck :: FilePath -> TestSuite -> UserInputs -> IO Bool 
    qCheck fp ts inps = case (_dataOpts ts) of 
      Gen{} -> (either throwIO return =<<) . runInterpreter $ 
        qCheckTestPrograms fp (_progs ts) inps
      -- Can't check test programs using manual test data.
      Manual{} -> return False


    printInvalidFlags :: [String] -> IO () 
    printInvalidFlags [] = do
      putStrLn ""
      putStrLn $ poorNest 5 "\8226 Compiled benchmarking file \10004"
    printInvalidFlags xs = do 
      putStrLn ""
      putStrLn $ poorNest 5 "\8226 Compiled benchmarking file \63"
      putStr $ poorNest 9 $ "Warning, invalid compiler flags:"
      putStrLn $ poorNest 1 $ PP.render $ PP.hcat $ PP.punctuate (PP.text ", ") xs'
      
      where xs' = fmap (\x -> PP.text $ "\ESC[3m" ++ x ++ "\ESC[0m") xs

    -- Poor pretty printing.
    poorNest :: Int -> String -> String 
    poorNest n = (replicate n ' ' ++)
      
    -- Temporary system files to delete after benchmarking.
    tempSysFiles ts = [] 
     --reportFile (_critCfg ts) == Nothing = [defBenchRepFilename] ******** TO DO !!
     -- | otherwise = []

    benchRepFilename ts = fromMaybe defBenchRepFilename (reportFile $ _critCfg ts) 
