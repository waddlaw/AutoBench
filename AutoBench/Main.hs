
{-# OPTIONS_GHC -Wall   #-} 
{-# LANGUAGE LambdaCase #-}

-- import qualified Options.Applicative as OPTS

import           Control.Exception.Base       ( SomeException, catch, finally 
                                              , fromException, throwIO )
import           Criterion.Types              (reportFile)
import           Data.List                    (nub)
import           Data.Maybe                   (catMaybes, fromMaybe)
import           Language.Haskell.Interpreter ( InterpreterError(..)
                                              , errMsg, runInterpreter )
import           System.Console.Haskeline     (defaultSettings, runInputT)
import           System.FilePath.Posix        (dropExtension)
import qualified Text.PrettyPrint.HughesPJ    as PP


import AutoBench.Internal.Analysis        (analyseWith)
import AutoBench.Internal.UserInputChecks (qCheckTestPrograms, userInputCheck)
import AutoBench.Internal.UserIO          ( selTestSuiteOption
                                          , printGoodbyeMessage )
import AutoBench.Internal.Utils           (strip, filepathToModuleName)

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
  , InputError(..)
  , SystemError(..)
  , TestSuite(..)
  , UserInputs
  , defBenchRepFilename
  )


{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - Which system files to delete?
   - Sanitise runtimes, max 0 ..
   - Handle CTRL-C?
-}


main :: IO () 
main  = flip catch catchSomeException $ do 

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
                  

                  analyseWith (_analOpts ts) testRep 


                  printGoodbyeMessage



              ) (deleteBenchmarkingFiles benchFP fp $ tempSysFiles ts)                               -- (X) Finally delete benchmarking files.  *** WHICH SYS FILES? ***



        

    _ -> printGoodbyeMessage

  where 

    catchSomeException :: SomeException -> IO ()
    catchSomeException e = do 
      putStrLn "\n"
      case catMaybes [ catchInterpreterError e
                     , catchSystemError      e 
                     , catchInputError       e] of 
        []      -> catchOtherError e >> putStrLn "Testing cancelled."
        (m : _) -> m >> putStrLn "Testing cancelled."


    catchInterpreterError :: SomeException -> Maybe (IO ())
    catchInterpreterError e = case fromException e of 
      Just (UnknownError  s) -> Just $ putStrLn s 
      Just (WontCompile  es) -> 
        Just $ putStrLn $ unlines . fmap strip . nub . map errMsg $ es
      Just (NotAllowed    s) -> Just $ putStrLn s  
      Just (GhcException  s) -> Just $ putStrLn s  
      _ -> Nothing

    catchSystemError :: SomeException -> Maybe (IO ())
    catchSystemError e = case fromException e of 
      Just (InternalErr s) -> Just $ putStrLn s 
      _ -> Nothing

    catchInputError :: SomeException -> Maybe (IO ())
    catchInputError e = case fromException e of 
      Just (FilePathErr   s) -> Just $ putStrLn s    
      Just (FileErr       s) -> Just $ putStrLn s        
      Just (TestSuiteErr  s) -> Just $ putStrLn s       
      Just (DataOptsErr   s) -> Just $ putStrLn s       
      Just (AnalOptsErr   s) -> Just $ putStrLn s       
      Just (TypeErr       s) -> Just $ putStrLn s      
      Just (InstanceErr   s) -> Just $ putStrLn s      
      Just (TestReportErr s) -> Just $ putStrLn s     
      _ -> Nothing
    
    catchOtherError :: SomeException -> IO ()
    catchOtherError e = print e












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
     -- | reportFile (_critCfg ts) == Nothing = [defBenchRepFilename]
     -- | otherwise = []

    benchRepFilename ts = fromMaybe defBenchRepFilename (reportFile $ _critCfg ts) 
