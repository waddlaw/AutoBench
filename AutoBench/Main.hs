
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


generateBenchmarks :: FilePath -> ModuleName -> UserInputs -> Id -> TestSuite -> IO ()
generateBenchmarks fp mn inps idt ts = writeFile fp (PP.render contents)
  where

    contents = PP.vcat 
      [ PP.text "" 
      , PP.text "module Main (main) where"
      , PP.text ""
      , PP.text "import AutoBench.Internal.Benchmarking"
      , PP.text "import" PP.<+> PP.text mn
      , PP.text ""
      , PP.text "main :: IO ()"
      , PP.text "main  = AutoBench.Internal.Benchmarking.runBenchmarks" 
          PP.<+> PP.char '(' PP.<> genFunc gen nf unary PP.<>  PP.char ')'
          PP.<+> PP.text idt
      ]

    ---------------------------------------------------------------------------

    -- genFunc gen? nf? unary?
    genFunc :: Bool -> Bool -> Bool -> PP.Doc
    -- genBenchmarksGenNfUn:    
    -- Generated test data, results to nf, unary test programs.
    genFunc True True True = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksGenNfUn" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      ]                                              
    -- genBenchmarksGenWhnfUn:    
    -- Generated test data, results to whnf, unary test programs.
    genFunc True False True = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksGenWhnfUn" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      ]            
     -- genBenchmarksGenNfBin:
     -- Generated test data, results to nf, binary test programs.                                     
    genFunc True  True  False = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksGenNfBin" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      ] 
    -- genBenchmarksGenWhnfBin:
    -- Generated test data, results to whnf, binary test programs.                                                                
    genFunc True  False False = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksGenWhnfBin" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      ]          
    -- genBenchmarksManNfUn:
    -- User-specified test data, results to nf, unary test programs.                                      
    genFunc False True  True  = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksManNfUn" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      , PP.text (getManualDat $ _dataOpts ts)
      ]              
    -- genBenchmarksManWhnfUn:
    -- User-specified test data, results to whnf, unary test programs.                                          
    genFunc False False True  = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksManWhnfUn" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      , PP.text (getManualDat $ _dataOpts ts)
      ] 
    -- genBenchmarksManNfBin:
    -- User-specified test data, results to nf, binary test programs.
    genFunc False True False  = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksManNfBin" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      , PP.text (getManualDat $ _dataOpts ts)
      ]                
    -- genBenchmarksManWhnfBin:
    -- User-specified test data, results to whnf, binary test programs.                                 
    genFunc False False False = PP.hsep 
      [ PP.text "AutoBench.Internal.Benchmarking.genBenchmarksManWhnfBin" 
      , ppList $ fmap ppTuple qualProgs
      , PP.text idt
      , PP.text (getManualDat $ _dataOpts ts)
      ]                                                  

    ppTuple :: Id -> PP.Doc
    ppTuple idt = PP.char '('
      PP.<> PP.text (show idt)
      PP.<> PP.text ", "
      PP.<> PP.text idt
      PP.<> PP.char ')'

    ppList :: [PP.Doc] -> PP.Doc
    ppList docs = PP.hcat $ 
      PP.char '[' : (PP.punctuate (PP.text ", ") docs) ++ [PP.char ']']

    -- Helpers 

    prog  = head (_progs ts)
    unary = prog `elem` fmap fst (_unaryFuns inps)
    nf    = _nf ts 
    gen   = case _dataOpts ts of 
      Manual{} -> False 
      Gen{}    -> True
    qualProgs = fmap (prettyPrint . qualIdt mn) (_progs ts)
    
    getManualDat (Manual s) = s 
    getManualDat Gen{}      = "" --                           **  ERROR HANDLING?! **



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