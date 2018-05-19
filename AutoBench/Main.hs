



import Language.Haskell.Interpreter
import Control.Exception.Base

import AutoBench.Hint 
import AutoBench.UserInputChecks
import AutoBench.AbstractSyntax
import AutoBench.Types

runAndHandle :: Interpreter a -> IO a
runAndHandle  = (either throwIO return =<<) . runInterpreter


main :: IO () 
main = do 
  inps <- runAndHandle $ check "./Input.hs"
  error $ show inps




instance Show UserInputs where 
  show usrInps = unlines 
    [
      "allElems ="
    , unlines $ indent 2 $ fmap show $ _allElems usrInps
    , "invalidElems ="
    , unlines $ indent 2 $ fmap show $ _invalidElems usrInps
    , "validElems ="
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _validElems usrInps 
    , "nullaryFuns =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _nullaryFuns usrInps
    , "unaryFuns =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _unaryFuns usrInps
    , "binaryFuns =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _binaryFuns usrInps
    , "benchFuns =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _benchFuns usrInps
    , "arbFuns =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _arbFuns usrInps
    , "nfFuns =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _nfFuns usrInps
    , "invalidData ="
    , unlines $ indent 2 $ fmap (\(idt, ty, errs) -> idt ++ " :: " ++ show (prettyPrint ty) ++ "\n" ++ (unlines $ indent 4 $ fmap show errs)) $ _invalidData usrInps
    , "unaryData =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _unaryData usrInps
    , "binaryData =" 
    , unlines $ indent 2 $ fmap (\(idt, ty) -> idt ++ " :: " ++ show (prettyPrint ty)) $ _binaryData usrInps
    , "invalidTestSuites ="
    , unlines $ indent 2 $ fmap (\(idt, errs) -> idt ++ "\n" ++ (unlines $ indent 4 $ fmap show errs)) $ _invalidTestSuites usrInps
    , "testSuites ="
    , unlines $ indent 2 $ fmap fst $ _testSuites usrInps
    ]

indent :: Int -> [String] -> [String]
indent n = zipWith (++) (repeat $ replicate n ' ')


