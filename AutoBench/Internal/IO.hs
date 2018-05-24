
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall   #-} 


module AutoBench.Internal.IO 
  ( 
    selOption
  , printGoodbyeMessage
  ) where

import Control.Exception          (catch)
import Control.Exception.Base     (throwIO)
import Control.Monad              (when, unless, void)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Char                  (toLower)
import Data.List                  (intersperse)
import GHC.Paths                  (libdir)
import System.Console.Haskeline   ( InputT, MonadException, getInputChar
                                  , getInputLine )
import System.Directory           ( doesFileExist, getDirectoryContents
                                  , removeFile )
import System.FilePath.Posix      (dropExtension, takeDirectory, takeExtension)
import System.IO                  (Handle)
import System.IO.Error            (isDoesNotExistError)

import System.Process 
  ( ProcessHandle
  , StdStream(..)
  , createProcess
  , getProcessExitCode
  , proc
  , std_out
  )

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C



import AutoBench.Types 
import AutoBench.Utils
import AutoBench.AbstractSyntax

import qualified Text.PrettyPrint.HughesPJ as PP


-- | Discover potential input files in the current directory.
discoverInputFiles :: IO [FilePath]
discoverInputFiles  = filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."


selOption 
  :: (MonadIO m, MonadException m) => UserInputs -> InputT m [(Id, TestSuite)]
selOption inps = case _testSuites inps of 
  []   -> do
    liftIO $ putStr "\n\n"
    liftIO (putStrLn "  No valid test suites.")
    let go = do 
               liftIO $ putStrLn ""
               liftIO $ putStrLn $ unlines [ "  * View parse results [P]" 
                                           , "  * Exit               [E]" ]
               fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case 
                 Nothing  -> return []
                 Just "e" -> return [] 
                 Just "p" -> liftIO (putStrLn "\n" >> showUserInputs >> putStrLn "\n") >> go
                 Just _   -> inpErr >> go
    go
  [ts] -> return [ts]
  tss  -> do 
    liftIO $ putStr "\n\n"
    liftIO (putStrLn "  Multiple valid test suites:")
    liftIO (showTestSuites $ _testSuites inps)
    let go = do 
               liftIO $ putStrLn ""
               liftIO $ putStrLn $ unlines [ "  * Run a test suite   [1" ++ endRange
                                           , "  * View test suites   [V]"
                                           , "  * View parse results [P]" 
                                           , "  * Exit               [E]" ]
               fmap (fmap toLower . strip) <$> getInputLine "> " >>= \case 
                 Nothing  -> return []
                 Just "e" -> return [] 
                 Just "p" -> liftIO (putStrLn "\n" >> showUserInputs >> putStrLn "\n") >> go
                 Just "v" -> liftIO (showTestSuites $ _testSuites inps) >> go
                 Just inp -> case reads inp :: [(Int, String)] of 
                   []         -> inpErr >> go
                   (n, _) : _ -> if n >= 1 && n <= l
                                 then return [_testSuites inps !! (n - 1)]
                                 else inpErr >> go
    go
 
  where 
    l        = length (_testSuites inps)
    endRange = if l > 1
               then ".." ++ show (l :: Int) ++ "]"
               else "]"
    inpErr   = liftIO $ putStrLn "\n Error: invalid choice.\n"

    showTestSuites tss = do 
      putStrLn ""
      print $ PP.nest 4 $ PP.vcat $ (PP.punctuate (PP.text "\n") $ fmap (uncurry showTestSuite) $ zip [1..] tss)
      where
        showTestSuite :: Int -> (Id, TestSuite) -> PP.Doc
        showTestSuite idx (idt, ts) = PP.vcat 
          [ PP.text $ "" ++ show idx ++ ") " ++ idt
          , PP.nest 10 $ docTestSuite ts ]

    showUserInputs = print $ PP.nest 2 $ docUserInputs inps



printGoodbyeMessage :: IO () 
printGoodbyeMessage  = putStrLn "Leaving AutoBench."