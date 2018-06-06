
{-# OPTIONS_GHC -Wall   #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

{-|

  Module      : AutoBench.Internal.Hint
  Description : Dynamically interpreting user input files.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module is responsible for dynamically interpreting user input files,
  for example:

  * Loading files and setting top level modules;
  * Loading AutoBench helper modules used for dynamic checking;
  * Extracting definitions and typing information for user input files.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   -
-}

module AutoBench.Internal.Hint
  (
    loadFileSetTopLevelModule               -- Load a file and set the top level module.
  , loadFileSetTopLevelModuleWithHelpers    -- Load a file and set the top level module; also load AutoBench helper modules.
  , extractElemsAndTypes                    -- From a previously loaded file, extract all definitions and their corresponding types if appropriate.

  ) where

import Control.Monad.Catch (throwM)
import Data.List           (intersect)
import Language.Haskell.Interpreter
  ( MonadInterpreter
  , getLoadedModules
  , getModuleExports
  , loadModules
  , setImportsQ
  , setTopLevelModules
  , typeOf
  )

import AutoBench.Internal.AbstractSyntax ( Id, ModuleElem(..), ModuleName
                                         , TypeString, prettyPrint, qualIdt )
import AutoBench.Internal.Types          (InputError(..), SystemError(..))
import AutoBench.Internal.Utils          (filepathToModuleName)

-- | Load a file and set the top level module. The module name is calculated
-- from the filename by simply dropping the \'.hs\' extension. For example, if
-- the given file is named \'Input.hs\', then module name is assumed to be
-- \'Input\'.
--
-- If the file doesn't have a module name that matches the filename, an
-- 'InputError' is thrown.
--
-- N.B. the system requires that module names match filenames.
loadFileSetTopLevelModule :: MonadInterpreter m => FilePath -> m ()
loadFileSetTopLevelModule fp = do
  let mn = filepathToModuleName fp  -- Drop the .hs extension.
  loadModules [fp]
  mods <- getLoadedModules          -- Make sure module is successfully loaded.
  if mn `elem` mods
    then setTopLevelModules [mn]
    else throwM (FileErr "Invalid module name.")

-- | Similar to 'loadFileSetTopLevelModule' but loads additional helper
-- modules. In practice, this is used for dynamically checking user input files:
-- see AutoBench.Internal.UserInputChecks.
--
-- For AutoBench purposes:
-- 'hint' requires that all modules are loaded at the same, so the system cannot
-- simply load additional modules when they are required (e.g., for the dynamic
-- checking phase). Instead, all modules have to be (re-)loaded, including the
-- user input file.
loadFileSetTopLevelModuleWithHelpers
  :: MonadInterpreter m
  => FilePath
  -> [ModuleName]  -- Helper modules.
  -> m ()
loadFileSetTopLevelModuleWithHelpers fp helpers = do
  let mn = filepathToModuleName fp
  loadModules [fp]
  mods <- getLoadedModules
  if | mn `notElem` mods ->                                             -- Firstly check the user input file is loaded.
         throwM (FileErr "Invalid module name.")
 --    | length (mods `intersect` helpers) /= length helpers ->           -- Then check all the AutoBench helper modules are loaded.
 --        throwM (InternalErr $ "loadFileSetTopLevelModuleWithHelpers: failed to load one or more helper modules: " ++ show helpers)
     | otherwise -> do 
         setTopLevelModules [mn] --(mn : helpers)
         setImportsQ $ fmap (\fp -> (fp, Just fp)) helpers

-- | From a previously loaded file, extract all the definitions and their
-- corresponding types, if appropriate.
extractElemsAndTypes
  :: MonadInterpreter m
  => ModuleName
  -> m [(ModuleElem, Maybe TypeString)]
extractElemsAndTypes mn = do
  defs <- getModuleExports mn     -- Get the module definitions.
  tys  <- mapM (\case             -- Can only get typing information from 'Fun's.
    Nothing  -> return Nothing    -- Qualify identifier with module name.
    Just idt -> Just <$> (typeOf $ prettyPrint $ qualIdt mn idt))
      (fmap funIdt defs)
  return (zip defs tys)
  where
    -- Extract the identifiers from 'Fun's.
    funIdt :: ModuleElem -> Maybe Id
    funIdt (Fun idt) = Just idt
    funIdt _         = Nothing
