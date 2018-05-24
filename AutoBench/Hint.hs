
{-# OPTIONS_GHC -Wall   #-} 
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}


{-|

  Module      : AutoBench.Hint
  Description : Dynamically interpreting user input files.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module is responsible for dynamically interpreting user input files.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.Hint
  (
    loadFileSetTopLevelModule               -- Load a file and set the top level module.
  , loadFileSetTopLevelModuleWithHelpers    -- Load a file and set the top level module; also load helper modules.
  , extractElemsAndTypes                    -- From a previously loaded file, extract all definitions and their corresponding types if appropriate.

  ) where

import Control.Monad.Catch (throwM)
import Data.List           (intersect)
import Language.Haskell.Interpreter 
  ( MonadInterpreter
  , getLoadedModules
  , getModuleExports
  , loadModules
  , setTopLevelModules
  , typeOf
  )

import AutoBench.AbstractSyntax (Id, ModuleElem(..), ModuleName, TypeString)
import AutoBench.Types          (InputError(..), SystemError(..))
import AutoBench.Utils          (filepathToModuleName)

-- | Load a file and set the top level module. The module name is calculated
-- from the file name by simply dropping the \'.hs\' extension. For example, if
-- the given file is named \'Input.hs\', then module name is assumed to be
-- \'Input\'.
-- 
-- If the file doesn't have a module name that matches the file name, an
-- 'InputError' is thrown.
loadFileSetTopLevelModule :: MonadInterpreter m => FilePath -> m ()
loadFileSetTopLevelModule fp = do
  let mn = filepathToModuleName fp
  loadModules [fp]
  mods <- getLoadedModules
  if mn `elem` mods
  then setTopLevelModules [mn]        
  else throwM (FileErr "Invalid module name.")

-- | Similar to 'loadFileSetTopLevelModule' but loads additional helper 
-- modules. In practice, this is used for user input checking, 
-- see AutoBench.UserInputChecks.
loadFileSetTopLevelModuleWithHelpers 
  :: MonadInterpreter m 
  => FilePath 
  -> [ModuleName] 
  -> m ()
loadFileSetTopLevelModuleWithHelpers fp helpers = do
  let mn = filepathToModuleName fp
  loadModules (fp : helpers)
  mods <- getLoadedModules
  if | mn `notElem` mods -> 
         throwM (FileErr "Invalid module name.")
     | length (mods `intersect` helpers) /= length helpers -> 
         throwM (InternalErr $ "loadFileSetTopLevelModuleWithHelpers: failed to load one or more helper modules: " ++ show helpers)
     | otherwise -> setTopLevelModules (mn : helpers)        

-- | From a previously loaded file, extract all the definitions and their 
-- corresponding types, if appropriate.
extractElemsAndTypes
  :: MonadInterpreter m 
  => ModuleName 
  -> m [(ModuleElem, Maybe TypeString)] 
extractElemsAndTypes mn = do
  defs <- getModuleExports mn
  tys  <- mapM (\case
    Nothing  -> return Nothing 
    Just idt -> Just <$> typeOf idt) (fmap funIdt defs)
  return (zip defs tys)
  where 
    -- Extract the identifiers from 'Fun's.
    funIdt :: ModuleElem -> Maybe Id
    funIdt (Fun idt) = Just idt 
    funIdt _         = Nothing