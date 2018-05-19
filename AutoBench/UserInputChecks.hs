
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.UserInputChecks
  Description : Interpreting, validating, and classifying user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  User inputs are always passed to the system using files (Haskell modules), 
  which must be interpreted and validated. Test suites ('TestSuites') are 
  AutoBench's principle user input datatype, and are used to structure 
  performance tests into logical units. Thus, input files will typically 
  contain one or more test suites. In addition, input files will contain test 
  programs (those whose performance is to be analysed/compared) and possibly 
  user-specified test data.

  As user inputs have a number of different purposes, but are all specified
  in the same way (i.e., inside files), the system must not only validate 
  user inputs appropriately, but it must also classify them according to their
  purpose: test suites, test programs, test data, etc.

  The classification of user inputs is a non-trivial process, due to the fact 
  that, for example, any program to be tested using the system must satisfy the 
  following properties,

  * Be a unary or binary function;
  * Have an input type that is a member of the 'NFData' type class;
  * Have an input type that is a member of the 'Arbitrary' type class 
     OR
    Be associated with valid user-specified test data;
  * Be referred to by a valid test suite;

  which in themselves rely on the classification of \'unary functions\', 
  \'valid test data\', \'valid test suites\', \'functions whose input types are 
  members of the 'NFData' type class\', etc.
  
  Two different classification procedures are required. For example, a type 
  signature can be used to determine whether a function is unary, binary, or 
  otherwise. However, to the best of my knowledge, there is no generic way of 
  checking whether a function's input type is a member of the 'NFData' type 
  class aside from performing a dynamic test, because it depends on which 
  type class instances are in scope. Therefore, user inputs can be classified 
  according the property \'is a unary function\' /statically/, but only 
  /dynamic/ checks can be used to classify functions according to the property 
  \'has input types that are members of the 'NFData' type class\'.
  
  As such, the system performs static ('AutoBench.StaticChecks') and dynamic 
  validation and classification ('AutoBench.DynamicChecks') of user inputs.

  This module is responsible for coordinating the overall checking process.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.UserInputChecks (check) where 

import Control.Category             ((>>>))
import Control.Monad                ((>=>))
import Language.Haskell.Interpreter (MonadInterpreter)

import AutoBench.AbstractSyntax
  ( HsType
  , Id
  , ModuleElem(..)
  , TypeString
  , unqualTyToTy
  )
import AutoBench.DynamicChecks
  ( catArbitrary
  , catNFDataInput
  , catNFDataResult
  , checkFullTestSuites
  , checkValidTestData
  , interpTestSuites
  )
import AutoBench.StaticChecks 
  ( isABGenTyFun
  , isABTyFun
  , isBinaryTestData
  , isBinaryTyFun
  , isNullaryTyFun
  , isUnaryTestData
  , isUnaryTyFun
  , isUnqualQualTy
  , parseTySig
  )
import AutoBench.Hint  
  ( extractElemsAndTypes
  , loadFileSetTopLevelModule
  , loadFileSetTopLevelModuleWithHelpers
  )
import AutoBench.Types (UserInputs(..), initUserInputs)
import AutoBench.Utils (filepathToModuleName)

-- makeLenses ''UserInputs

-- * Top-level 

check :: MonadInterpreter m => FilePath -> m UserInputs 
check fp  = do 
  let mn = filepathToModuleName fp
  -- Load the user input file.
  loadFileSetTopLevelModule fp
  -- Extract user inputs.
  inps <- extractUserInputs fp
  -- First static checks/categorising.
  let fstStInps = firstStatic inps
  loadFileSetTopLevelModuleWithHelpers fp ["AutoBench.DynamicInstanceChecks"]
  -- First dynamic checks/categorising.
  fstDynInps <- firstDynamic mn fstStInps
  -- Second static checks.
  let sndStInps = secondStatic fstDynInps
  

  return sndStInps
  where 
    -- First phase of static checking.
    firstStatic = 
      catValidInvalidElems                                                       -- 1. /Typeable/, 2. /Unqualified/, 3. /Function/.
        >>> catArityFuns                                                         -- 4. /NullaryFun/, 5. /UnaryFun/, 6. /BinaryFun/.
        >>> catGenableFuns                                                       -- 7. /Genable/.
        >>> catTestData                                                          -- 8. /UnaryData/, 9. /BinaryData/.

    -- Second phase of static checking.
    secondStatic = checkValidTestSuites
                   
    -- First phase of dynamic checking.
    firstDynamic mn = 
      catNFDataInput            mn                                               -- 1. /NFDataInput/.
        >=> catNFDataResult     mn                                               -- 2. /NFDataResult/.
        >=> catArbitrary        mn                                               -- 3. /Arbitrary/.
        >=> interpTestSuites    mn                                               -- 4. /TestSuites/.
        >=> checkFullTestSuites mn                                               -- 5. /FullTestSuites/.
        >=> checkValidTestData  mn                                               -- 6. /ValidUnaryData/, 7. /ValidBinaryData/.

-- * Static checking

-- ** First phase 

-- | Categorise elements in the 'UserInputs' '_allElems' list according to 
-- whether they have types that are 1. /Typeable/, 2. /Unqualified/, and 3.
-- /Function/, in which case they are valid and put into the '_validElems' list
-- of 'UserInputs'. If not then they are '_invalidElems'.
catValidInvalidElems :: UserInputs -> UserInputs
catValidInvalidElems inps = 
  inps { _invalidElems = invalids, _validElems = valids }
  where 
    (valids, invalids) = foldr cat ([], []) (_allElems inps)
    
    cat 
      :: (ModuleElem, Maybe TypeString) 
      -> ([(Id, HsType)], [(ModuleElem, Maybe TypeString)])
      -> ([(Id, HsType)], [(ModuleElem, Maybe TypeString)])
    cat x@(Fun idt, Just tyStr) (vs, ivs) = 
      case parseTySig (idt ++ " :: " ++ tyStr) of 
        -- 1. /Typeable/, 2. /Unqualified/, 3. /Function/ => add to @valids@.
        Just ty | isUnqualQualTy ty && isABTyFun (unqualTyToTy ty) -> 
          ((idt, unqualTyToTy ty) : vs, ivs)
        -- Otherwise add to @invalids@.
        _ -> (vs, x : ivs)
    -- Anything that isn't a 'Fun' => add to @invalids@.
    cat iv (vs, ivs) = (vs, iv : ivs)

-- | Categorise elements in the '_validElems' list according to their arity,
-- i.e., 4. /NullaryFun/, 5. /UnaryFun/, or 6. /BinaryFun/.
--
-- Nullary functions are added to the '_nullaryFuns' list, unary functions to 
-- the '_unaryFuns' list and binary functions to the '_binaryFuns' list.
catArityFuns :: UserInputs -> UserInputs
catArityFuns inps = 
  inps { _nullaryFuns = nuls, _unaryFuns = uns, _binaryFuns = bins }
  where
    (nuls, uns, bins) = foldr cat ([], [], []) (_validElems inps) 

    cat 
      :: (Id, HsType)
      -> ([(Id, HsType)], [(Id, HsType)], [(Id, HsType)])
      -> ([(Id, HsType)], [(Id, HsType)], [(Id, HsType)])
    cat x@(_, ty) (ns, us, bs) 
      | isNullaryTyFun ty = (x : ns, us, bs)
      | isUnaryTyFun   ty = (ns, x : us, bs)
      | isBinaryTyFun  ty = (ns, us, x : bs)
      | otherwise         = (ns, us, bs) 

-- | Categorise elements in the '_validElems' list according to whether they 
-- are 7. /Genable/ (unary/binary functions that do not contain type variables 
-- in their input types). 
--
-- /Genable/ functions are added to the '_arbFuns' list for subsequent dynamic 
-- checking.
catGenableFuns :: UserInputs -> UserInputs
catGenableFuns inps = 
  inps { _arbFuns = filter (isABGenTyFun . snd) (_validElems inps) }

-- | Categorise elements in the '_nullaryFuns' list according to whether they 
-- are 8. /UnaryData/ or 9. /BinaryData/.
--
-- /UnaryData/ is added to the '_unaryData' list. /BinaryData/ is added to the 
-- '_binaryDat' list.
catTestData :: UserInputs -> UserInputs
catTestData inps = inps { _unaryData = uns, _binaryData = bins }
  where
    (uns, bins) = foldr cat ([], []) (_nullaryFuns inps) 

    cat 
      :: (Id, HsType)
      -> ([(Id, HsType)], [(Id, HsType)])
      -> ([(Id, HsType)], [(Id, HsType)])
    cat x@(_, ty) (us, bs) 
      | isUnaryTestData  ty = (x : us, bs)
      | isBinaryTestData ty = (us, x : bs)
      | otherwise           = (us, bs)


-- ** Second phase

checkValidTestSuites :: UserInputs -> UserInputs
checkValidTestSuites inps = inps


-- * Dynamic checking





-- * Helpers 

-- | Extract all the definitions in a user input file and initialise the 
-- 'UserInputs' data structure.
extractUserInputs :: MonadInterpreter m => FilePath -> m UserInputs 
extractUserInputs fp = 
  initUserInputs <$> extractElemsAndTypes (filepathToModuleName fp)