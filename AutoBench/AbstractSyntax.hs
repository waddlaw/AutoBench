
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.AbstractSyntax
  Description : Abstract syntactic representation of user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  In order to statically validate and classify user inputs, the system uses 
  an abstract representation of Haskell 98. This module provides simple 
  functions to manipulate this representation.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   -
-}

module AutoBench.AbstractSyntax
  (

  -- * Abstract syntax
    TypeString            -- String representation of a type.
  -- ** Type constructor definitions
  , unaryTestDataTyCon    -- Abstract type constructor for the 'UnaryTestData' datatype.
  , binaryTestDataTyCon   -- Abstract type constructor for the 'BinaryTestData' datatype.
  , testSuiteTyCon        -- Abstract type constructor for the 'TestSuite' datatype.
  -- ** Helpers
  , hasTyVars             -- Check whether a 'HsType' contains type variables.
  , tyFunInps             -- Extract the input types from unary/binary function types.
  , unqualTyToTy          -- Convert an unqualified 'HsQualType' to a 'HsType'.
  , qualIdt               -- Add a qualifying module name to an identifier.
  -- * Re-exports
  , Id
  , HsDecl(..)
  , HsExp(..)
  , HsModule(..)
  , HsName(..)
  , HsQName(..)
  , HsQualType(..)
  , HsType(..)
  , Module(..)
  , ModuleElem(..)
  , ModuleName
  , prettyPrint

  ) where 

import Language.Haskell.Interpreter (Id, ModuleName, ModuleElem(..))
import Language.Haskell.Pretty      (prettyPrint)
import Language.Haskell.Syntax 
  ( HsDecl(..)
  , HsExp(..)
  , HsName(..)
  , HsModule(..)
  , HsQName(..)
  , HsQualType(..)
  , HsType(..)
  , Module(..)
  )

-- * Abstract syntax

-- | The string representation of a type.
type TypeString = String 

-- ** Type constructor definitions

-- | Abstract type constructor for the 'UnaryTestData' datatype.
unaryTestDataTyCon :: HsType 
unaryTestDataTyCon  = HsTyCon (UnQual (HsIdent "UnaryTestData"))

-- | Abstract type constructor for the 'BinaryTestData' datatype.
binaryTestDataTyCon :: HsType 
binaryTestDataTyCon  = HsTyCon (UnQual (HsIdent "BinaryTestData"))

-- | Abstract type constructor for the 'TestSuite' datatype.
testSuiteTyCon :: HsType 
testSuiteTyCon  = HsTyCon (UnQual (HsIdent "TestSuite"))

-- ** Helpers

-- | Check whether a 'HsType' contains one or more type variables.
hasTyVars :: HsType -> Bool 
hasTyVars HsTyVar{} = True
hasTyVars HsTyCon{} = False
hasTyVars (HsTyTuple ts)  = any hasTyVars ts
hasTyVars (HsTyFun t1 t2) = hasTyVars t1 || hasTyVars t2
hasTyVars (HsTyApp t1 t2) = hasTyVars t1 || hasTyVars t2

-- | Convert an unqualified 'HsQualType' to a 'HsType' by removing its context.
--
-- Warning: assumes the context is empty.
unqualTyToTy :: HsQualType -> HsType
unqualTyToTy (HsQualType _ ty) = ty 

-- | Extract the input types from a /unary/ or /binary/ function type. Return 
-- them as a 'HsTyTuple'.
--
-- Examples in pseudocode:
-- 
-- * Int -> Int               ===> (Int)              -- unary
-- * Int -> String -> Int     ===> (Int, String)      -- binary
-- * Int -> Int -> Int -> Int ===> ()                 -- not unary/binary 
-- * Int                      ===> ()                 -- not unary/binary 
tyFunInps:: HsType -> HsType
tyFunInps (HsTyFun t1 (HsTyFun t2 _)) = HsTyTuple [t1, t2] 
tyFunInps (HsTyFun t _) = HsTyTuple [t]
tyFunInps _ = HsTyTuple []

-- | Add a qualifying module name to an identifier.
qualIdt :: ModuleName -> Id -> HsExp
qualIdt mn idt = HsVar (Qual (Module mn) (HsIdent idt))