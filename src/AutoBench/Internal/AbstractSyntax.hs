
{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--
-- Module      : AutoBench.Internal.AbstractSyntax
-- Description : Abstract representation of test inputs
-- Copyright   : (c) 2018 Martin Handley
-- License     : BSD-style
-- Maintainer  : martin.handley@nottingham.ac.uk
-- Stability   : Experimental
-- Portability : GHC
--
-- In order to statically and dynamically validate and classify test inputs,
-- i.e., test programs, test data, and test suites, the system uses an abstract 
-- representation of Haskell 98. This module provides simple functions to 
-- manipulate this abstract representation.
--

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------

module AutoBench.Internal.AbstractSyntax
  (

  -- * Abstract syntax
    TypeString            -- String representation of a type.
  -- ** Type constructor definitions
  , unaryTestDataTyCon    -- Abstract type constructor for the 'UnaryTestData' datatype.
  , binaryTestDataTyCon   -- Abstract type constructor for the 'BinaryTestData' datatype.
  , testSuiteTyCon        -- Abstract type constructor for the 'TestSuite' datatype.
  -- * Helpers
  , hasTyVars             -- Check whether a 'HsType' contains type variables.
  , tyFunInps             -- Extract the input types from unary/binary function types.
  , unqualTyToTy          -- Convert an unqualified 'HsQualType' to a 'HsType'.
  , qualIdt               -- Add a qualifying module name to an identifier.
  , unqualIdt             -- Remove a qualifying module names from an identifier.
  -- * Re-exports
  , Id                    -- Al from 'Language.Haskell'
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

import qualified AutoBench.Internal.Configuration as Config

-- * Abstract syntax

-- | The string representation of a type.
type TypeString = String 

-- | In order to group definitions by their types, we compare their string 
-- representations. It is a naive approach but seems to work fine in practice.
instance Ord HsType where 
  compare t1 t2 = compare (prettyPrint t1) (prettyPrint t2)

-- ** Type constructor definitions

-- | Abstract type constructor for the 'UnaryTestData' datatype.
unaryTestDataTyCon :: HsType 
unaryTestDataTyCon  = HsTyCon $ UnQual $ HsIdent Config.unaryTestDataConstructor          

-- | Abstract type constructor for the 'BinaryTestData' datatype.
binaryTestDataTyCon :: HsType 
binaryTestDataTyCon  = HsTyCon $ UnQual $ HsIdent Config.binaryTestDataConstructor

-- | Abstract type constructor for the 'TestSuite' datatype.
testSuiteTyCon :: HsType 
testSuiteTyCon  = HsTyCon $ UnQual $ HsIdent Config.testSuiteDataConstructor

-- * Helpers

-- | Check whether a 'HsType' contains type variables.
hasTyVars :: HsType -> Bool 
hasTyVars HsTyVar{} = True
hasTyVars HsTyCon{} = False
hasTyVars (HsTyTuple ts)  = any hasTyVars ts
hasTyVars (HsTyFun t1 t2) = hasTyVars t1 || hasTyVars t2
hasTyVars (HsTyApp t1 t2) = hasTyVars t1 || hasTyVars t2

-- | Convert an unqualified 'HsQualType' to a 'HsType' by removing its context.
-- Warning: assumes the context is empty.
unqualTyToTy :: HsQualType -> HsType
unqualTyToTy (HsQualType [] ty) = ty 
unqualTyToTy _ = error "AutoBench.Internal.AbstractSyntax.unqualTyToTy: HsQualType has a non-empty context."

-- | Extract the input types from a /unary/ or /binary/ function type. Return 
-- them as a 'HsTyTuple'.
--
-- Examples in pseudocode:
-- 
-- * Int -> Int               ===> (Int)              -- OK:    unary
-- * Int -> String -> Int     ===> (Int, String)      -- OK:    binary
-- * Int                      ===> ()                 -- EMPTY: nullary
-- * Int -> Int -> Int -> Int ===> ()                 -- EMPTY: ternary
tyFunInps:: HsType -> HsType
tyFunInps (HsTyFun t1 (HsTyFun t2 _)) = HsTyTuple [t1, t2] 
tyFunInps (HsTyFun t _) = HsTyTuple [t]
tyFunInps _ = HsTyTuple []

-- | Add a qualifying module name to an identifier.
--
-- > quallIdt "Foo" "bar" = "Foo.bar"
qualIdt :: ModuleName -> Id -> HsExp
qualIdt mn idt = HsVar (Qual (Module mn) (HsIdent idt))

-- | Remove a qualifying module names from an identifier.
--
-- > unqualIdt "Foo.bar"     = bar
-- > unqualIdt "Foo.Bar.bar" = bar
-- > unqualIdt "foo"         = foo
unqualIdt :: Id -> Id 
unqualIdt  = reverse . takeWhile (/= '.') . reverse