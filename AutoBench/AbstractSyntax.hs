
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.AbstractSyntax
  Description : Abstract syntax used to represent user inputs for /syntactic/ 
                validation purposes.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  In order to validate user input files, the system uses an abstract 
  representation of Haskell 98. A number of syntactic checks are performed on 
  the abstract representations of types to assess whether the types of 
  user-specified test program's meet various requirements of the system: 

  * Unqualified: the types of test inputs (i.e., test programs and test data) 
    /must/ be unqualified;
  * Testable: the system can test unary and binary functions only.
  * Genable: unary/binary functions can only be tested on randomly generated 
    inputs if their input types /do not/ contain type variables.

  A subsequent verification phase then determines whether user-specified test 
  programs satisfy the required type class constraints for data generation
  (i.e., Arbitrary) and/or benchmarking (i.e., NFData). The checks that are 
  performed during this subsequent phase are not syntactic, however.

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

    parseTySig          -- Parse a string representation of a type signature to an abstract qualified type representation.
  , tyFunInps           -- Extract the input types from unary/binary function types.
  , unqualTyToTy        -- Convert an unqualified 'HsQualType' to a 'HsType'.
  , qualIdt             -- Add a qualifying module name to an identifier.

   -- * Syntactic checks

  , hasTyVars           -- Check whether a 'HsType' contains type variables.
  , isNullaryTyFun      -- Is a 'HsType' a nullary function type?
  , isUnaryTyFun        -- Is a 'HsType' a unary function type? 
  , isBinaryTyFun       -- Is a 'HsType' a binary function type?
  , isUnqualQualTy      -- Does a 'HsQualType' meet the /unqualified/ syntactic type requirements of AutoBench?
  , isABTyFun           -- Does a 'HsType' meet the syntactic type requirements of AutoBench?
  , isABTestTyFun       -- Does a 'HsType' meet the /testable/ syntactic type requirements of AutoBench?
  , isABGenTyFun        -- Does a 'HsType' meet the /genable/ syntactic type requirements of AutoBench?
    
    -- * Re-exports

  , Id
  , HsExp(..)
  , HsName(..)
  , HsQName(..)
  , HsQualType(..)
  , HsType(..)
  , Module(..)
  , ModuleName
  , prettyPrint

  ) where 

-- Abstract syntax 
import Language.Haskell.Interpreter (Id, ModuleName)
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

-- Parsing 
import Language.Haskell.Parser (ParseResult(..), parseModule)

-- * Abstract syntax 

-- | Parse a string representation of a type signature to an abstract 
-- qualified type representation if possible.
--
-- > prettyPrint <$> parseTypeSig "foo :: Int -> Int" = Just "Int -> Int"
parseTySig :: String -> Maybe HsQualType
parseTySig s = case parseModule s of 
  ParseOk (HsModule _ _ _ _ [HsTypeSig _ _ qTy]) -> Just qTy
  _ -> Nothing 

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

-- ** Syntactic checks 

-- | Check whether a 'HsType' contains one or more type variables.
hasTyVars :: HsType -> Bool 
hasTyVars HsTyVar{} = True
hasTyVars HsTyCon{} = False
hasTyVars (HsTyTuple ts)  = any hasTyVars ts
hasTyVars (HsTyFun t1 t2) = hasTyVars t1 || hasTyVars t2
hasTyVars (HsTyApp t1 t2) = hasTyVars t1 || hasTyVars t2

-- | Check whether a 'HsType' ia a nullary function type.
isNullaryTyFun :: HsType -> Bool
isNullaryTyFun HsTyFun{} = False
isNullaryTyFun _         = True

-- | Check whether a 'HsType' ia a unary function type.
isUnaryTyFun :: HsType -> Bool 
isUnaryTyFun (HsTyFun _ HsTyFun{}) = False 
isUnaryTyFun HsTyFun{} = True
isUnaryTyFun _ = False

-- | Check whether a 'HsType' ia a binary function type.
isBinaryTyFun :: HsType -> Bool 
isBinaryTyFun (HsTyFun _ (HsTyFun _ HsTyFun{})) = False
isBinaryTyFun (HsTyFun _ HsTyFun{}) = True 
isBinaryTyFun _ = False 

-- | Check whether a 'HsQualType' meets the /unqualified/ syntactic type 
-- requirements of AutoBench, i.e., has an empty context.
isUnqualQualTy :: HsQualType -> Bool 
isUnqualQualTy (HsQualType [] _) = True 
isUnqualQualTy _ = False

-- | Check whether a 'HsType' meets the syntactic type requirements of 
-- AutoBench, i.e., is a nullary, unary, or binary function type.
isABTyFun :: HsType -> Bool 
isABTyFun ty = isNullaryTyFun ty || isUnaryTyFun ty || isBinaryTyFun ty 

-- | Check whether a 'HsType' meets the /testable/ syntactic type 
-- requirements of AutoBench, i.e., is a unary, or binary function type.
isABTestTyFun :: HsType -> Bool 
isABTestTyFun ty = isUnaryTyFun ty || isBinaryTyFun ty 

-- | Check whether a 'HsType' meets the /genable/ syntactic type 
-- requirements of AutoBench, i.e., 'isABTestTyFun' and input types do not
-- contain type variables.
--
-- The latter requirement is because QuickCheck cannot generate /sized/ test 
-- data for polymorphic types because it defaults to (), which clearly doesn't 
-- have a /sensible/ notion of size. 
--
-- Examples in pseudocode:
--
-- * Int -> Int      ===> True 
-- * Int             ===> False     -- not 'isABTestTyFun'
-- * a -> Int -> Int ===> False     -- containts tyVar 'a'
isABGenTyFun :: HsType -> Bool 
isABGenTyFun ty = isABTestTyFun ty && noTyVars (tyFunInps ty) 
  where 
    noTyVars (HsTyTuple [t])      = not (hasTyVars t)
    noTyVars (HsTyTuple [t1, t2]) = not (hasTyVars t1 || hasTyVars t2)
    noTyVars _ = False -- shouldn't happen