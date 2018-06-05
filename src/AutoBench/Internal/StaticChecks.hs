
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.Internal.StaticChecks
  Description : Statically validating and classifying user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  To statically validate and classify user inputs, the system uses an abstract 
  representation of Haskell 98 (see 'AutoBench.Internal.AbstractSyntax'). This 
  representation allows the system to check syntactic properties of types. 
  In particular, the system checks whether the types of user inputs have the 
  following properties:

  1. Typeable: the types of inputs adhere to Haskell 98 standards;               => 
  2. Unqualified: the types of inputs are unqualified;                           => added to '_validElems'
  3. Function: inputs are nullary/unary/binary functions.                        =>

  These properties are used to validate user inputs, as any input that does not 
  satisfy all three is incompatible with the system, and thus is added to the
  '_invalidElems' list of 'UserInputs'.
  
  Additional checks are then used to classify user inputs as follows:

  4.  NullaryFun: a nullary function;                                            => added to '_nullaryFuns'
  5.  UnaryFun: a unary function;                                                => added to '_unaryFuns'
  6.  BinaryFun: a binary function;                                              => added to '_binaryFuns'
  7.  Genable: unary/binary functions that do not contain type variables in 
      their input types;                                                         => added to '_arbFuns'
  8.  UnaryData: a nullary function with type @UnaryTestData x@ for some type 
      @x@;                                                                       => added to '_unaryData'
  9.  BinaryData: a nullary function with type @BinaryTestData x y@ for some 
      types @x@ and @y@.                                                         => added to '_binaryData'
  
  Subsequent /dynamic/ checks (see 'AutoBench.Internal.DynamicChecks') are 
  performed to further (re-)classify user inputs according to additional 
  properties they satisfy.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   -
-}

module AutoBench.Internal.StaticChecks
  (

  -- * Syntactic checks
    parseTySig          -- Parse a string representation of a type signature to an abstract qualified type representation.
                        -- Any type that can be parsed from a 'TypeString' satisfies the /typeable/ syntactic property.
  , isNullaryTyFun      -- Is a 'HsType' a nullary function type?
  , isUnaryTyFun        -- Is a 'HsType' a unary function type? 
  , isBinaryTyFun       -- Is a 'HsType' a binary function type?
  , isABTyFun           -- Does a 'HsType' satisfy the /function/ syntactic property?
  , isUnqualQualTy      -- Does a 'HsQualType' satisfy the /unqualified/ syntactic property?
  , isABGenTyFun        -- Does a 'HsType' satisfy the /genable/ syntactic property?
  , isUnaryTestData     -- Does a 'HsType' correspond to @UnaryTestData x@ for some type @x@?
  , isBinaryTestData    -- Does a 'HsType' correspond to @BinaryTestData x y@ for some types @x@ and @y@?
  , isTestSuite         -- Does a 'HsType' correspond to @TestSuite@?
  -- * Helper functions
  , testDataTyFunInps   -- Extract the input types from the abstract representations of /UnaryTestData/ and /BinaryTestData/.

  ) where 

import Language.Haskell.Parser (ParseResult(..), parseModule)

import AutoBench.Internal.AbstractSyntax
  ( HsDecl(..)
  , HsModule(..)
  , HsQualType(..)
  , HsType(..)
  , TypeString
  , binaryTestDataTyCon
  , hasTyVars
  , testSuiteTyCon
  , tyFunInps
  , unaryTestDataTyCon
  )

-- * Syntactic checks

-- | Parse a string representation of a type signature to an abstract 
-- qualified type representation if possible.
--
-- > prettyPrint <$> parseTypeSig "foo :: Int -> Int" = Just "Int -> Int"
parseTySig :: TypeString -> Maybe HsQualType
parseTySig s = case parseModule s of 
  ParseOk (HsModule _ _ _ _ [HsTypeSig _ _ qTy]) -> Just qTy
  _ -> Nothing 

-- | Check whether a 'HsType' is a nullary function type.
isNullaryTyFun :: HsType -> Bool
isNullaryTyFun HsTyFun{} = False
isNullaryTyFun _         = True

-- | Check whether a 'HsType' is a unary function type.
isUnaryTyFun :: HsType -> Bool 
isUnaryTyFun (HsTyFun _ HsTyFun{}) = False 
isUnaryTyFun HsTyFun{} = True
isUnaryTyFun _ = False

-- | Check whether a 'HsType' is a binary function type.
isBinaryTyFun :: HsType -> Bool 
isBinaryTyFun (HsTyFun _ (HsTyFun _ HsTyFun{})) = False
isBinaryTyFun (HsTyFun _ HsTyFun{}) = True 
isBinaryTyFun _ = False 

-- | Check whether a 'HsQualType' satisfies the /unqualified/ syntactic 
-- property, i.e., has an empty context.
isUnqualQualTy :: HsQualType -> Bool 
isUnqualQualTy (HsQualType [] _) = True 
isUnqualQualTy _ = False

-- | Check whether a 'HsType' satisfies the /function/ syntactic property, 
-- i.e., is a nullary, unary, or binary function type.
isABTyFun :: HsType -> Bool 
isABTyFun ty = isNullaryTyFun ty || isUnaryTyFun ty || isBinaryTyFun ty

-- | Check whether a 'HsType' satisfies the /genable/ syntactic property, 
-- i.e., is a unary or binary function type whose input types do not contain 
-- type  variables.
--
-- The latter requirement is needed because QuickCheck cannot generate sized 
-- test data for polymorphic types because it defaults to (), which clearly 
-- doesn't have a sensible notion of size. 
--
-- Examples in pseudocode:
--
-- * Int -> Int      ===> True 
-- * Int             ===> False     -- nullary function type
-- * a -> Int -> Int ===> False     -- contains a type variable @a@
isABGenTyFun :: HsType -> Bool 
isABGenTyFun ty = (isUnaryTyFun ty || isBinaryTyFun ty) && noTyVars (tyFunInps ty) 
  where 
    noTyVars (HsTyTuple [t])      = not (hasTyVars t)
    noTyVars (HsTyTuple [t1, t2]) = not (hasTyVars t1 || hasTyVars t2)
    noTyVars _ = False -- shouldn't happen

-- | Check whether a 'HsType' corresponds to @UnaryTestData x@ for some type 
-- @x@.
isUnaryTestData :: HsType -> Bool 
isUnaryTestData (HsTyApp tyCon _) = tyCon == unaryTestDataTyCon 
isUnaryTestData _ = False

-- | Check whether a 'HsType' corresponds to @BinaryTestData x y@ for some types
-- @x@ and @y@.
isBinaryTestData :: HsType -> Bool 
isBinaryTestData (HsTyApp (HsTyApp tyCon _) _) = tyCon == binaryTestDataTyCon 
isBinaryTestData _ = False

-- | Check whether a 'HsType' corresponds to @TestSuite@.
isTestSuite :: HsType -> Bool 
isTestSuite ty = ty == testSuiteTyCon

-- * Helper functions

-- | Extract the input types from the abstract representations of 
-- /UnaryTestData/ and /BinaryTestData/.
testDataTyFunInps :: HsType -> HsType
testDataTyFunInps (HsTyApp (HsTyApp tyCon t1) t2) 
  | tyCon == binaryTestDataTyCon = HsTyTuple [t1, t2]
testDataTyFunInps (HsTyApp tyCon t) 
  | tyCon == unaryTestDataTyCon = HsTyTuple [t]
testDataTyFunInps _ = HsTyTuple []