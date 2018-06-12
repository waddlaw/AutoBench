
{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}


{-|

  Module      : AutoBench.QuickCheck
  Description : Default 'Arbitrary' instances for AutoBench.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module defines a number of default 'Arbitrary' instances to be used with 
  AutoBench. These purposefully override 'Arbitrary' instances in 
  'Test.QuickCheck.Arbitrary' because those are incompatible with this system.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.QuickCheck where 

import Test.QuickCheck 
  ( Arbitrary
  , Gen
  , arbitrary
  , sized
  , vectorOf
  )


-- Fixed size lists -----------------------------------------------------------

-- The default 'Arbitrary' instance for lists in 'Test.QuickCheck.Arbitrary'
-- generates lists of random size, this is not compatible with AutoBench.

-- I want to do this but can't?
-- instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary [a] where
--   arbitrary = sized $ \n -> vectorOf n arbitrary

sizedArbitraryVector :: Arbitrary a => Gen [a]
sizedArbitraryVector  = sized $ \n -> vectorOf n arbitrary

instance {-# OVERLAPPING #-} Arbitrary [Bool] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Char] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Double] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Float] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Int] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Integer] where 
  arbitrary = sizedArbitraryVector


{-

instance {-# OVERLAPPING #-} Arbitrary [Int8] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Int16] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Int32] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Int64] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Ordering] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Word] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Word8] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Word16] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Word32] where 
  arbitrary = sizedArbitraryVector

instance {-# OVERLAPPING #-} Arbitrary [Word64] where 
  arbitrary = sizedArbitraryVector

-}