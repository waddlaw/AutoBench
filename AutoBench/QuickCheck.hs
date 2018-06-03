
{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}  

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
  , arbitrary
  , sized
  , vectorOf
  )


-- LISTS ----------------------------------------------------------------------

-- The default 'Arbitrary' instance for lists in 'Test.QuickCheck.Arbitrary'
-- generates lists of random size, this is not compatible with AutoBench.

-- Want to do this but can't?
-- instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary [a] where
--   arbitrary = sized $ \n -> vectorOf n arbitrary

instance {-# OVERLAPPING #-} Arbitrary [Int] where 
  arbitrary = sized $ \n -> vectorOf n arbitrary


