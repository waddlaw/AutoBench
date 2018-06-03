
{-# LANGUAGE FlexibleInstances #-}  
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.QuickCheck
  Description : <TO-DO>
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  <TO-DO>

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
  , generate
  , resize
  , sized
  , vectorOf
  )


-- | We override QuickCheck's Arbitrary instance for [Int] because the standard 
-- implemenetation for [a] (in Test.QuickCheck.Arbitrary) generates lists /up 
-- to/ a certain size, where as we want to /fix/ the size.
instance {-# OVERLAPPING #-} Arbitrary [Int] where 
  arbitrary = sized $ \n -> vectorOf n arbitrary