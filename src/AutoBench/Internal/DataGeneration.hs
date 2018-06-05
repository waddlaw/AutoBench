
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.Internal.DataGeneration
  Description : Generate test data using QuickCheck.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module is responsible for generating test data using QuickCheck.
  
  The types do all the work here, so when these functions are used in 
  a specific context, they generate the right data. Only the size
  range of the inputs to be generated must be specified using 'DataOpts'.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.Internal.DataGeneration 
  (

    genDataBin -- Generate test data for binary test programs in the given size range.
  , genDataUn  -- Generate test data for unary test programs in the given size range.

  ) where 

import Test.QuickCheck (Arbitrary, arbitrary, generate, resize)
import AutoBench.Internal.Types (DataOpts(..), toHRange)

-- | Generate test data for binary test programs in the given size range.
-- Using the size range 'Gen l s u' and generating all pairs.
-- I.e., [5,10..100] => [(5,5),(5, 10)..(5,100),(10, 5)..(100, 100)] 
genDataBin :: (Arbitrary a, Arbitrary b) => DataOpts -> [IO (a, b)] 
genDataBin dOpts = 
  [ (,) <$> generate (resize s1 arbitrary)   
        <*> generate (resize s2 arbitrary)
  | (s1, s2) <- (,) <$> size <*> size ]
  where size = toHRange dOpts

-- | Generate test data for unary test programs in the given size range.
genDataUn :: Arbitrary a => DataOpts -> [IO a] 
genDataUn dOpts = (generate . flip resize arbitrary) <$> toHRange dOpts