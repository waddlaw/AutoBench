
{-# OPTIONS_GHC -Wall #-}


{-|

  Module      : AutoBench.Internal.DataGeneration
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
   - Comment all;
-}

module AutoBench.Internal.DataGeneration 
  (

    genDataBin -- Generate test data for binary test programs in the given size range.
  , genDataUn  -- Generate test data for unary test programs in the given size range.

  ) where 

import Test.QuickCheck (Arbitrary, arbitrary, generate, resize)
import AutoBench.Internal.Types (DataOpts(..), toHRange)

-- | Generate test data for binary test programs in the given size range.
genDataBin :: (Arbitrary a, Arbitrary b) => DataOpts -> [IO (a, b)] 
genDataBin dOpts = 
  [ (,) <$> generate (resize s1 arbitrary)
        <*> generate (resize s2 arbitrary)
  | (s1, s2) <- (,) <$> size <*> size ]
  where size = toHRange dOpts

-- | Generate test data for unary test programs in the given size range.
genDataUn :: Arbitrary a => DataOpts -> [IO a] 
genDataUn dOpts = (generate . flip resize arbitrary) <$> toHRange dOpts