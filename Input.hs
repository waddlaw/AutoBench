
{-# LANGUAGE FlexibleInstances #-}  

module Input where 

import AutoBench.Types 
import Data.Default


import Test.QuickCheck (Arbitrary, arbitrary, generate, resize, sized, vectorOf)


-- | We override QuickCheck's Arbitrary instance for [Int] because the standard 
-- implemenetation for [a] (in Test.QuickCheck.Arbitrary) generates lists /up 
-- to/ a certain size, where as we want to /fix/ the size.
instance {-# OVERLAPPING #-} Arbitrary [Int] where 
  arbitrary = sized $ \n -> vectorOf n arbitrary


slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys) 

ts :: TestSuite 
ts  = def 