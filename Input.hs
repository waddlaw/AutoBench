
{-# LANGUAGE FlexibleInstances #-}  

module Input where 

import AutoBench.Types 
import Data.Default


import AutoBench.QuickBench

import Prelude hiding (id, const)


import qualified Data.List as List

import Test.QuickCheck (Arbitrary, arbitrary, generate, resize, sized, vectorOf)


-- | We override QuickCheck's Arbitrary instance for [Int] because the standard 
-- implemenetation for [a] (in Test.QuickCheck.Arbitrary) generates lists /up 
-- to/ a certain size, where as we want to /fix/ the size.
instance {-# OVERLAPPING #-} Arbitrary [Int] where 
  arbitrary = sized $ \n -> vectorOf n arbitrary


{-
slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys) -}



ts :: TestSuite 
ts  = def { _dataOpts = Gen 5 5 100, _progs = ["sort"], _ghcFlags = ["loll", "lolll", "llllol", "lllloll", "lolll", "lollll", "lopppl", "oolol", "loiiil", "loooiol", "loyyhl"] }


const :: Int -> Int -> Int
const x y = x 

const2 :: Int -> Int -> Int
const2 x y = x 

sort :: [Int] -> [Int] 
sort  = List.sort

sort2 :: [Int] -> [Int]
sort2  = sort

sort3333 = Input.sort
sort4 = Input.sort