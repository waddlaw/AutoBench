
{-# LANGUAGE FlexibleInstances #-}  

module Input where 

import AutoBench.Types 
import Data.Default 
import Data.List





import AutoBench.Internal.IO 
import AutoBench.Internal.UserInputChecks
import AutoBench.Internal.Types
import AutoBench.Internal.Analysis

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
ts  = def { _dataOpts = Gen 5 5 100, _progs = ["sort", "sort2", "sort3333", "sort4" ]}


const :: Int -> Int -> Int
const x y = x 

const2 :: Int -> Int -> Int
const2 x y = x 

sort :: [Int] -> [Int] 
sort  = List.sort

sort2 = Input.sort
sort3333 = Input.sort
sort4 = Input.sort