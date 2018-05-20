
module Input where 

import AutoBench.Types 
import Data.Default 

import Prelude hiding (id)

tDat :: UnaryTestData Int
tDat  = take 20 $ zip [1..] (fmap return [1..])

tDat2 :: BinaryTestData Int Int
tDat2  = []

const_ :: Int -> Int -> Int 
const_ x y = x

id :: a -> a 
id x = x 

{-
recFib :: Int -> Int
recFib 0 = 0
recFib 1 = 1
recFib n = recFib (n-1) + recFib (n-2)

dynamicFib :: Int -> Int
dynamicFib n = a ! n
  where
    a :: Array Int Int
    a = listArray (0,n) (map f [0..n])

    f :: Int -> Int
    f 0 = 0
    f 1 = 1
    f n = a ! (n-1) + a ! (n-2)

tData :: TestDataUn Int 
tData  = fmap (\i -> (i, return i)) [2,4..48]

testOpts :: TestOpts 
testOpts  = defaultTestOpts { dat = Manual }  -}


slowRev :: [Int] -> [Int]
slowRev []       = []
slowRev (x : xs) = slowRev xs ++ [x]

fastRev :: [Int] -> [Int]
fastRev xs = go xs []
  where 
    go [] ys       = ys 
    go (x : xs) ys = go xs (x : ys)


{-
{-
quickSort :: [Int] -> [Int]
quickSort []       = []
quickSort (x : xs) = quickSort small ++ (x : quickSort large)
  where 
    small = [y | y <- xs, y <= x]
    large = [y | y <- xs, y > x]

mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort $ fstHalf xs) (mergeSort $ sndHalf xs)
  where 
    fstHalf xs = take (length xs `div` 2) xs
    sndHalf xs = drop (length xs `div` 2) xs

    merge xs [] = xs
    merge [] ys = ys 
    merge (x : xs) (y : ys) 
      | (x <= y)  = x : (merge xs (y : ys)) 
      | otherwise = y : (merge (x : xs) ys)

defSort :: [Int] -> [Int]
defSort  = sort -}

tOpts :: TestOpts 
tOpts  = defaultTestOpts { verbosity = Verbose }


test :: Int -> Int -> Int -> Int 
test x y s = s

test2 :: a -> b 
test2 x = undefined


class YesNo a where  
    yesno :: a -> Bool 


tDat1 :: TestDataUn Int 
tDat1  = []

tDat2 :: TestDataBin Int Int 
tDat2 = []


tConfig :: TestConfig 
tConfig  = TestConfig { _verbosity  = Normal }

tConfig2 :: TestConfig 
tConfig2  = TestConfig {}


tSuite :: TestSuiteOpts
tSuite = defaultTestSuiteOpts { _dataOpts = Gen 0 0 0, _progs = ["lol"] }

tSuite2 :: TestSuiteOpts
tSuite2 = TestSuiteOpts {}-}

data Test = Test

ts :: TestSuite 
ts  = def { _analOpts = def { runtimeComp = undefined} }