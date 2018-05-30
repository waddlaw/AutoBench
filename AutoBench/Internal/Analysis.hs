

{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall      #-} 

{-|

  Module      : AutoBench.Internal.Analysis
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

module AutoBench.Internal.Analysis where 

import Data.Default (def)
import Data.Either  (partitionEithers)
import Data.List    (sort)

import AutoBench.Internal.AbstractSyntax (Id)    
import AutoBench.Internal.Utils          (allEq, notNull, uniqPairs)
import AutoBench.Internal.Types  
  ( AnalOpts(..)
  , BenchReport(..)
  , Coord
  , Coord3
  , DataSize(..)
  , Improvement
  , InputError(..)
  , LinearType
  , SimpleReport(..)
  , TestReport(..)
  , maxCVIters
  , maxCVTrain
  , maxPredictors
  , minCVIters
  , minCVTrain
  , numPredictors
  )   






-- * Top-level 

analyse :: TestReport -> IO ()
analyse  = analyseWith def

analyseWith :: AnalOpts -> TestReport -> IO () 
analyseWith aOpts tr = do 
  let errs = checkAnalOpts
  if notNull errs
  then do 
    putStrLn "Cannot analyse results due to one or more 'AnalOpts' errors:"
    mapM_ print errs
  else do 
    -- Calculate the efficiency improvement results by comparing 
    -- the runtimes of test programs pointwise.
    let improvs = calculateImprovements (_reports $ _br tr) aOpts
    error $ show improvs

    undefined


  where 
   
    -- Valid 'AnalOpts':                                                                             
    -- Ensure the linear models have <= maximum number of allowed 
    -- predictors. Check the '_cvIters', '_cvTrain', and '_topModels' values 
    -- are in the correct range.
    checkAnalOpts :: [InputError]                                                          -- <TO-DO>: Merge this with static checks.
    checkAnalOpts  =
      checkModels (_linearModels aOpts) 
        ++ checkCVIters   (_cvIters   aOpts) 
        ++ checkCVTrain   (_cvTrain   aOpts)
        ++ checkTopModels (_topModels aOpts)
      where 
        -- Maximum number of predictors for linear models.
        checkModels :: [LinearType] -> [InputError]
        checkModels ls 
          | maxPredictors >= maximum (fmap numPredictors ls) = []
          | otherwise = [aOptsModelErr]
        
        -- 100 <= '_cvIters' 500.
        checkCVIters n 
          | n >= minCVIters && n <= maxCVIters = []
          | otherwise = [aOptsCVItersErr]
        
        -- 0.5 <= '_cvTrain' 0.8.
        checkCVTrain n 
          | n >= minCVTrain && n <= maxCVTrain = []
          | otherwise = [aOptsCVTrainErr]

        -- 'topModels' strictly positive.
        checkTopModels n 
          | n > 0 = []
          | otherwise = [aOptsTopModelsErr] 

    -- Errors:
    aOptsModelErr     = AnalOptsErr $ "Linear regression models can have a maximum of " ++ show maxPredictors ++ " predictors."
    aOptsCVItersErr   = AnalOptsErr $ "The number of cross-validation iterators must be " ++ show minCVIters ++ " <= x <= " ++ show maxCVIters ++ "." 
    aOptsCVTrainErr   = AnalOptsErr $ "The percentage of cross-validation training data must be " ++ show minCVTrain ++ " <= x <= " ++ show maxCVTrain ++ "." 
    aOptsTopModelsErr = AnalOptsErr $ "The number of models to review must be strictly positive."









-- * Improvement results 

-- | Calculate efficiency improvements by comparing the runtimes of test 
-- programs pointwise.
calculateImprovements :: [[SimpleReport]] -> AnalOpts -> [Improvement]
calculateImprovements srs aOpts                                                  -- Basic validation checks:
  | any null srs = []                                                            -- Make sure no list of simple reports is empty.  
  | not (allEq $ fmap length srs) = []                                           -- Make sure all lists are the same length.
  | not (checkCoords srCoords $ length $ head srs) = []                          -- Make sure all lists of coords are the same form and the expected length.
  | otherwise = concatMap (uncurry calculateImprovement) (uniqPairs $ 
      zip srNames srCoords)

  where 
    -- Names of test programs from 'SimpleReport's.
    srNames  = fmap (_name . head) srs           
    -- (Input size(s), runtime) coordinates.         
    srCoords = zipWith simpleReportsToCoords srNames srs  

    -- Compare the runtimes of two test programs pointwise. Only compare
    -- unary against unary and binary against binary.
    -- Also match the coordinates to ensure that the sizing information for
    -- each runtime measurement is the same (it should be by the construction
    -- of the list, but better to check).
    calculateImprovement
     :: (Id, Either [Coord] [Coord3]) 
     -> (Id, Either [Coord] [Coord3])
     -> [Improvement]
    calculateImprovement (idt1, Left cs1) (idt2, Left cs2) -- Unary against unary.
      -- If all coordinates didn't match, then can't generate improvements.
      | length toCompare < length cs1 = [] 
      | otherwise = case (_improv aOpts) toCompare of 
          Nothing       -> []
          Just (ord, d) -> [(idt1, ord, idt2, d)]
      where toCompare = matchCoords (sort cs1) (sort cs2)
    calculateImprovement (idt1, Right cs1) (idt2, Right cs2) -- Binary against binary.
      -- If all coordinates didn't match, then can't generate improvements.
      | length toCompare < length cs1 = []
      | otherwise = case (_improv aOpts) toCompare of 
          Nothing       -> []
          Just (ord, d) -> [(idt1, ord, idt2, d)]
      where toCompare = matchCoords3 (sort cs1) (sort cs2) 
    calculateImprovement _ _ = [] -- Shouldn't happen.

    -- Helpers

    -- Check coords are all of the same form (i.e., all 'Coord' or
    -- all 'Coord3') and the same, expected length @n@.
    checkCoords :: [Either [Coord] [Coord3]] -> Int -> Bool 
    checkCoords css n = case partitionEithers css of
      ([], c3s) -> allEq (n : fmap length c3s)
      (cs, [])  -> allEq (n : fmap length cs)
      _         -> False  
    
    -- Make sure coordinates to compare have the same size information. This 
    -- should always be the case because of how they are generated, but better 
    -- to check.
    matchCoords :: [Coord] -> [Coord] -> [(Double, Double)]
    matchCoords [] _  = []
    matchCoords _  [] = []
    matchCoords ((s1, t1) : cs1) ((s1', t2) : cs2)
      | s1 == s1' = (t1, t2) : matchCoords cs1 cs2 
      | otherwise = matchCoords cs1 cs2 

    -- As above but for 'Coord3'.
    matchCoords3 :: [Coord3] -> [Coord3] -> [(Double, Double)]
    matchCoords3 [] _  = []
    matchCoords3 _  [] = []
    matchCoords3 ((s1, s2, t1) : cs1) ((s1', s2', t2) : cs2)
      | s1 == s1' && s2 == s2' = (t1, t2) : matchCoords3 cs1 cs2 
      | otherwise = matchCoords3 cs1 cs2 

-- * Helpers 

-- | Convert a list of 'SimpleReport's to a list of (input size(s), runtime) 
-- coordinates, i.e., a list 'Coord's or 'Coord3's. The name of each 
-- simple report is verified against the given test program identifier.
simpleReportsToCoords :: Id -> [SimpleReport] -> Either [Coord] [Coord3]
simpleReportsToCoords idt srs = case (cs, cs3) of 
  ([], _) -> Right cs3 
  (_, []) -> Left cs 
  _       -> Left [] -- Shouldn't happen.
  where 
    srs' = filter (\sr -> _name sr == idt) srs
    (cs, cs3) = partitionEithers (fmap simpleReportToCoord srs')

-- | Convert a 'SimpleReport' to a (input size(s), runtime) coordinate, 
-- i.e., 'Coord' or 'Coord3'.
simpleReportToCoord :: SimpleReport -> Either Coord Coord3 
simpleReportToCoord sr = case _size sr of 
  SizeUn n      -> Left  (fromIntegral n, _runtime sr)
  SizeBin n1 n2 -> Right (fromIntegral n1, fromIntegral n2, _runtime sr)