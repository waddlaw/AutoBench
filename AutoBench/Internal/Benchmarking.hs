
{-# OPTIONS_GHC -Wall #-}


{-|

  Module      : AutoBench.Internal.Benchmarking
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

module AutoBench.Internal.Benchmarking 
  (
    genBenchSuites  -- <TO-DO>
  ) where 


import AutoBench.AbstractSyntax (Id, ModuleName)
import AutoBench.Types          (BenchSuite(..), TestSuite(..))





-- Generate BenchSuites from TestSuites
genBenchSuites :: ModuleName -> [(Id, TestSuite)] -> [BenchSuite]
genBenchSuites mn = reIndex . concatMap (uncurry genBenchSuite)
  where 
    -- For each test suite, /should/ generate one benchmarking suite.
    -- We perform an extra check here to ensure that the '_progs' list 
    -- isn't empty.
    genBenchSuite :: Id -> TestSuite -> [BenchSuite]
    genBenchSuite idt ts 
      -- This should never happen.
      | null (_progs ts) = []
      -- We just copy information across verbatim.
      | otherwise = 
          [ BenchSuite 
              {
                _benchID       = 0  -- Placeholder for now.
              , _benchIDT      = idt 
              , _moduleName    = mn
              , _benchProgs    = _progs    ts 
              , _benchDataOpts = _dataOpts ts
              , _benchNf       = _nf       ts 
              , _benchBaseline = _baseline ts
              }
          ]

    -- Helpers:
    -- Re-index to make sure each 'BenchSuite' integer identifier is unique.
    reIndex = fmap (uncurry addIndex) . zip [1..]
      where addIndex idx bs = bs { _benchID = idx }

