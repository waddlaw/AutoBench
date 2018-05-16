
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wall   #-}

{-|

  Module      : AutoBench.Types
  Description : Data types and associated helper functions\/defaults.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  Data types used throughout AutoBench's implementation and any associated 
  helper functions\/defaults.

-}

data TestConfig = 
  TestConfig
   {
     _verbosity :: Verbosity        -- ^ The console's verbosity level.
   }

data TestSuite = 
  TestSuite
   { 
     _progs    :: [Id]              -- ^ Identifiers of the programs to test: note all programs in the user 
                                    --   input file will be considered if this list is empty.
   , _dataOpts :: DataOpts          -- ^ Test data options ('DataOpts').
   , _analOpts :: AnalOpts          -- ^ Statistical analysis options ('AnalOpts').
   , _critCfg  :: Criterion.Config  -- ^ Criterion's configuration ('Criterion.Types.Config').
   , _baseline :: Bool              -- ^ Whether the graphs of runtime results should include baseline measurements.
   , _nf       :: Bool              -- ^ Whether test cases should be evaluated to nf (@True@) or whnf (@False@).
   , _ghcFlags :: [String]          -- ^ GHC compiler flags used when compiling 'BenchSuite's.
   
   }



-- BenchSuite 