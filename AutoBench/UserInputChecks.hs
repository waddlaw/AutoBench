
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.UserInputChecks
  Description : Interpreting, validating, and classifying user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  User inputs are always passed to the system using files (Haskell modules), 
  which must be interpreted and validated. Test suites ('TestSuites') are 
  AutoBench's principle user input datatype, and are used to structure 
  performance tests into logical units. Thus, input files will typically 
  contain one or more test suites. In addition, input files will contain test 
  programs (those whose performance is to be analysed/compared) and possibly 
  user-specified test data.

  As user inputs have a number of different purposes, but are all specified
  in the same way (i.e., inside files), the system must not only validate each 
  type of user input (test suite, test program, test data, etc.) appropriately, 
  but it must also classify them. 

  The classification of user inputs is a non-trivial procedure, due to the fact 
  that, for example, any program to be tested using the system must satisfy the 
  following properties:

  * Be a unary or binary function;
  * Have an input type that is a member of the 'NFData' type class;
  * Have an input type that is a member of the 'Arbitrary' type class 
     OR
    Be associated with valid user-specified test data;
  * Be referred to by a valid test suite;

  In addition, the above properties in themselves rely on the classification of 
  \'unary functions\', \'valid test data\', \'valid test suites\', 
  \'functions whose input types are members of the 'NFData' type class\', etc.
  
  On top of this, two different classification procedures are required. For 
  example, a type signature can be used to determine whether a function is 
  unary, binary, or otherwise. However, to the best of my knowledge, there is no 
  generic way of checking whether a function's input type is a member of the 
  'NFData' type class aside from a dynamic test. Therefore, user inputs
  can be classified according the property \'is a unary function\' /statically/,
  but only /dynamic/ checks can be used to classify functions according to
  the property \'has input types that are members of the 'NFData' type class\'.
  
  As such, the system performs static validation/classification
  ('AutoBench.StaticChecks') and dynamic validation/classification 
  ('AutoBench.DynamicChecks').

  This module is responsible for coordinating the overall checking process.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.UserInputChecks
  (

  ) where 

