
{-# OPTIONS_GHC -Wall #-}

{-|

  Module      : AutoBench.DynamicChecks
  Description : Dynamically validating and classifying user inputs.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  A number of dynamic checks are used to classify user inputs according to 
  properties that cannot be checked statically (see 'AutoBench.StaticChecks'
  for static checking). For example, the system cannot determine whether the 
  input types of user-specified test programs are members of the 'Arbitrary' or 
  'NFData' type classes by simply inspecting their type signatures. Instead,
  dynamic checks are used to determine whether these properties hold.

  Following static checking, the system determines whether the types of user 
  inputs have the following properties:

  1. NFDataInput: functions satisfying the static properties of /unaryFun/ and
     /binaryFun/ whose input types are members of the 'NFData' type class;       ==> added to '_benchFuns'
  2. NFDataOutput: functions satisfying the /nFDataInput/ dynamic property
     whose result types are members of the 'NFData' type class;                  ==> added to '_nfFuns'
  3. Arbitrary: functions satisfying the /genable/ static property whose 
     input types are members of the 'Arbitrary' type class;                      ==> kept in '_arbFuns'

  The system all interprets a number of user inputs in order to perform a 
  number of dynamic checks on the definitions themselves: 

  4. TestSuites: functions satisfying the /nullaryFun/ static property whose     
     types are @TestSuite@;                                                      ==> interpreted and added to '_testSuites'
  5. FullTestSuites: interpreted 'TestSuites' whose record fields are            
     all initialised;                                                            ==> kept in '_testSuites'
  6. ValidUnaryData: functions satisfying the /unaryData/ static property whose  
     definitions are valid;                                                      ==> interpreted, checked, but only Id kept in '_unaryData'
  7. ValidBinaryData: functions satisfying the /binaryData/ static property 
     whose definitions are valid.                                                ==> interpreted, checked, but only Id kept in '_binaryData'
     
  User inputs that fail checks 5-7 are added to the the respective invalid
  lists, for example '_invalidTestSuites' for failing check 5.

  Following dynamic checking, test suites in the '_testSuites' list are subject
  to further static checks (see AutoBench.StaticChecks).

-}