
{-# OPTIONS_GHC -Wall #-}

-- |
--
-- Module      : AutoBench.Internal.PrettyPrinting
-- Description : Pretty printing
-- Copyright   : (c) 2018 Martin Handley
-- License     : BSD-style
-- Maintainer  : martin.handley@nottingham.ac.uk
-- Stability   : Experimental
-- Portability : GHC
--
-- This module handles system-wide pretty printing.
--

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------
-- * Fix showImprovements, docImprovement;

module AutoBench.Internal.PrettyPrinting
  (

  -- * Datatypes 
  -- ** User Inputs    
    docDataOpts                           -- Pretty print data options.
  , docDataSize                           -- Pretty print data size.
  , docTestSuite_ProgsDataOpts            -- Pretty print test suites: prints the names of test programs and data options only.
  , docUserInputs                         -- Pretty print user inputs.
  -- ** Benchmarking
  , docBenchReport                        -- Pretty print benchmarking reports.
  , docTestReport                         -- Pretty print test reports.

  -- ** Statistical analysis 
  , docCoords                             -- Pretty print coordinates.
  , docCVStats                            -- Pretty print cross-validated fitting statistics. 
  , docLinearType                         -- Pretty print linear types.
  , docQuickResults                       -- Pretty print quick results.
  , docQuickResultss                      -- Pretty print a list of quick results.
  , docSimpleReport_NameDataSizeRuntime   -- Pretty print simple reports: prints the name of the test program, the size of the test data, and its runtime only.
  , docSimpleResults                      -- Pretty print simple results.
  , docSimpleResultss                     -- Pretty print a list of simple results.
  , docStats                              -- Pretty print fitting statistics.

  -- * Utilities 
  -- ** Bullet points 
  , arrowBullet                           -- Add an arrow bullet point to a string.
  , circleBullet                          -- Add a circle bullet point to a string.
  -- ** Fonts
  , italic                                -- Add italic Unicode formatting.
  -- ** Formatting 
  , commaSeparate                         -- Comma separate a list of 'PP.Doc's.
  , equation                              -- An equation (LHS = RHS).
  , hscat                                 -- 'PP.hcat' for lists of strings.
  , wrappedList                           -- Wrapped, comma-separated list.
  -- ** Shapes 
  , cross                                 -- A cross Unicode character.
  , questionMark                          -- A question mark Unicode character.
  , tick                                  -- A tick Unicode character.
  -- ** Style
  , lineStyle                             -- Create a 'PP.Style' with a given line width.



  -- * To Fix
  , showImprovements
  , docImprovement
  , wrapDocExpr

  ) where 

import           Control.Arrow             ((&&&))
import           Criterion.Types           (OutlierEffect(..))
import           Data.Bits                 (xor)
import           Data.List                 (sort, sortBy, transpose)
import           Data.List.Split           (chunksOf)
import           Data.Ord                  (comparing)
import           Data.Tuple.Select         (sel1, sel2, sel3)
import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.Printf               (printf)

import           AutoBench.Internal.AbstractSyntax ( HsType(..), Id
                                                   , ModuleElem(..), TypeString
                                                   , prettyPrint, unqualIdt )
import qualified AutoBench.Internal.Expr           as E
import           AutoBench.Internal.Utils          ( bySide, deggar, forceSecs
                                                   , secs, subNum, superNum
                                                   , wrapPPList )

import AutoBench.Internal.Types 
  ( BenchReport(..)
  , Coord
  , Coord3 
  , CVStats(..)
  , DataOpts(..)
  , DataSize(..)
  , Improvement
  , InputError
  , LinearFit(..)
  , LinearType(..)
  , QuickResults(..)
  , SimpleReport(..)
  , SimpleResults(..)
  , Stats(..)
  , TestReport(..)
  , TestSuite(..)
  , UserInputs(..)
  )

-- * Datatypes 

-- ** User inputs 

-- | Pretty printing for the 'UserInputs' data structure. 
-- Prints all fields; invalids are printed last.                                               
docUserInputs :: UserInputs -> PP.Doc 
docUserInputs inps = PP.vcat $ PP.punctuate (PP.text "\n")
  [ PP.text "All module elements"     PP.$$ (PP.nest 2 $ showElems             $ _allElems          inps)
  , PP.text "Valid module elements"   PP.$$ (PP.nest 2 $ showTypeableElems     $ _validElems        inps)
  , PP.text "Nullary functions"       PP.$$ (PP.nest 2 $ showTypeableElems     $ _nullaryFuns       inps)
  , PP.text "Unary functions"         PP.$$ (PP.nest 2 $ showTypeableElems     $ _unaryFuns         inps)
  , PP.text "Binary functions"        PP.$$ (PP.nest 2 $ showTypeableElems     $ _binaryFuns        inps)
  , PP.text "Benchmarkable functions" PP.$$ (PP.nest 2 $ showTypeableElems     $ _benchFuns         inps)
  , PP.text "Arbitrary functions"     PP.$$ (PP.nest 2 $ showTypeableElems     $ _arbFuns           inps)
  , PP.text "NFData functions"        PP.$$ (PP.nest 2 $ showTypeableElems     $ _nfFuns            inps)
  , PP.text "Unary test data"         PP.$$ (PP.nest 2 $ showTypeableElems     $ fmap (sel1 &&& sel2) $ _unaryData  inps) -- Don't print sizing information.
  , PP.text "Binary test data"        PP.$$ (PP.nest 2 $ showTypeableElems     $ fmap (sel1 &&& sel2) $ _binaryData inps) -- Don't print sizing information.
  , PP.text "Test suites"             PP.$$ (PP.nest 2 $ showTestSuites        $ _testSuites        inps)
  -- Invalids come last because they have 'InputError's.
  , PP.text "Invalid module elements" PP.$$ (PP.nest 2 $ showElems             $ _invalidElems      inps) 
  , PP.text "Invalid test data"       PP.$$ (PP.nest 2 $ showInvalidData       $ _invalidData       inps)
  , PP.text "Invalid test suites"     PP.$$ (PP.nest 2 $ showInvalidTestSuites $ _invalidTestSuites inps)
  ]
  
  where 
    -- Pretty printing for @[(ModuleElem, Maybe TypeString)]@.
    showElems :: [(ModuleElem, Maybe TypeString)] -> PP.Doc 
    showElems [] = PP.text "N/A"
    showElems xs = PP.vcat [showDs, showCs, showFs]
      where 
        -- Split into (Fun, Class, Data).
        ((fs, tys), cs, ds) = foldr splitShowModuleElems (([], []), [], []) xs

        -- Data.
        showDs | null ds   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Data"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort ds
                   ]
        -- Class.
        showCs | null cs   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Class"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort cs
                   ]
        -- Fun.
        showFs | null fs   = PP.empty 
               | otherwise = PP.vcat 
                   [ PP.text "Fun"
                   , PP.nest 2 $ PP.vcat $ fmap PP.text $ sort $ 
                       zipWith (\idt ty -> idt ++ " :: " ++ ty) (deggar fs) tys
                   ]

    -- Pretty printing for @[(Id, HsType)]@.
    showTypeableElems :: [(Id, HsType)] -> PP.Doc
    showTypeableElems [] = PP.text "N/A"
    showTypeableElems xs = PP.vcat $ fmap PP.text $ sort $ 
      zipWith (\idt ty -> idt ++ " :: " ++ prettyPrint ty) (deggar idts) tys
      where (idts, tys) = unzip xs

    -- Pretty printing for 'TestSuite's.
    showTestSuites :: [(Id, TestSuite)] -> PP.Doc 
    showTestSuites [] = PP.text "N/A"
    showTestSuites xs = PP.vcat $ fmap (uncurry showTestSuite) $ 
      sortBy (comparing fst) xs
      where 
        showTestSuite :: Id -> TestSuite -> PP.Doc 
        showTestSuite idt ts = PP.vcat 
          [ PP.text idt PP.<+> PP.text ":: TestSuite"
          , PP.nest 2 $ docTestSuite_ProgsDataOpts ts
          ]

    -- Invalids, need additional nesting for input errors: --------------------
    -- Note: don't forget to sort alphabetically. 

    showInvalidData :: [(Id, HsType, [InputError])] -> PP.Doc
    showInvalidData [] = PP.text "N/A"
    showInvalidData xs = PP.vcat $ fmap showInvalidDat $ 
      sortBy (comparing sel1) xs
      where 
        showInvalidDat :: (Id, HsType, [InputError]) -> PP.Doc
        showInvalidDat (idt, ty, errs) = PP.vcat 
          [ PP.text $ idt ++ " :: " ++ prettyPrint ty
          , PP.nest 2 $ PP.vcat $ fmap (PP.text . show) $ 
              sortBy (comparing show) errs 
          ]

    showInvalidTestSuites :: [(Id, [InputError])]  -> PP.Doc 
    showInvalidTestSuites [] = PP.text "N/A"
    showInvalidTestSuites xs = PP.vcat $ fmap showInvalidTestSuite $ 
      sortBy (comparing fst) xs
      where 
        showInvalidTestSuite :: (Id, [InputError]) -> PP.Doc 
        showInvalidTestSuite (idt, errs) = PP.vcat 
          [ PP.text idt PP.<+> PP.text ":: TestSuite"
          , PP.nest 2 $ PP.vcat $ fmap (PP.text . show) $ 
              sortBy (comparing show) errs 
          ]

    -- Helpers:

    -- Split the 'ModuleElem's to display 'Fun' types by the side of 'Fun' 
    -- identifiers. (The 'Class' and 'Data' 'ModuleElem's don't have typing 
    -- information.)
    splitShowModuleElems 
      :: (ModuleElem, Maybe TypeString)
      -> (([String], [String]), [String], [String]) 
      -> (([String], [String]), [String], [String])
    -- Types.
    splitShowModuleElems (Fun idt, Just ty) ((fs, tys), cs, ds) = 
      ((idt : fs, ty : tys), cs, ds)
    -- No types.
    splitShowModuleElems (Fun idt, Nothing) ((fs, tys), cs, ds) = 
      ((idt : fs, "" : tys), cs, ds) -- Shouldn't happen.
    splitShowModuleElems (Class idt _, _) (fs, cs, ds) = (fs, idt : cs, ds)
    splitShowModuleElems (Data idt _, _)  (fs, cs, ds) = (fs, cs, idt : ds)


-- | Pretty print test suites: prints the names of test programs and data
-- options only.
docTestSuite_ProgsDataOpts :: TestSuite -> PP.Doc                                                                        
docTestSuite_ProgsDataOpts ts = PP.vcat 
  [ wrappedList $ fmap PP.text (_progs ts)        -- Names of test programs.
  , docDataOpts (_dataOpts ts) ]                  -- Data options.

-- | Pretty print data size.
docDataSize :: DataSize -> PP.Doc 
docDataSize (SizeUn n)      = PP.text (show n)
docDataSize (SizeBin n1 n2) = hscat ["(", show n1, ", ", show n2, ")"]

-- | Pretty print data options.
docDataOpts :: DataOpts -> PP.Doc 
docDataOpts (Manual idt) = hscat ["Manual ", "\"", idt, "\""]
docDataOpts (Gen l s u)  = hscat 
  ["Random, size range [", show l, ",", show s, "..", show u, "]"]

-- ** Benchmarking 

-- | Pretty print benchmark reports.
docBenchReport :: BenchReport -> PP.Doc 
docBenchReport br = PP.vcat
  [ PP.text "Reports:" PP.$+$ PP.nest 2 (PP.vcat $ fmap ppTestResults $ _reports br)
  , ppBaselines (_baselines br) ]
  
  where 
    -- Pretty print 'SimpleReport's belonging to same test program.
    ppTestResults :: [SimpleReport] -> PP.Doc 
    ppTestResults [] = PP.empty
    ppTestResults srs = PP.vcat $ 
      [ PP.text (_name $ head srs)
      , PP.nest 2 $ PP.vcat $ 
          fmap (\sr -> docDataSizeRuntime (_size sr) (_runtime sr)) srs
      ]

    -- Pretty print baseline measurements.
    ppBaselines :: [SimpleReport] -> PP.Doc
    ppBaselines []  = PP.empty
    ppBaselines bls = PP.text "Baseline measurements:" PP.$+$ PP.nest 2
      (PP.vcat $ fmap (uncurry docDataSizeRuntime . (_size &&& _runtime)) bls)

-- | Pretty print test reports.
docTestReport :: TestReport -> PP.Doc
docTestReport tr = PP.vcat 
  [ PP.text "Test programs:" PP.<+> (wrappedList $ fmap PP.text $ _tProgs tr)
  , PP.text "Data options:" PP.<+> docDataOpts (_tDataOpts tr)
  , PP.text "Normal form:" PP.<+> PP.text (show $ _tNf tr)
  , PP.text "Denotationally equal:" PP.<+> PP.text (show $ _eql tr)
  , docGhcFlags (_tGhcFlags tr)
  , PP.text ""
  , docBenchReport (_br tr)
  ]
  where 
    -- Pretty print list of GHC flags.
    docGhcFlags :: [String] -> PP.Doc 
    docGhcFlags [] = PP.empty
    docGhcFlags flags = PP.sep 
      [PP.text "GHC flags:", PP.nest 1 $ wrappedList $ fmap PP.text $ flags]

-- ** Statistical analysis

-- | Pretty print a list of quick results. The maximum runtime among 
-- all test cases is formatted into seconds/milliseconds/nanoseconds etc. and 
-- the rest of the results are forced into the same units for consistency. 
-- This makes it easier to compare runtimes at a glance of the raw results on 
-- the command line.
docQuickResultss :: [QuickResults] -> PP.Doc 
docQuickResultss qrs = (PP.vcat $ PP.punctuate (PP.text "\n") $ 
  fmap (docQuickResults units) qrs) PP.<> PP.text "\n"
  where 
    maxRuntime = maximum $ fmap (maxFromCoords . _qrRaws) qrs  -- Maximum runtime of all test cases.
    (_, units) = secs maxRuntime                               -- Display all runtimes in the same /units/.

    -- Maximum runtime from a set of coordinates.
    maxFromCoords :: Either [Coord] [Coord3] -> Double 
    maxFromCoords (Left cs)  = maximum (fmap snd cs)
    maxFromCoords (Right cs) = maximum (fmap sel3 cs)



-- | Pretty print quick results. The runtimes of test programs are formatted
-- according the /units/ parameter. See 'forceSecs'. This makes it easier to 
-- compare runtimes as they are all in the same units.
docQuickResults :: String -> QuickResults -> PP.Doc 
docQuickResults units qr = title PP.$$ (PP.nest 2 $ PP.vcat 
  [ 
    PP.text size PP.<+> sizes                        -- Input sizes.
  , PP.text time PP.<+> runtimes PP.<> PP.text "\n"  -- Runtimes.
  , fits                                             -- 'LinearFits'.
  ])

  where 
    -- Side headings with some manual spacing so everything aligns properly.
    title = PP.text (unqualIdt $ _qrIdt qr)                       -- Name of program.
    size   = "Size    " ++ replicate (length sUnits) ' ' ++ " "   -- Input sizes.
    time   = "Time    " ++ sUnits ++ " "                          -- Runtime measurements.
    sUnits  = "(" ++ units ++ ")"                                 -- Forced units.
    
    -- Output input sizes and runtime measurements in a tabular format with
    -- maximum width of ~80.
    (sizes, runtimes) = docCoordsTabular 60 maxWidth units (_qrRaws qr)
    
    -- Pretty print the equations of 'LinearFits'.
    fits :: PP.Doc 
    fits = case _qrFits qr of 
      []   -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> PP.text "N/A"
      [lf] -> PP.text ("Fit" ++ replicate (length units + 8) ' ') 
        PP.<+> docLinearFitsTabular 60 [lf]
      lfs  -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> docLinearFitsTabular 60 lfs

    -- Maximum width of input sizes: to align table columns.
    maxWidth :: Int 
    maxWidth  = case _qrRaws qr of
      Left  cs -> max (length . show . round' . maximum $ fmap fst cs) 7
      Right cs -> max (maximum $ fmap (\(x1, x2, _) -> 
        length (show $ round' x1) + length (show $ round' x2) + 5) cs) 7

-- | Pretty print simple results. The runtimes of test programs are formatted 
-- according the /units/ parameter. See 'forceSecs'. This makes it easier to 
-- compare runtimes as they are all in the same units.
docSimpleResults :: String -> SimpleResults -> PP.Doc 
docSimpleResults units sr = title PP.$$ (PP.nest 2 $ PP.vcat 
  [ PP.text size   PP.<+> sizes    -- Input sizes.
  , PP.text time   PP.<+> runtimes -- Runtimes.
   -- Simple cumulative statistics for all test cases.
  , PP.text stdDev PP.<+> PP.text (forceSecs maxWidth units $ _srStdDev sr)    
  , (PP.text $ "Average variance introduced by outliers: " ++ 
      printf "%d%% (%s)" (round (_srAvgPutVarFrac sr * 100) :: Int) wibble) PP.<> PP.text "\n" 
  , fits -- 'LinearFits'.
  ])

  where 
    -- Side headings with some manual spacing so everything aligns properly.
    title = PP.text (unqualIdt $ _srIdt sr)                       -- Name of program.
    size   = "Size    " ++ replicate (length sUnits) ' ' ++ " "   -- Input sizes.
    time   = "Time    " ++ sUnits ++ " "                          -- Runtime measurements.
    stdDev = "Std dev " ++ sUnits ++ " "                          -- Standard deviation.
    sUnits  = "(" ++ units ++ ")"                                 -- Forced units.
    
    -- Output input sizes and runtime measurements in a tabular format with
    -- maximum width of ~80.
    (sizes, runtimes) = docCoordsTabular 60 maxWidth units (_srRaws sr)
    
    -- Pretty print the equations of 'LinearFits'.
    fits :: PP.Doc 
    fits = case _srFits sr of 
      []   -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> PP.text "N/A"
      [lf] -> PP.text ("Fit" ++ replicate (length units + 8) ' ') 
        PP.<+> docLinearFitsTabular 60 [lf]
      lfs  -> PP.text ("Fits" ++ replicate (length units + 7) ' ') 
        PP.<+> docLinearFitsTabular 60 lfs

    -- Helpers:

    -- Maximum width of input sizes: to align table columns.
    maxWidth :: Int 
    maxWidth  = case (_srRaws sr) of
      Left  cs -> max (length . show . round' . maximum $ fmap fst cs) 7
      Right cs -> max (maximum $ fmap (\(x1, x2, _) -> 
        length (show $ round' x1) + length (show $ round' x2) + 5) cs) 7

    -- Note: taken from Criterion source code.. wibble??
    wibble = case _srAvgOutVarEff sr of
      Unaffected -> "unaffected"
      Slight     -> "slightly inflated"
      Moderate   -> "moderately inflated"
      Severe     -> "severely inflated"


-- | Pretty printing a list of simple results. The maximum runtime among 
-- all test cases is formatted into seconds/milliseconds/nanoseconds etc. and 
-- the rest of the results are forced into the same units for consistency. 
-- This makes it easier to compare runtimes at a glance of the raw results on 
-- the command line.
docSimpleResultss :: [SimpleResults] -> PP.Doc 
docSimpleResultss srs = (PP.vcat $ PP.punctuate (PP.text "\n") $ 
  fmap (docSimpleResults units) srs) PP.<> PP.text "\n" 
  where 
    maxRuntime = maximum $ fmap (maxFromCoords . _srRaws) srs  -- Maximum runtime of all test cases.
    (_, units) = secs maxRuntime                               -- Display all runtimes in the same /units/.

    -- Maximum runtime from a set of coordinates.
    maxFromCoords :: Either [Coord] [Coord3] -> Double 
    maxFromCoords (Left cs)  = maximum (fmap snd cs)
    maxFromCoords (Right cs) = maximum (fmap sel3 cs)

-- | Pretty print simple reports: prints the name of the test program, the
-- size of the test data, and its runtime only. 
docSimpleReport_NameDataSizeRuntime :: SimpleReport -> PP.Doc 
docSimpleReport_NameDataSizeRuntime sr = PP.vcat $
  [ PP.text (_name sr)
  , PP.nest 2 $ docDataSizeRuntime (_size sr) (_runtime sr) ]
    
-- | Pretty print coordinates.
docCoords :: Either [Coord] [Coord3] -> PP.Doc 
docCoords (Left  cs) = PP.vcat $ fmap (\(s, t) -> PP.hcat $ commaSeparate 
  [PP.int $ round s, PP.double t]) cs
docCoords (Right cs) = PP.vcat $ fmap (\(s1, s2, t) -> PP.hcat $ commaSeparate
  [PP.int $ round s1, PP.int $ round s2, PP.double t]) cs

-- | Pretty print linear types.
docLinearType :: LinearType -> PP.Doc 
docLinearType (Poly 0)      = PP.text "constant"
docLinearType (Poly 1)      = PP.text "linear"
docLinearType (Poly 2)      = PP.text "quadratic"
docLinearType (Poly 3)      = PP.text "cubic"
docLinearType (Poly 4)      = PP.text "quartic"
docLinearType (Poly 5)      = PP.text "quintic"
docLinearType (Poly 6)      = PP.text "sextic"
docLinearType (Poly 7)      = PP.text "septic"
docLinearType (Poly 8)      = PP.text "octic"
docLinearType (Poly 9)      = PP.text "nonic"
docLinearType (Poly n)      = hscat ["n", superNum n]
docLinearType (Exp n)       = hscat [show n, "\x207F"]
docLinearType (Log b n)     = hscat ["log", subNum b, superNum n, "n"]
docLinearType (PolyLog b n) = hscat ["n", superNum n, "log", subNum b, superNum n, "n"]

-- | Pretty print all statistics in the 'CVStats' datatype.
docCVStats :: CVStats -> PP.Doc 
docCVStats cvSts = 
  PP.vcat $ zipWith (\n f -> equation n $ printf "%.4g" $ f cvSts) ns fs
  where 
    -- Names of statistics in 'CVStats'.
    ns :: [String]
    ns  = deggar ["MSE", "MAE", "SST", "SSR"]
    -- Record fields in 'CVStats'. 
    -- Note: correspond to the names above.
    fs :: [CVStats -> Double]
    fs  = [_cv_mse, _cv_mae, _cv_ss_tot, _cv_ss_res]

-- | Pretty print all statistics in the 'Stats' datatype.
docStats :: Stats -> PP.Doc
docStats sts = 
  PP.vcat $ zipWith (\n f -> equation n $ printf "%.4g" $ f sts) ns fs
  where 
    -- Names of statistics in 'Stats'.
    ns :: [String]
    ns  = deggar [ "PMSE", "PMAE", "SST", "PRESS", "R\x00B2", "Adj. R\x00B2"
                 , "Pred. R\x00B2", "BIC", "AIC", "CP" ]
    -- Record fields in 'Stats'. 
    -- Note: correspond to the names above.
    fs :: [Stats -> Double]
    fs  = [ _p_mse, _p_mae, _ss_tot, _p_ss_res, _r2, _a_r2, _p_r2, _bic
          , _aic, _cp ]

-- * Utilities 

-- ** Bullet points 

-- | Add an arrow bullet point to a string.
arrowBullet :: String -> PP.Doc
arrowBullet s = PP.char '\9656' PP.<+> PP.text s

-- | Add a circle bullet point to a string.
circleBullet :: String -> PP.Doc 
circleBullet s = PP.char '\8226' PP.<+> PP.text s

-- ** Fonts 

-- | Add italic Unicode formatting.
italic :: String -> PP.Doc 
italic s = PP.hcat 
  [PP.zeroWidthText "\ESC[3m", PP.text s, PP.zeroWidthText "\ESC[0m"]

-- ** Formatting 

-- | Comma separate a list of 'PP.Doc's.
commaSeparate :: [PP.Doc] -> [PP.Doc]
commaSeparate  = PP.punctuate (PP.char ',')

-- | An equation (LHS = RHS).
equation :: String -> String -> PP.Doc 
equation lhs rhs = PP.hsep [PP.text lhs, PP.char '=', PP.text rhs]

-- | 'PP.hcat' for lists of strings.
hscat :: [String] -> PP.Doc
hscat  = PP.hcat . fmap PP.text

-- | Wrapped, comma-separated list.
wrappedList :: [PP.Doc] -> PP.Doc 
wrappedList  = PP.fcat . PP.punctuate (PP.text ", ")

-- ** Shapes

-- | A cross Unicode character.
cross :: PP.Doc 
cross  = PP.char '\10007'

-- | A question mark Unicode character.
questionMark :: PP.Doc 
questionMark  = PP.char '\63'

-- | A tick Unicode character.
tick :: PP.Doc
tick  = PP.char '\10004'

-- ** Style

-- | Create a 'PP.Style' with a given line width.
lineStyle :: Int -> PP.Style 
lineStyle n = PP.style { PP.lineLength = n }

-- * Helpers 

-- | Pretty print coordinates in tabular format of a given width. It is 
-- slightly involved because multiple rows are required for both input sizes 
-- and runtimes, so stack them. Force runtimes to be the given units.
docCoordsTabular 
  :: Int                        -- Table width.
  -> Int                        -- Width of each column.
  -> String                     -- Runtime units.
  -> Either [Coord] [Coord3]    -- Measurements.
  -> (PP.Doc, PP.Doc)
docCoordsTabular maxWidth width units (Left cs) = 
  (PP.vcat $ chunks xs, PP.vcat $ chunks ys)
  where 
    xs     = fmap (PP.text . printf ("%-" ++ show width ++ "s") . show . 
               round' . fst) cs' 
    ys     = fmap (PP.text . forceSecs width units . snd) cs'
    cs'    = sort cs
    chunks = hsepChunks maxWidth width
-- Input sizes for 'Coord3's are printed as tuples.
docCoordsTabular maxWidth width units (Right cs) = 
  (PP.vcat $ chunks xs, PP.vcat $ chunks ys')
  where 
    (xs1, xs2, ys) = unzip3 cs'
    xs     = zipWith (\x1 x2 -> PP.text $ printf ("%-" ++ show width ++ "s") $ 
               show $ PP.char '(' PP.<> PP.int (round x1) PP.<> PP.char ',' 
               PP.<+> PP.int (round x2) PP.<> PP.char ')') xs1 xs2
    ys'    = fmap (PP.text . forceSecs width units) ys
    cs'    = sort cs
    chunks = hsepChunks maxWidth width

 -- | Pretty print the equations of linear fits in a tabular layout of a given 
-- width.
docLinearFitsTabular :: Int -> [LinearFit] -> PP.Doc 
docLinearFitsTabular _ [] = PP.empty 
docLinearFitsTabular width lfs = 
  PP.vcat $ fmap ((PP.text "y =" PP.<+>) . wrapDocExpr (width - 5) . _ex) lfs -- Subtract 5 width for "y = ".

-- | Pretty print data sizes and runtimes in n-tuples.
docDataSizeRuntime :: DataSize -> Double -> PP.Doc
docDataSizeRuntime (SizeUn n)      d = hscat ["(", show n, ", ", show d, ")"]
docDataSizeRuntime (SizeBin n1 n2) d = hscat ["(", show n1, ", ", show n2, ", ", show d, ")"]

-- | Just for typing information.
round' :: Double -> Int 
round'  = round

-- | Chunk off rows into maxWidth/width columns and then display chunks 
-- horizontally. 
hsepChunks :: Int -> Int -> [PP.Doc] -> [PP.Doc]
hsepChunks maxWidth width = fmap PP.hsep . chunksOf (maxWidth `div` width)






-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- * To Fix

-- | Pretty printing for 'Improvement's.
docImprovement :: Bool -> Improvement -> PP.Doc 
docImprovement b (idt1, ord, idt2, d) = 
  PP.hsep $ docImprovement' b (unqualIdt idt1, ord, unqualIdt idt2, d)


-- | Pretty printing helper for 'Improvement's.
docImprovement' :: Bool -> Improvement -> [PP.Doc] 
docImprovement' b (idt1, LT, idt2, d) = docImprovement' b (idt2, GT, idt1, d)
docImprovement' True  (idt1, EQ, idt2, d) = 
  [ PP.text idt1, PP.text "\8804\8805", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]
docImprovement' False (idt1, EQ, idt2, d) =  
  [ PP.text idt1, PP.text "\8818\8819", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]
docImprovement' True  (idt1, GT, idt2, d) = 
  [ PP.text idt1, PP.text "\8805", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]
docImprovement' False (idt1, GT, idt2, d) = 
  [ PP.text idt1, PP.text "\8819", PP.text idt2
  , PP.char '(' PP.<> (PP.text $ printf "%.2f" d) PP.<> PP.char ')' ]

-- | Pretty printing for a list of 'Improvement's.
showImprovements :: Bool -> [Improvement] -> String 
showImprovements b imps = bySide (fmap PP.vcat $ transpose docImps) " "
  where 
    imps' = fmap (\(idt1, ord, idt2, d) -> 
      (unqualIdt idt1, ord, unqualIdt idt2, d)) imps
    docImps = fmap (docImprovement' b) imps'

-- | Pretty printing function for the 'Expr' datatype. Note: this is specialised
-- to handle the kind of 'Expr's used by the system. It is /not/ a generic
-- pretty printing function for all possible 'Expr's. Note: wraps equations to 
-- a given width.
wrapDocExpr :: Int -> E.Expr Double -> PP.Doc 
wrapDocExpr width expr = 
  wrapPPList (max width $ maximum $ fmap length docs) " " docs
  where 
    (p : ps) = listDocExpr expr
    docs = fmap PP.render $ uncurry signDoc' p : fmap (uncurry signDoc) ps

    -- Add signs in front of each term: True := '+', False := '-'.
    
    -- Special case for first term as no spacing between '-' and term and no
    -- '+' sign.
    signDoc' :: Bool -> PP.Doc -> PP.Doc 
    signDoc' False  doc = doc
    signDoc' True doc = PP.char '-' PP.<> doc
    
    -- Normal spacing for rest of terms.
    signDoc :: Bool -> PP.Doc -> PP.Doc 
    signDoc False  doc = PP.char '+' PP.<+> doc
    signDoc True doc = PP.char '-' PP.<+> doc

    -- List all expression terms and whether they contain a negation.
    listDocExpr :: E.Expr Double -> [(Bool, PP.Doc)]             
    listDocExpr (E.Num n) = [(False, PP.text (printf "%.2g" n))]
    listDocExpr (E.Add ex1 ex2) = reverse $ fmap docExpr' $ 
      collectAdds ex2 [] ++ [ex1]
      where 
        collectAdds (E.Add e1 e2) es = collectAdds e2 (e1 : es)
        collectAdds e             es = e : es 
    listDocExpr x = [(False, PP.text $ show x)]

  -- * Helpers 

-- | Pretty printing 'Expr' datatype, returns the 'PP.Doc' for a term and
-- whether it contains a negation.
docExpr' :: E.Expr Double -> (Bool, PP.Doc)
docExpr' (E.Var x) = (False, PP.text x)
docExpr' (E.Num d)
  | d < 0     = (True, PP.text $ printf "%.2g" (-d)) -- Record the negation.
  | otherwise = (False, PP.text$ printf "%.2g" d)
docExpr' (E.Mul e1 e2) = (b1 `xor` b2, s1 PP.<> s2) -- Shouldn't be the case that both are negations, but just in case.
  where 
    (b1, s1) = docExpr' e1 
    (b2, s2) = docExpr' e2
docExpr' (E.Log (E.Num b) e) = (b1, PP.text "log" PP.<> PP.text (subNum b) PP.<> 
  PP.char '(' PP.<> s PP.<> PP.char ')')
  where (b1, s) = docExpr' e
docExpr' (E.Pow (E.Log (E.Num b) e) (E.Num p)) = (b1, PP.text "log" PP.<> 
  PP.text (subNum b) PP.<> PP.text (superNum p) PP.<> PP.char '(' PP.<> s 
  PP.<> PP.char ')')
  where (b1, s) = docExpr' e
docExpr' (E.Pow e (E.Num p)) = (b, s PP.<> PP.text (superNum p))
  where (b, s) = docExpr' e
docExpr' (E.Pow (E.Num b) (E.Var _)) = (False, PP.text " *" PP.<+> PP.int (round b) 
  PP.<> PP.text "\x02E3")
docExpr' x = (False, PP.text $ show x)