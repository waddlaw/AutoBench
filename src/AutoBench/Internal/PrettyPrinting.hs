
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
-- This module handles all pretty printing.
--

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------

module AutoBench.Internal.PrettyPrinting
  (

  -- * Bullet points 
    arrowBullet           -- Add an arrow bullet point to a string.
  , circleBullet          -- Add a circle bullet point to a string.
  -- * Fonts
  , italic                -- Add italic Unicode formatting.
  -- * Formatting 
  , equation              -- An equation (LHS = RHS).
  , hscat                 -- 'PP.hcat' for lists of strings.
  , wrappedList           -- Wrapped, comma-separated list.
  -- * Shapes 
  , cross                 -- A cross Unicode character.
  , questionMark          -- A question mark Unicode character.
  , tick                  -- A tick Unicode character.
  -- * Statistical analysis 
  , docLinearType         -- Pretty print linear types.
  , docStats              -- Pretty print all the statistics in the 'Stats' datatype. 
  -- * Style
  , lineStyle             -- Create a 'PP.Style' with a given line width.
  -- * User Inputs    
  , docDataOpts           -- Pretty print data options.

  ) where 

import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.Printf               (printf)

import AutoBench.Internal.Types (DataOpts(..), LinearType(..), Stats(..))
import AutoBench.Internal.Utils (deggar, subNum, superNum)


-- * Bullet points 

-- | Add an arrow bullet point to a string.
arrowBullet :: String -> PP.Doc
arrowBullet s = PP.char '\9656' PP.<+> PP.text s

-- | Add a circle bullet point to a string.
circleBullet :: String -> PP.Doc 
circleBullet s = PP.char '\8226' PP.<+> PP.text s

-- * Fonts 

-- | Add italic Unicode formatting.
italic :: String -> PP.Doc 
italic s = PP.zeroWidthText "\ESC[3m" PP.<> PP.text s PP.<> PP.zeroWidthText "\ESC[0m"

-- * Formatting 

-- | An equation (LHS = RHS).
equation :: String -> String -> PP.Doc 
equation lhs rhs = PP.text lhs PP.<+> PP.char '=' PP.<+> PP.text rhs

-- | 'PP.hcat' for lists of strings.
hscat :: [String] -> PP.Doc
hscat  = PP.hcat . fmap PP.text

-- | Wrapped, comma-separated list.
wrappedList :: [PP.Doc] -> PP.Doc 
wrappedList  = PP.fcat . PP.punctuate (PP.text ", ")

-- * Shapes

-- | A cross Unicode character.
cross :: PP.Doc 
cross  = PP.char '\10007'

-- | A question mark Unicode character.
questionMark :: PP.Doc 
questionMark  = PP.char '\63'

-- | A tick Unicode character.
tick :: PP.Doc
tick  = PP.char '\10004'

-- * Statistical analysis

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

-- | Pretty print all the statistics in the 'Stats' datatype.
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

-- * Style

-- | Create a 'PP.Style' with a given line width.
lineStyle :: Int -> PP.Style 
lineStyle n = PP.style { PP.lineLength = n }

-- * User inputs 

-- | Pretty print data options.
docDataOpts :: DataOpts -> PP.Doc 
docDataOpts (Manual idt) = hscat ["Manual ", '"', idt, '"']
docDataOpts (Gen l s u)  = hscat 
  ["Random, size range [", show l, ",", show s, "..", show u, "]"]