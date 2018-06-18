
{-# OPTIONS_GHC -Wall   #-}
{-# LANGUAGE LambdaCase #-}

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
    arrowBullet
  , circleBullet
  -- * Fonts
  , toItalic
  -- * Shapes 
  , cross
  , questionMark
  , tick
  -- * Style
  , lineStyle

  ) where 

import qualified Text.PrettyPrint.HughesPJ as PP


-- * Bullet points 

arrowBullet :: String -> PP.Doc
arrowBullet s = PP.char '\9656' PP.<+> PP.text s

circleBullet :: String -> PP.Doc 
circleBullet s = PP.char '\8226' PP.<+> PP.text s

-- * Fonts 

toItalic :: String -> PP.Doc 
toItalic s = PP.zeroWidthText "\ESC[3m" PP.<> PP.text s PP.<> PP.zeroWidthText "\ESC[0m"

-- * Shapes

cross :: PP.Doc 
cross  = PP.char '\10007'

questionMark :: PP.Doc 
questionMark  = PP.char '\63'

tick :: PP.Doc
tick  = PP.char '\10004'

-- * Style

lineStyle :: Int -> PP.Style 
lineStyle n = PP.style { PP.lineLength = n }