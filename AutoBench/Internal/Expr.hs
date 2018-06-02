
{-# OPTIONS_GHC -Wall #-} 

{-|

  Module      : AutoBench.Internal.Expr
  Description : Mathematical expressions used for regression analysis.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  This module defines the type of mathematical expressions used for regression 
  analysis and associated pretty printing functions that are compatible 
  /with the needs of the system/. Warning: the pretty printing functions 
  defined here are /not/ generic for the 'Expr' datatype.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   -
-}

module AutoBench.Internal.Expr
  (

    Expr(..)      -- Mathematical expressions used for regression analysis.
  , docExpr       -- The system's pretty printing function for the 'Expr' datatype. Warning: not generic.
  , wrapDocExpr   -- The system's wrapped pretty printing function for the 'Expr' datatype. Warning: not generic.

  ) where

import           Data.Bits                 (xor)
import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.Printf               (printf)

import AutoBench.Internal.Utils (subNum, superNum, wrapPPList)

-- | Mathematical expressions used for regression analysis.
-- Note: expressions used by the system are either literals ('Num's) or 
-- additions ('Add's) of multiplications ('Mul's) of expressions ('Expr's).
data Expr a = 
    Num a
  | Var String
  | Exp (Expr a)
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  | Log (Expr a) (Expr a) 
    deriving Show 

-- | Pretty printing function for the 'Expr' datatype. Note: this is specialised
-- to handle the kind of 'Expr's used by the system. It is /not/ a generic
-- pretty printing function for all possible 'Expr's.
docExpr :: Expr Double -> PP.Doc 
docExpr (Num n) = PP.text (printf "%.2g" n)
docExpr (Add ex1 ex2) = 
   foldr addTerms PP.empty $ reverse $ fmap docExpr' $ collectAdds ex2 [] ++ [ex1]
  where 
    -- If the term contains a negation, then 'add' using " - ", otherwise
    -- use " + ".
    addTerms (False, s) acc   -- Note: False := '+', True := '-'.
      | acc == PP.empty = s
      | otherwise       = acc PP.<+> PP.char '+' PP.<+> s
    addTerms (True, s) acc
      | acc == PP.empty = PP.char '-' PP.<> s
      | otherwise       = acc PP.<+> PP.char '-' PP.<+> s

    collectAdds (Add e1 e2) es = collectAdds e2 (e1 : es)
    collectAdds e           es = e : es
docExpr e = PP.text (show e)

-- | Pretty printing function for the 'Expr' datatype. Note: this is specialised
-- to handle the kind of 'Expr's used by the system. It is /not/ a generic
-- pretty printing function for all possible 'Expr's. Note: wraps equations to 
-- a given width.
wrapDocExpr :: Int -> Expr Double -> PP.Doc 
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
    listDocExpr :: Expr Double -> [(Bool, PP.Doc)]             
    listDocExpr (Num n) = [(False, PP.text (printf "%.2g" n))]
    listDocExpr (Add ex1 ex2) = reverse $ fmap docExpr' $ 
      collectAdds ex2 [] ++ [ex1]
      where 
        collectAdds (Add e1 e2) es = collectAdds e2 (e1 : es)
        collectAdds e           es = e : es 
    listDocExpr x = [(False, PP.text $ show x)]

  -- * Helpers 

-- | Pretty printing 'Expr' datatype, returns the 'PP.Doc' for a term and
-- whether it contains a negation.
docExpr' :: Expr Double -> (Bool, PP.Doc)
docExpr' (Var x) = (False, PP.text x)
docExpr' (Num d)
  | d < 0     = (True, PP.text $ printf "%.2g" (-d)) -- Record the negation.
  | otherwise = (False, PP.text$ printf "%.2g" d)
docExpr' (Mul e1 e2) = (b1 `xor` b2, s1 PP.<> s2) -- Shouldn't be the case that both are negations, but just in case.
  where 
    (b1, s1) = docExpr' e1 
    (b2, s2) = docExpr' e2
docExpr' (Log (Num b) e) = (b1, PP.text "log" PP.<> PP.text (subNum b) PP.<> 
  PP.char '(' PP.<> s PP.<> PP.char ')')
  where (b1, s) = docExpr' e
docExpr' (Pow (Log (Num b) e) (Num p)) = (b1, PP.text "log" PP.<> 
  PP.text (subNum b) PP.<> PP.text (superNum p) PP.<> PP.char '(' PP.<> s 
  PP.<> PP.char ')')
  where (b1, s) = docExpr' e
docExpr' (Pow e (Num p)) = (b, s PP.<> PP.text (superNum p))
  where (b, s) = docExpr' e
docExpr' (Pow (Num b) (Var _)) = (False, PP.text " *" PP.<+> PP.int (round b) 
  PP.<> PP.text "\x02E3")
docExpr' x = (False, PP.text $ show x)