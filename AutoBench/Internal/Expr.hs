
{-# OPTIONS_GHC -Wall #-} 

{-|

  Module      : AutoBench.Internal.Expr
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

module AutoBench.Internal.Expr
  (
    Expr(..)
  , docExpr


  ) where

import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.Printf (PrintfArg, printf)

import AutoBench.Internal.Utils (subNum, superNum)

-- | Mathematical expressions used for regression analysis.
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

-- | Pretty printing function for the 'Expr' datatype. Note this is specialised
-- to handle the kind of 'Expr's used by the system. It is /not/ a generic
-- pretty printing function for all possible 'Expr's.
docExpr :: (Num a, Ord a, PrintfArg a, Show a) => Expr a -> PP.Doc 
docExpr (Num n) = PP.text (printf "%.2g" n)
docExpr (Add ex1 ex2) = 
   foldr addTerms PP.empty $ reverse $ fmap doc $ collectAdds ex2 [] ++ [ex1]

  where 
    -- If the term contains a negation, then 'add' using " - ", otherwise
    -- use " + ".
    addTerms (False, s) acc
      | acc == PP.empty = s
      | otherwise       = acc PP.<+> PP.char '+' PP.<+> s
    addTerms (True, s) acc
      | acc == PP.empty = PP.char '-' PP.<> s
      | otherwise       = acc PP.<+> PP.char '-' PP.<+> s

    collectAdds (Add e1 e2) es = collectAdds e2 (e1 : es)
    collectAdds e           es = e : es

    doc :: (Num a, Ord a, PrintfArg a, Show a) => Expr a -> (Bool, PP.Doc)
    doc (Var x) = (False, PP.text x)
    doc (Num d)
      | d < 0     = (True, PP.text $ printf "%.2g" (-d)) -- Record the negation.
      | otherwise = (False, PP.text$ printf "%.2g" d)
    doc (Mul e1 e2) = (b1 || b2, s1 PP.<> s2)
      where 
        (b1, s1) = doc e1 
        (b2, s2) = doc e2
    doc (Log (Num b) e) = (b1, PP.text "log" PP.<> PP.text (subNum b) PP.<> 
      PP.char '(' PP.<> s PP.<> PP.char ')')
      where (b1, s) = doc e
    doc (Pow (Log (Num b) e) (Num p)) = (b1, PP.text "log" PP.<> 
      PP.text (subNum b) PP.<> PP.text (superNum p) PP.<> PP.char '(' PP.<> s 
      PP.<> PP.char ')')
      where (b1, s) = doc e
    doc (Pow e (Num p)) = (b, s PP.<> PP.text (superNum p))
      where (b, s) = doc e
    doc x = (False, PP.text $ show x)
docExpr e = PP.text (show e)