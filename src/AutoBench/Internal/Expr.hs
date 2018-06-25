
{-# OPTIONS_GHC -Wall #-} 

-- |
--
-- Module      : AutoBench.Internal.Expr
-- Description : Mathematical expressions for regression analysis
-- Copyright   : (c) 2018 Martin Handley
-- License     : BSD-style
-- Maintainer  : martin.handley@nottingham.ac.uk
-- Stability   : Experimental
-- Portability : GHC
--
-- The 'Expr' datatype is used to represent the equations of regression models.
-- 

-------------------------------------------------------------------------------
-- <TO-DO>:
-------------------------------------------------------------------------------

module AutoBench.Internal.Expr (Expr(..)) where

-- | Mathematical expressions used to represent the equations of regression 
-- models.
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