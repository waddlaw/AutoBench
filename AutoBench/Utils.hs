
{-# OPTIONS_GHC -Wall #-} 

{-|

  Module      : AutoBench.Utils
  Description : General-purpose helper functions.
  Copyright   : (c) 2018 Martin Handley
  License     : BSD-style
  Maintainer  : martin.handley@nottingham.ac.uk
  Stability   : Experimental
  Portability : GHC

  General-purpose helper functions used throughout AutoBench's implementation.

-}

{-
   ----------------------------------------------------------------------------
   <TO-DO>:
   ----------------------------------------------------------------------------
   - 
-}

module AutoBench.Utils
  (

  -- * AutoBench specific
    filepathToModuleName -- Convert a filepath to a module name.
  , genIdts              -- Generate identifiers for unnamed test programs.
  -- * Lists 
  , notNull              -- not . null.
  , allEq                -- Check whether all elements in a list are equal.
  , uniqPairs            -- Unique pairs from a list.
  -- * Tuples
  , (**>)                -- Convert a list of 2-tuples to a list of 3-tuples by applying a function to the values in each 2-tuple.
  , (==>)                -- Convert a list of 2-tuples to a list of 3-tuples by placing a constant element at the end of each 2-tuple.
  -- * Formatting
  , subNum               -- Subscripts for 0-9, then "_n". 
  , superNum             -- Superscripts for 0-9, then "^n".
  , strip                -- Strip surrounding whitespace from a string.
  -- * Parsing primitives 
  , Parser               -- Basic Megaparsec parsing type.
  , integer              -- Basic 'Int' parser.
  , lexeme               -- Basic lexeme parser.
  , sc                   -- Basic space consumer.
  , symbol               -- Basic symbol parser.
  -- ** Command line parsing
  , CLArgs(..)           -- Command line arguments.
  , clArgsParser         -- Options parser for command line arguments.

  -- * Misc 
  , (.*)                 -- Generalised function composition.

  ) where 

import           Control.Applicative          ((<*>))
import           Data.Char                    (isSpace, toUpper)
import           Data.List                    (dropWhileEnd, tails)
import           Data.Monoid                  ((<>))
import           Data.Void                    (Void)
import           Language.Haskell.Interpreter (ModuleName)
import qualified Options.Applicative          as OPTS
import           System.FilePath.Posix        (isValid, makeValid, takeBaseName) 
import qualified Text.Megaparsec              as MP
import qualified Text.Megaparsec.Char         as MP 
import qualified Text.Megaparsec.Char.Lexer   as L 



-- * AutoBench specific

-- | Convert a filepath to a module name.
--
-- > fpTfilePathToModuleNameoModuleName "some/filepath/Input.hs" = "Input"
filepathToModuleName :: FilePath -> ModuleName
filepathToModuleName fp = let (c : cs) = takeBaseName fp in toUpper c : cs

-- | Generate identifier for unnamed test programs.
--
-- > genIdts = ["P1", "P2", "P3", ...]
genIdts :: [String]
genIdts  = fmap (\n -> 'P' : show (n :: Int)) [1..]

-- * Lists 

-- | Noone has time for @not . null@.
--
-- > notNull = not . null
notNull :: [a] -> Bool 
notNull  = not . null

-- | Check whether all elements in a list are equal.
allEq :: Eq a => [a] -> Bool
allEq []       = True
allEq [_]      = True
allEq (x : xs) = all (== x) xs

-- | All unique pairs from a list. 
uniqPairs :: [a] -> [(a, a)]
uniqPairs xs = [ (l, r) | (l : ys) <- tails xs, r <- ys ]

-- * Tuples

-- Convert a list of 2-tuples to a list of 3-tuples by applying a function to 
-- the values in each 2-tuple.
(**>) :: (a -> b -> c) -> [(a, b)] -> [(a, b, c)]
f **> ts = fmap (uncurry (,,) <*> uncurry f) ts

-- Convert a list of 2-tuples to a list of 3-tuples by placing a constant 
-- element at the end of each 2-tuple.
(==>) :: c -> [(a, b)] -> [(a, b, c)]
x ==> ts = (\_ _ -> x) **> ts

-- * Formatting 

-- | Strip surrounding whitespace from a string.
--
-- > strip "  bacon  " = "bacon"
strip :: String -> String
strip  = dropWhileEnd isSpace . dropWhile isSpace

-- | Unicode subscripts for 0-9, then "_n". 
subNum :: (Num a, Eq a, Show a) => a -> String
subNum 0 = "\x2080"
subNum 1 = "\x2081"
subNum 2 = "\x2082"
subNum 3 = "\x2083"
subNum 4 = "\x2084"
subNum 5 = "\x2085"
subNum 6 = "\x2086"
subNum 7 = "\x2087"
subNum 8 = "\x2088"
subNum 9 = "\x2089"
subNum n = "_" ++ show n

-- | Unicode superscripts for 0-9, then "^n".
superNum :: (Num a, Eq a, Show a) => a -> String
superNum 1 = ""
superNum 2 = "\x00B2"
superNum 3 = "\x00B3"
superNum 4 = "\x2074"
superNum 5 = "\x2075"
superNum 6 = "\x2076"
superNum 7 = "\x2077"
superNum 8 = "\x2078"
superNum 9 = "\x2079"
superNum n = "^" ++ show n

-- * Parsing

-- | Basic Megaparsec parsing type.
type Parser = MP.Parsec Void String

-- | Basic space consumer.
sc :: Parser ()
sc = L.space MP.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | Basic symbol parser.
symbol :: String -> Parser String
symbol  = L.symbol sc

-- | Basic lexeme parser.
lexeme :: Parser a -> Parser a
lexeme  = L.lexeme sc

-- | Basic 'Int' parser.
integer :: Parser Int
integer  = lexeme L.decimal

-- * Command line parsing

-- | AutoBench's command line arguments.
data CLArgs = 
  CLArgs
    {
      _userInputFile :: FilePath  -- ^ User input file.
    }

-- | Options parser for AutoBench's command line arguments.
clArgsParser :: OPTS.ParserInfo CLArgs
clArgsParser  = CLArgs <$> OPTS.info 
  userInputFile
  (OPTS.header "AutoBench (Version 0.1)") 
  where 
    -- Only argument so far is the user input file.
    userInputFile :: OPTS.Parser FilePath
    userInputFile  = OPTS.argument (OPTS.str >>= readFilepath) 
      (OPTS.metavar "FILEPATH" <> OPTS.help "User input file")

    -- Do some basic checking upfront.
    readFilepath :: String -> OPTS.ReadM FilePath
    readFilepath s 
      | isValid s' = return s'
      | otherwise = OPTS.readerError ("Invalid filepath: " ++ show s)
      where s' = makeValid s

-- * Misc.

-- Generalised function composition.
infixr 8 .*
(.*) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.*)  = fmap . fmap 