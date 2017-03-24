module Parsers where

import Data.Char
import Control.Applicative
import Control.Monad

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap f p = P (\inp -> case parse p inp of
                            []         -> []
                            [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return value = P (\inp -> [(value, inp)])
  p >>= f      = P (\inp -> case parse p inp of
                            []         -> []
                            [(v, out)] -> parse (f v) out)

instance Alternative Parser where 
  (<|>) = mplus 
  empty = mzero

instance MonadPlus Parser where
  mzero       = P (\inp -> [])
  p `mplus` q = P (\inp -> case parse p inp of
                            []         -> parse q inp
                            [(v, out)] -> [(v, out)])

parse :: Parser a -> String -> [(a, String)]
parse (P f) s = f s

item :: Parser Char
item = P (item')
  where item' []     = []
        item' (x:xs) = [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat pred = do c <- item
              if pred c then return c else empty

char :: Char -> Parser Char
char c = sat (==c)

string :: [Char] -> Parser String
string []    = return []
string (c:s) = do char c
                  string s
                  return (c:s)

digit :: Parser Char
digit = sat (and . ap [('0'<=), (<='9')] . pure)
lower :: Parser Char
lower = sat (and . ap [('a'<=), (<='z')] . pure)
upper :: Parser Char
upper = sat (and . ap [('A'<=), (<='Z')] . pure)

letter :: Parser Char
letter = lower <|> upper
alphanum :: Parser Char
alphanum = letter <|> digit


