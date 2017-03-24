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
digit = sat (overAll [('0'<=), (<='9')])
lower :: Parser Char
lower = sat (overAll [('a'<=), (<='z')])
upper :: Parser Char
upper = sat (overAll [('A'<=), (<='Z')])

overAll :: [(a -> Bool)] -> a -> Bool
overAll preds = and . ap preds . pure

letter :: Parser Char
letter = lower <|> upper
alphanum :: Parser Char
alphanum = letter <|> digit

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- manyP p
             return (v:vs)

manyP :: Parser a -> Parser [a]
manyP p = many1 p <|> return []

word :: Parser String
word = many alphanum

nat :: Parser Int 
nat = do d <- many1 digit
         return (read d :: Int)

int :: Parser Int 
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do v <- p
                    do sep
                       vs <- p `sepBy` sep
                       return (v:vs)
                     <|> return ([v])
                    

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = p `sepBy1` sep <|> return []

