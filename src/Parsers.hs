module Parsers where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isAlpha)
import Data.Foldable
import Text.Printf


import TinyMPC

fail :: Parser a
fail = Parser $ \_ -> []
    
any :: Parser Char
any = Parser go
  where
      go []     = []
      go (x:xs) = [(x, xs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- Parsers.any
    if f c
    then return c
    else Parsers.fail


char :: Char -> Parser Char
char c = satisfy (c ==)

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

operator :: Parser Char
operator = oneOf "+-*/"

space :: Parser Char
space = oneOf " \n\r"

spaces :: Parser String
spaces = many $ oneOf " \n\r"

natural :: Parser String
natural = some digit

notWhiteSpace :: Parser Char
notWhiteSpace = satisfy $ not . (`elem` " \n\r\t")

token :: Parser String
token = spaces *> some notWhiteSpace <* spaces

between :: Parser b -> Parser c -> Parser a -> Parser a
between l r m = l *> m <* r

string :: String -> Parser String
string "" = return ""
string (x:xs) = Parsers.char x >> Parsers.string xs >> return (x:xs)

