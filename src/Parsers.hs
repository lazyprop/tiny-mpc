module Parsers where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Text.Printf


import TinyMPC

fail :: String -> Parser a
fail e = Parser $ \_ -> Left e
    
any :: Parser Char
any = Parser go
  where
      go []     = Left "any: end of file"
      go (x:xs) = Right (x, xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- Parsers.any
    if f c
    then return c
    else Parsers.fail "does not satisfy"


char :: Char -> Parser Char
char c = satisfy (c ==) <?> printf "expected: `%c`" c

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s) <?> printf "expected one of: `%s`" s

eof :: Parser ()
eof = Parser $ \s ->
    if null s
    then Right ((), [])
    else Left "expected eof"


digit :: Parser Char
digit = satisfy isDigit <?> "expected digit"

operator :: Parser Char
operator = oneOf "+-*/"

spaces :: Parser String
spaces = many $ oneOf " \n\r"

spaceSurrounded :: Parser a -> Parser a
spaceSurrounded p = spaces *> p <* spaces

ignoreSpaces :: [Parser a] -> Parser [a]
ignoreSpaces = mapM spaceSurrounded

expr :: Parser String
expr = concat <$> sequence [ ignoreSpaces [digit]
                           , concat <$> some (ignoreSpaces [operator, digit])
                           ]


