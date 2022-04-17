module Parsers where

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
char c = satisfy (c ==)

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)


