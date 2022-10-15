module Sexp where

import Control.Applicative
import Control.Monad

import TinyMPC
import qualified Parsers as P

lparen, rparen :: Parser Char
lparen = P.char '('
rparen = P.char ')'

atom :: Parser String
atom = some (P.letter <|> P.digit <|> P.char '-')

sexp :: Parser [String]
sexp = do
    lp <- plift lparen
    atoms <- many ((P.spaces *> atom) <|> (concat <$> sexp))
    rp <- plift rparen
    return (lp : (atoms ++ [rp]))
