module Sexp where

import Control.Applicative
import Control.Monad

import TinyMPC
import qualified Parsers as P

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find repl s =
    if take (length find) s == find
        then repl ++ replace find repl (drop (length find) s)
        else head s : replace find repl (tail s)

lexer :: String -> String
lexer = replace "(" " ( " . replace ")" " ) "

lparen, rparen :: Parser String
lparen = P.spaceSurrounded $ P.string "("
rparen = P.spaceSurrounded $ P.string ")"
