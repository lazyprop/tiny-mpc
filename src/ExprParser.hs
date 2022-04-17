module ExprParser where

import Data.Char (isDigit, digitToInt)

import           TinyMPC (Parser)
import qualified Parsers as P

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show, Eq)

opParser :: Parser Op
opParser = do
    c <- P.any
    case c of
      '+' -> return Add
      '-' -> return Sub
      '*' -> return Mul
      '/' -> return Sub
      _   -> P.fail "not a valid operation"


digParser :: Parser Int
digParser = digitToInt <$> P.satisfy isDigit


type Expr = (Int, Op, Int)

exprParser :: Parser Expr
exprParser = do
    dig1 <- digParser
    op <- opParser
    dig2 <- digParser
    return (dig1, op, dig2)
