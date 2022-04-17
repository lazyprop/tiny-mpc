module ExprParser where

import Data.Char (isDigit, digitToInt)

import           TinyMPC (Parser)
import qualified Parsers as P

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show, Eq)


instance Read Op where
    readsPrec _ s
      | s == "+"  = [(Add, "")]
      | s == "-"  = [(Sub, "")]
      | s == "*"  = [(Mul, "")]
      | s == "/"  = [(Div, "")]
      | otherwise = []


charToOp :: Char -> Op
charToOp c
  | c == '+'  = Add
  | c == '-'  = Sub
  | c == '*'  = Mul
  | otherwise = Div


opParser :: Parser Op
opParser = charToOp <$> P.oneOf "+-*/"


digParser :: Parser Int
digParser = digitToInt <$> P.satisfy isDigit


type Expr = (Int, Op, Int)

exprParser :: Parser Expr
exprParser = do
    dig1 <- digParser
    op <- opParser
    dig2 <- digParser
    return (dig1, op, dig2)
