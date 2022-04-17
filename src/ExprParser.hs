module ExprParser where

import Data.Char (isDigit, digitToInt)

import TinyMPC

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show, Eq)

opParser :: Parser Op
opParser = Parser go
  where
      go ""     = Left "opParser: end of file"
      go (c:cs) =
          case c of
            '+' -> Right (Add, cs)
            '-' -> Right (Sub, cs)
            '*' -> Right (Mul, cs)
            '/' -> Right (Div, cs)
            _   -> Left $ "opParser: " ++ [c] ++ " is not a valid operator"

                           
digParser :: Parser Int
digParser = Parser go
  where
      go ""     = Left "digParser: end of file"
      go (c:cs) = if isDigit c
                  then Right (digitToInt c :: Int, cs)
                  else Left $ "digParser:" ++ [c] ++  "is not a digit"


type Expr = (Int, Op, Int)

exprParser :: Parser Expr
exprParser = do
    dig1 <- digParser
    op <- opParser
    dig2 <- digParser
    return (dig1, op, dig2)
