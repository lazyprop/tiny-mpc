module ExprParser where

import Data.Char (isDigit, digitToInt)

import           TinyMPC hiding (any, fail)
import qualified TinyMPC as Mpc

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show, Eq)

opParser :: Parser Op
opParser = do
    c <- Mpc.any
    case c of
      '+' -> return Add
      '-' -> return Sub
      '*' -> return Mul
      '/' -> return Sub
      _   -> Mpc.fail "not a valid operation"


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
