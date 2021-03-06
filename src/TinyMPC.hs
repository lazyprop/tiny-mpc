{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module TinyMPC  where

import Text.Printf

import Control.Applicative

newtype Parser a =
    Parser { parse :: String -> Either String (a, String) }


instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g (Parser p) = Parser $ \s ->
        case p s of
          Left e          -> Left e
          Right (res, s') -> Right (g res, s')

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure p = Parser $ \s -> Right (p, s)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser g) <*> (Parser p) = Parser $ \s ->
        case g s of
          Left e         -> Left e
          Right (fn, s') -> 
              case p s' of
                Left e'           -> Left e'
                Right (res, s'')  -> Right (fn res, s'')

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \s ->
        case p s of
          Left e          -> Left e
          Right (res, s') -> parse (f res) s'

instance Alternative Parser where
    empty = Parser $ \_ -> Left "empty parser"

    (Parser p) <|> (Parser q) = Parser $ \s ->
        case p s of
          Left _  -> q s
          Right x -> Right x

runParser :: Parser a -> String -> Either String a
runParser (Parser p) s = fst <$> p s

-- replaces the error of a parser with a given string
(<?>) :: Parser a -> String -> Parser a
(Parser p) <?> err = Parser $ \s ->
    case p s of
      Left  _ -> Left err
      Right x -> return x
infixl 2 <?>

-- combines two parsers in given order returning a list
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p <:> qs = do
    res1 <- p
    res2 <- qs
    return (res1 : res2)
infixr 7 <:>

(<:$>) :: Parser a -> Parser a -> Parser [a]
p <:$> q = p <:> sequence [q]
infixr 7 <:$>


(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
ps <++> qs = do
    res1 <- ps
    res2 <- qs
    return $ res1 ++ res2
