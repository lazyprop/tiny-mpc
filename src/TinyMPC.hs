{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module TinyMPC  where

import Text.Printf

import Control.Applicative
import Control.Monad

newtype Parser a =
    Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g (Parser p) = Parser $ \inp ->
        map (\(v, s) -> (g v, s)) $ p inp

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure p = Parser $ \s -> [(p, s)]

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser g) <*> (Parser p) = Parser $ \s ->
        case g s of
          []         -> []
          [(fn, s')] -> 
              case p s' of
                []           -> []
                [(res, s'')]  -> [(fn res, s'')]

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \inp ->
        concatMap (\(res, s) -> parse (f res) s) (p inp)

instance Alternative Parser where
    empty = Parser $ \_ -> []

    (Parser p) <|> (Parser q) = Parser $ \inp ->
        case p inp of
          []  -> q inp
          (x:xs) -> (x:xs)

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser p) s = p s

plift :: Parser a -> Parser [a]
plift = liftM (\c -> [c])
