module TinyMPC  where

newtype Parser a =
    Parser { parse :: String -> Either String (a, String) }


instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g (Parser p) = Parser $
        \s -> case p s of
                Left e          -> Left e
                Right (res, s') -> Right (g res, s')

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure p = Parser $ \s -> Right (p, s)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser g) <*> (Parser p) = Parser $
        \s -> case g s of
                Left e         -> Left e
                Right (fn, s') -> 
                    case p s' of
                      Left e'           -> Left e'
                      Right (res, s'')  -> Right (fn res, s'')

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $
        \s -> case p s of
                Left e          -> Left e
                Right (res, s') -> parse (f res) s'


fail :: String -> Parser a
fail e = Parser $ \_ -> Left e
    
any :: Parser Char
any = Parser go
  where
      go []     = Left "any: end of file"
      go (x:xs) = Right (x, xs)


runParser :: Parser a -> String -> Either String a
runParser (Parser p) s = fst <$> p s

