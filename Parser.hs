module Parser (
    Parser (runParser), reject, get, look, (+++), (<++),
    satisfy, many, many1, skipMany, skipMany1,
    sepBy, sepBy1, option, optional, char, string
  ) where

import Control.Arrow (first)
import Control.Monad (liftM2)

newtype Parser tok a = Parser { runParser :: [tok] -> [(a, [tok])] }

instance Functor (Parser tok) where
  fmap f (Parser g) = Parser $ \toks -> map (\(val, rest) -> (f val, rest)) (g toks)

instance Applicative (Parser tok) where
  pure x = Parser $ \toks -> [(x, toks)] -- just leave state unmodified
  (Parser f) <*> (Parser x) = Parser $ \toks -> do
    (func, rest) <- f toks
    map (first func) (x rest)

instance Monad (Parser tok) where
  (Parser p) >>= f = Parser $ \toks -> do
    (val, rest) <- p toks
    runParser (f val) rest

-- parser that always fails
reject :: Parser tok a
reject = Parser $ \_ -> []

-- parser that gets the first token
get :: Parser tok tok
get = Parser f
  where f []     = [] -- failure
        f (t:ts) = [(t, ts)] -- consume one token and return it

-- parser that returns the rest of the toks
look :: Parser tok [tok]
look = Parser $ \toks -> [(toks, toks)]

-- choose between two parsers
(+++) :: Parser tok a -> Parser tok a -> Parser tok a
p +++ q = Parser $ \toks ->
  runParser p toks ++ runParser q toks

-- leftbiased choice: Only run q if p fails
(<++) :: Parser tok a -> Parser tok a -> Parser tok a
p <++ q = Parser $ \toks -> case runParser p toks of
  [] -> runParser q toks
  xs -> xs

-- get if predicate holds
satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pred = do
  t <- get
  if pred t then
    return t
  else
    reject

-- parses p zero or more times
many :: Parser tok a -> Parser tok [a]
many p = many1 p +++ return []

-- parses p one or more times
many1 :: Parser tok a -> Parser tok [a]
many1 p = do
  x <- p
  xs <- many p
  pure $ x:xs

skipMany :: Parser tok a -> Parser tok ()
skipMany p = (many p) >> pure ()

skipMany1 :: Parser tok a -> Parser tok ()
skipMany1 p = p >> skipMany p

sepBy :: Parser tok a -> Parser tok sep -> Parser tok [a]
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: Parser tok a -> Parser tok sep -> Parser tok [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

option :: Parser tok a -> a -> Parser tok a
option p a = p <++ return a

optional :: Parser tok a -> Parser tok ()
optional p = (p >> return ()) +++ return ()

char :: Char -> Parser Char Char
char c = satisfy (== c)

string :: String -> Parser Char String
string str = do
  toks <- look
  scan str toks
  where
    scan [] _ = return str
    scan (x:xs) (y:ys) | x == y = get >>= \_ -> scan xs ys 
    scan _ _  = return []
