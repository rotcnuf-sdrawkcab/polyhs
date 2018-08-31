module GeneralParsers where
import Parser
import Data.Char (isDigit, isSpace)

skipWhitespaces :: Parser Char ()
skipWhitespaces = skipMany $ satisfy isSpace

token :: Parser Char a -> Parser Char a
token p = skipWhitespaces >> p

num :: Parser Char Int
num = (char '-' >> fmap negate num') <++ num'
  where
    num' = do
      s <- many1 (satisfy isDigit)
      pure $ read s

double :: Parser Char Double
double = (char '-' >> fmap negate double') <++ double'
  where
    double' = do
      x <- many1 (satisfy isDigit)
      char '.'
      y <- many1 (satisfy isDigit)
      return $ read (x ++ '.':y)
