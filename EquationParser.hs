module EquationParser (parseEquation) where

import Parser
import EquationTypes
import GeneralParsers

parseX :: Parser Char Char
parseX = char 'x' +++ char 'X'

parseTSign :: Parser Char TSign
parseTSign = (char '+' >> return TPlus) +++ (char '-' >> return TMinus)

parseTNum :: Parser Char TNum
parseTNum = fmap TDouble double <++ fmap TInt num

parseCoeff :: Parser Char TNum
parseCoeff = do
  x <- token parseTNum
  token $ optional (char '*')
  return x

parseDegree :: Parser Char TNum
parseDegree = do
  token $ char '^'
  token $ parseTNum

parseXTerm :: Parser Char Term
parseXTerm = do
  coef <- token $ option parseCoeff (TInt 1)
  token parseX
  degree <- token $ option parseDegree (TInt 1)
  return (Term coef degree)

parseTerm :: Parser Char Term
parseTerm = parseXTerm <++ (fmap (flip Term (TInt 0)) parseTNum)

parseSignedTerm :: Parser Char Term
parseSignedTerm = do
  sign <- token $ parseTSign
  term <- token $ parseTerm
  case sign of
    TMinus -> return (negate term)
    _      -> return term

parseTerms :: Parser Char [Term]
parseTerms = do
  x <- token $ parseTerm
  y <- many parseSignedTerm
  return (x:y)

parseEquation :: Parser Char Equation
parseEquation = do
  lhs <- parseTerms
  token $ char '='
  rhs <- parseTerms
  skipWhitespaces
  return (Equation lhs rhs)
