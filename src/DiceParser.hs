{-# LANGUAGE GADTs, FlexibleContexts #-}

module DiceParser (parseWithWhitespace, parseExpr) where
-- Imports for Parsec

import Text.Parsec (ParseError, try, char, satisfy, many1, digit, oneOf, many, eof, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim((<|>))
import Control.Monad (void)

data Expr = Num Integer
          | DieTerm (Int, Int)
          | Add Expr Expr
          deriving(Show, Eq)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme exp = do{
  x <- exp;
  whitespace;
  return x
}

numOrDie :: Parser Expr
numOrDie = numE <|> diceTermE

numE :: Parser Expr
numE = do {
  n <- lexeme (many1 digit);
  return $ Num ( read n)
}

diceTermE :: Parser Expr
diceTermE = do {
    numRolls <- lexeme $ many1 digit;
    void $ char 'd';
    numFaces <- many1 digit;
    return (DieTerm (read numRolls, read numRolls))
  }
  
addE :: Parser Expr
addE = do {
  t1 <- numOrDie;
  void (lexeme (char '+'));
  t2 <- numOrDie;
  return (Add t1 t2)
}


parseExpr :: Parser Expr
parseExpr = try addE <|> try diceTermE <|> numE

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p