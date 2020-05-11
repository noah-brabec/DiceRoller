{-# LANGUAGE GADTs, FlexibleContexts #-}

module DiceParser where

import Control.Monad
import DiceEval (DiceString(..), Die)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token 
import Text.Parsec.Language

languageDef = 
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = ["d"]
            , reservedOpNames = ["+", "-", "d", "*"]
            }

lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser DiceString
expr = buildExpressionParser opTable term

opTable = [ [ inFix "*" Times AssocLeft ]
          , [ inFix "+" Plus AssocLeft ]
          , [ inFix "-" Minus AssocLeft ]
          ]

modExpr :: Parser DiceString
modExpr = do i <- integer lexer
             return (Num (fromInteger i))

dieExpr :: Parser DiceString
dieExpr = do n <- integer lexer
             reserved lexer "d"
             d <- integer lexer
             return (DiceTerm (fromInteger n) (fromInteger d))

term = parens lexer expr
       <|> dieExpr
       <|> modExpr

parseDiceString = parseString expr

