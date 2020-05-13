{-# LANGUAGE GADTs, FlexibleContexts #-}

module DiceParser where

import Control.Monad
import DiceEval (DiceString(..), Die)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token 
import Text.Parsec.Language

-- Dice language def, all we really need is basic math operations and 'd'
-- Thanks to Dr. Perry Alexander for showing us this system in EECS 662
languageDef = 
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = []
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

--Sets up what operators we will match on
--This is in order of priority
--Each constructor corresponds to a constructor from DiceEval.DiceString
opTable = [ [ inFix "d" DiceTerm AssocLeft]
          , [ inFix "*" Times AssocLeft ]
          , [ inFix "+" Plus AssocLeft ]
          , [ inFix "-" Minus AssocLeft ]
          ]

--Parser for singular nums
modExpr :: Parser DiceString
modExpr = do i <- integer lexer
             return (Num (fromInteger i))

--A term is either an expr as defined from the op table or a modifier (a single number)
term = parens lexer expr
       <|> modExpr

dsParse = parseString expr

