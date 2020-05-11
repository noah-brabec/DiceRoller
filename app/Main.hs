module Main where

import Lib
import DiceParser (parseWithWhitespace, parseExpr)
import DiceEval (dsTest)

main :: IO ()
main = print dsTest
