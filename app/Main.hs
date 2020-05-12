module Main where

import Lib
import DiceEval (dsTest, dsEval)
import DiceParser (parseDiceString)

main :: IO ()
main = print (dsEval ([], 0) (parseDiceString "1+2d4+3d6"))
