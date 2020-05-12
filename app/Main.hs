module Main where

import Lib
import DiceEval (dsTest)
import DiceParser (parseDiceString)

main :: IO ()
main = print (parseDiceString "1+2d4+3d6")
