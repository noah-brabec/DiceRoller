module Main where

import Endpoints (getRoll)
import DiceEval (dsEval)
import DiceParser (dsParse)

main :: IO ()
main = getRoll
