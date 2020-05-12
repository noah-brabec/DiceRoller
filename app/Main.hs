module Main where

import Endpoints (getRoll)
import DiceEval (dsTest, dsEval)
import DiceParser (dsParse)

main :: IO ()
main = do{
    print (dsEval ([], 0) (dsParse "1+2d4+3d6"));
    getRoll
}
