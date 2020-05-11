module Main where

import Lib
import DiceParser (parseWithWhitespace, parseExpr)

main :: IO ()
main = do {
    print (parseWithWhitespace parseExpr "1");
    print (parseWithWhitespace parseExpr "41d100");
}
