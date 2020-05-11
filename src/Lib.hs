{-# LANGUAGE OverloadedStrings, GADTs #-}

module Lib where

{-
import Web.Scotty
import Data.Text.Lazy as LazyT
import Data.Monoid (mconcat) 
import System.Random

data DiceString where 
  Num :: Int -> DiceString
  Die :: Dice -> DiceString
  Plus :: DiceString -> DiceString -> DiceString
  Minus :: DiceString -> DiceString -> DiceString
  Times :: DiceString -> DiceString -> DiceString
  Div :: DiceString -> DiceString -> DiceString
  deriving(Show, Eq)

{- 
  The laguage is going to look something like:
  expr :: int | dice | plus | minus | mult | div 
  plus :: expr expr | Nop
  ^^ This is the same for the rest, roughly
-}



data Dice where
  D4 :: Dice
  D6 :: Dice
  D8 :: Dice
  D10 :: Dice
  D12 :: Dice
  D20 :: Dice
  D100 :: Dice
  deriving(Show, Eq)

libMain :: IO ()
libMain = scotty 3000 $
  get "/:word" $ do
    beam <- param "word";
    html $ mconcat ["<h1>Scotty, ", LazyT.pack (printDiceString beam), " me up!</h1>"]


view :: String
view = "<h1>Dice Roller</h1>"

printDiceString :: DiceString -> String
printDiceString ds = show (eval ds)

eval :: DiceString -> Maybe DiceString
eval (Num n) = return (Num n)
eval (Die d) = eval (getRoll d)
eval (Plus l r) = do { t1 <- eval l;
                       t2 <- eval r;
                       return (t1 + t2)}
eval (Minus l r) = do { t1 <- eval l;
                        t2 <- eval r;
                        return (t1 - t2)}
eval (Times l r) = do { t1 <- eval l;
                        t2 <- eval r;
                        return (t1 * t2)}
eval (Div l r) = do { t1 <- eval l;
                      t2 <- eval r;
                      return (t1 `div` t2)}

getRoll :: Dice -> DiceString
getRoll D4 = roll 1 4
getRoll D6 = roll 1 6
getRoll D8 = roll 1 8
getRoll D10 = roll 1 10
getRoll D12 = roll 1 12
getRoll D20 = roll 1 20
getRoll D100 = roll 1 100

roll :: Int -> Int -> DiceString
roll lower upper =  Num (fst (randomR(lower, upper) $ newStdGen))
-}