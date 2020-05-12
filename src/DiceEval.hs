{-# LANGUAGE GADTs #-}

module DiceEval (dsEval, DiceString (..), Die, Result) where

import System.Random
import System.IO.Unsafe
import Control.Monad.IO.Class

{- After parsing, this file will be able to handle the parsed strings. Below is an example:
    
    Plus((DiceRoll 3 4) (Modifier 6))
    This equates to 3d4 + 6
    
    This will return a tuple containing the result of each roll, and the total.
    Below is an example output from the input above
    ([(D4,1), (D4, 3), (D4, 2), (Mod, 6)], 12) 
-}

data DiceString where
    Num :: Int -> DiceString
    DiceTerm :: DiceString -> DiceString -> DiceString
    DiceRoll :: Int -> Die -> DiceString 
    Plus :: DiceString -> DiceString -> DiceString
    Minus :: DiceString -> DiceString -> DiceString
    Times :: DiceString -> DiceString -> DiceString
    deriving(Show, Eq)

data Die where
    Mod :: Die
    D4 :: Die
    D6 :: Die
    D8 :: Die
    D10 :: Die
    D12 :: Die
    D20 :: Die
    D100 :: Die
    deriving(Show, Eq)

type Result = ([(Die, Int)], Int)

dsEval :: Result -> DiceString -> IO Result
dsEval (rolls, total) (Num n) = return ((Mod, n):rolls, total + n)
dsEval result (DiceRoll n d) = (rollDie result n d)
dsEval result (DiceTerm l r) = (dsEval result (termToRoll (DiceTerm l r)))
dsEval (rolls, total) (Plus l r) = do{ lr <- (dsEval ([], 0) l);
                                       rr <- (dsEval ([], 0) r);
                                       return ((fst lr) ++ (fst rr) ++ rolls, (snd lr) + (snd rr) + total)
                                       }
dsEval (rolls, total) (Minus l r) = do { lr <- (dsEval ([], 0) l);
                                         rr <- (dsEval ([], 0) r);
                                         return ((fst lr) ++ (fst rr) ++ rolls, total + ((snd lr) - (snd rr)))
                                        }
dsEval (rolls, total) (Times l r) = do { lr <- (dsEval ([], 0) l);
                                         rr <- (dsEval ([], 0) r);
                                         return ((fst lr) ++ (fst rr) ++ rolls, total + ((snd lr) * (snd rr)))
                                        }

rollDie :: Result -> Int -> Die -> IO Result
rollDie (rolls, total) 0 d = return (rolls, total) -- We are done rolling 
rollDie (rolls, total) n D4 = do { value <- (getRandom 1 4);
                                   (rollDie ((D4, value):rolls, value + total) (n-1) D4);
                                }
rollDie (rolls, total) n D6 = do { value <- (getRandom 1 6);
                                   (rollDie ((D6, value):rolls, value + total) (n-1) D6)
                                }
rollDie (rolls, total) n D8 = do { value <- (getRandom 1 8);
                                   (rollDie ((D8, value):rolls, value + total) (n-1) D8)
                                }
rollDie (rolls, total) n D10 = do { value <- (getRandom 1 10);
                                    (rollDie ((D10, value):rolls, value + total) (n-1) D10)
                                }
rollDie (rolls, total) n D12 = do { value <- (getRandom 1 12);
                                    (rollDie ((D12, value):rolls, value + total) (n-1) D12)
                                }
rollDie (rolls, total) n D20 = do { value <- (getRandom 1 20);
                                    (rollDie ((D20, value):rolls, value + total) (n-1) D20)
                                }
rollDie (rolls, total) n D100 = do { value <- (getRandom 1 100);
                                     (rollDie ((D100, value):rolls, value + total) (n-1) D100)
                                }

getRandom :: Int -> Int -> IO Int
getRandom l u = randomRIO (l, u)

termToRoll :: DiceString -> DiceString
termToRoll (DiceTerm (Num n) (Num 4)) = DiceRoll n D4
termToRoll (DiceTerm (Num n) (Num 6)) = DiceRoll n D6
termToRoll (DiceTerm (Num n) (Num 8)) = DiceRoll n D8
termToRoll (DiceTerm (Num n) (Num 10)) = DiceRoll n D10
termToRoll (DiceTerm (Num n) (Num 12)) = DiceRoll n D12
termToRoll (DiceTerm (Num n) (Num 20)) = DiceRoll n D20
termToRoll (DiceTerm (Num n) (Num 100)) = DiceRoll n D100
termToRoll (DiceTerm _ _) = DiceRoll 0 D4
