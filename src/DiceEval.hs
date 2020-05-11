{-# LANGUAGE GADTs #-}

module DiceEval (dsEval, dsTest) where


{- After parsing, this file will be able to handle the parsed strings. Below is an example:
    
    Plus((DiceRoll 3 4) (Modifier 6))
    This equates to 3d4 + 6
    
    This will return a tuple containing the result of each roll, and the total.
    Below is an example output from the input above
    ([(D4,1), (D4, 3), (D4, 2), (Mod, 6)], 12) 
-}

data DiceString where
    Modifier :: Int -> DiceString
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

dsEval :: Result -> DiceString -> Result
dsEval (rolls, total) (Modifier n) = ((Mod, n):rolls, total + n)
dsEval result (DiceRoll n d) = (rollDie result n d)
dsEval (rolls, total) (Plus l r) = let lr = (dsEval ([], 0) l) in
                                       let rr = (dsEval ([], 0) r) in
                                           ((fst lr) ++ (fst rr) ++ rolls, (snd lr) + (snd rr) + total)
dsEval (rolls, total) (Minus l r) = let lr  = (dsEval ([], 0) l) in
                                        let rr  = (dsEval ([], 0) r) in
                                            ((fst lr) ++ (fst rr) ++ rolls, total + ((snd lr) - (snd rr)))
dsEval (rolls, total) (Times l r) = let lr = (dsEval ([], 0) l) in
                                        let rr = (dsEval ([], 0) r) in
                                            ((fst lr) ++ (fst rr) ++ rolls, total + ((snd lr) * (snd rr)))

rollDie :: Result -> Int -> Die -> Result
rollDie (rolls, total) 0 d = (rolls, total) -- We are done rolling 
rollDie (rolls, total) n d = (rollDie ((d, n):rolls, n + total) (n-1) d) --TODO actually get a random val lol

dsTest :: Result
dsTest = (dsEval ([], 0) (Plus (DiceRoll 3 D4) (Modifier 6)))