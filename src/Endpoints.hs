{-# LANGUAGE OverloadedStrings #-}

module Endpoints (getRoll) where

import Web.Scotty
import DiceParser (dsParse)
import DiceEval (dsEval, Result)
import Data.Text.Lazy as LazyT
import Data.Monoid (mconcat) 
import Control.Monad.IO.Class

getRoll :: IO ()
getRoll = scotty 3000 $
  get "/:word" $ do {
    beam <- param "word";
    output <- liftIO (parseRoll beam);
    json (mconcat ["total: ", show (snd output), ", rolls: ", show(fst output)])
  }


parseRoll :: Text -> IO Result
parseRoll n = (dsEval ([], 0) (dsParse (Prelude.tail (show n))))