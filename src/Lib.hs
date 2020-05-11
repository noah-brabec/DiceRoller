{-# LANGUAGE OverloadedStrings, GADTs #-}

module Lib where

import Web.Scotty
import Data.Text.Lazy as LazyT
import Data.Monoid (mconcat) 

libMain :: IO ()
libMain = scotty 3000 $
  get "/:word" $ do
    beam <- param "word";
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


view :: String
view = "<h1>Dice Roller</h1>"