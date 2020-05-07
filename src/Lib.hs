{-# LANGUAGE OverloadedStrings #-}

module Lib (libMain) where

import Web.Scotty

import Data.Monoid (mconcat) 

libMain :: IO ()
libMain = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
