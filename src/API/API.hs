{-# LANGUAGE OverloadedStrings #-}

module API where

newtype User = String

import Web.Scotty

runAPI :: IO ()
runAPI =  scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
