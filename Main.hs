{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 8000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Beam, ", beam, " me up!</h1>"]
