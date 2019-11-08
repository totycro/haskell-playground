{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.Wai.Test
import           Network.HTTP.Types

import           WebApp                         ( webApp )

import           Debug.Trace

main :: IO ()
main = hspec $ do
    spec


spec :: Spec
spec = with webApp $ do
    -- TODO; cleanup db between runs
    describe "Retrieval" $ do
        it "list provides list" $ do
            get "/word"
                `shouldRespondWith` [json|[{text: "narudo"},{text: "epur"}]|]

        it "get provides detail" $ do
            get "/word/1" `shouldRespondWith` [json|{text: "narudo"}|]

        it "get results in 404 if word doesn't exist" $ do
            get "/word/12345" `shouldRespondWith` 404
        --it "word list responds arbit" $ do foo <- get "/word" liftIO $ (simpleBody foo) `shouldBe` "a" trace (show foo) 3

    describe "Creation" $ do
        it "allows to add a new word" $ do
            post "/word" [json|{text: "newWord"}|] `shouldRespondWith` 201

        it "fails with bad request if text is not provided" $ do
            post "/word" [json|{}|] `shouldRespondWith` 400
