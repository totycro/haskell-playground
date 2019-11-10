{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.Wai.Test
import           Network.HTTP.Types
import           Database.PostgreSQL.Simple     ( SqlError )

import           WebApp                         ( webApp
                                                , dbConnection
                                                )

import           InitDB
import           Debug.Trace
import           Control.Exception              ( catch )

main :: IO ()
main = hspec $ before_ recreateDb $ do
    spec


recreateDb :: IO ()
recreateDb = do
    conn <- dbConnection
    deleteContent conn `catch` (\(e :: SqlError) -> createTable conn)
    insertData conn

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