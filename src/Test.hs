{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import qualified Network.Wai.Test              as WT
import           Network.HTTP.Types
import           Database.PostgreSQL.Simple    as PG
import qualified Data.Aeson                    as A
import           Data.Coerce                    ( coerce )

import           WebApp                         ( webApp
                                                , dbConnection
                                                )
import           Types                          ( MyWord(..)
                                                , WordId(..)
                                                )

import           InitDB
import           Control.Exception              ( catch )

import           Data.ByteString.UTF8          as BSU

import           Debug.Trace

main :: IO ()
main = do
    conn <- dbConnection
    hspec $ before_ (recreateDb conn) $ spec conn


recreateDb :: PG.Connection -> IO ()
recreateDb conn = do
    deleteContent conn `catch` (\(e :: SqlError) -> createTable conn)
    insertData conn


someWord :: Connection -> IO MyWord
someWord conn =
    head
        <$> (PG.query_ conn "SELECT id, text FROM words LIMIT 1" :: IO [MyWord])


detailUrl :: MyWord -> BSU.ByteString
detailUrl word =
    BSU.fromString $ "/word/" ++ show (coerce $ pk word :: Integer)


matchMaybe :: (a -> Bool) -> Maybe a -> Bool
matchMaybe = maybe False


spec :: PG.Connection -> Spec
spec conn = with webApp $ do
    describe "Retrieval" $ do
        it "get provides detail" $ do
            word     <- liftIO $ someWord conn
            response <- get $ detailUrl word
            liftIO
                $          (A.decode (WT.simpleBody response) :: Maybe MyWord)
                `shouldBe` Just word

        it "list provides list" $ do
            response <- get "/word"
            liftIO
                $ (A.decode (WT.simpleBody response) :: Maybe [MyWord])
                `shouldSatisfy` matchMaybe (\l -> Prelude.length l > 2)

        it "get results in 404 if word doesn't exist"
            $                   get "/word/12345"
            `shouldRespondWith` 404

    describe "Creation" $ do
        it "allows to add a new word"
            $                   post "/word" [json|{text: "newWord"}|]
            `shouldRespondWith` 201

        it "fails with bad request if text is not provided"
            $                   post "/word" [json|{}|]
            `shouldRespondWith` 400

        it "fails with bad request if text is wrong type"
            $                   post "/word" [json|{text: 24}|]
            `shouldRespondWith` 400
