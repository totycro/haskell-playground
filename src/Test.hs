{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import qualified Network.Wai.Test              as WT
import           Network.HTTP.Types
import qualified Database.PostgreSQL.Simple    as PG
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

import qualified Data.ByteString.UTF8          as BSU
import qualified Data.ByteString.Lazy.Char8    as BSLC

import           Debug.Trace

-- TODO: look into linting tools (hlint?)

main :: IO ()
main = do
    conn <- dbConnection
    hspec $ before_ (recreateDb conn) $ spec conn


recreateDb :: PG.Connection -> IO ()
recreateDb conn = do
    deleteContent conn `catch` (\(e :: PG.SqlError) -> createTable conn)
    insertData conn


someWord :: PG.Connection -> IO MyWord
someWord conn =
    Prelude.head
        <$> (PG.query_ conn "SELECT id, text FROM words LIMIT 1" :: IO [MyWord])


detailUrl :: MyWord -> BSU.ByteString
detailUrl word =
    BSU.fromString $ "/word/" ++ show (coerce $ pk word :: Integer)


detailUrlDoesNotExist :: BSU.ByteString
detailUrlDoesNotExist = "/word/1234541"



matchMaybe :: (a -> Bool) -> Maybe a -> Bool
matchMaybe = maybe False


spec :: PG.Connection -> Spec
spec conn = with webApp $ do

    describe "Retrieval" $ do

        it "get provides detail" $ do
            word     <- liftIO $ someWord conn
            response <- get $ detailUrl word
            liftIO $ A.decode (WT.simpleBody response) `shouldBe` Just word

        it "list provides list" $ do
            response <- get "/word"
            liftIO
                $ (A.decode (WT.simpleBody response) :: Maybe [MyWord])
                `shouldSatisfy` matchMaybe (\l -> Prelude.length l > 2)

        it "get results in 404 if word doesn't exist"
            $                   get detailUrlDoesNotExist
            `shouldRespondWith` 404


    describe "Creation" $ do

        it "return 201 on success"
            $                   post "/word" [json|{text: "newWord"}|]
            `shouldRespondWith` 201

        it "actually creates objects" $ do
            response <- post "/word" [json|{text: "newWord"}|]
            liftIO
                (              PG.query_
                        conn
                        "SELECT count(*) FROM words WHERE text = 'newWord';"
                `shouldReturn` [PG.Only (1 :: Integer)]
                )

        it "doesn't allow duplicates" $ do
            let jsonData = [json|{text: "newWord"}|]
            _ <- post "/word" jsonData
            post "/word" jsonData `shouldRespondWith` 400

        it "fails with bad request if text is not provided"
            $                   post "/word" [json|{}|]
            `shouldRespondWith` 400

        it "fails with bad request if text is wrong type"
            $                   post "/word" [json|{text: 24}|]
            `shouldRespondWith` 400


    describe "Update" $ do
        it "returns 200 on success" $ do
            word <- liftIO (someWord conn)
            put (detailUrl word) [json|{text: "foo"}|] `shouldRespondWith` 200

        it "changes db entry" $ do
            word <- liftIO (someWord conn)
            let newText = text word ++ " and more"
            put (detailUrl word)
                (BSLC.pack $ "{\"text\": \"" ++ newText ++ "\"}")
            liftIO
                $ PG.query conn "SELECT text FROM words WHERE id = ?" [pk word]
                `shouldReturn` [PG.Only newText]

        it "returns 404 on unknown data"
            $                   put detailUrlDoesNotExist [json|{text: "foo"}|]
            `shouldRespondWith` 404


        it "refuses to update on invalid data" $ do
            word <- liftIO (someWord conn)
            put (detailUrl word) "foobar" `shouldRespondWith` 400


    describe "Deletion" $ do

        it "returns 204 on success" $ do
            word <- liftIO $ someWord conn
            delete (detailUrl word) `shouldRespondWith` 204

        it "actually deletes element" $ do
            word <- liftIO $ someWord conn
            delete $ detailUrl word
            liftIO
                (PG.query conn
                          "SELECT count(*) FROM words WHERE id = ?;"
                          (pk word)

                `shouldReturn` [PG.Only (0 :: Integer)]
                )

        it "errors if item doesn't exist"
            $                   delete detailUrlDoesNotExist
            `shouldRespondWith` 404  -- sometimes 204 is also used
