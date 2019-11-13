{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.WordSpec
    ( spec
    )
where

import           TestImport

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy          as BSL
import qualified Network.Wai.Test              as WAIT

import qualified Yesod



matchMaybe :: (a -> Bool) -> Maybe a -> Bool
matchMaybe = maybe False


decodedResponseShouldSatisfy
    :: (Show a, FromJSON a) => (a -> Bool) -> YesodExample site ()
decodedResponseShouldSatisfy check = withResponse $ \response ->
    liftIO
        $               (decode . WAIT.simpleBody) response
        `shouldSatisfy` matchMaybe check


shouldHaveLength :: Show a => [a] -> Int -> Expectation
shouldHaveLength l i = l `shouldSatisfy` (\li -> length li == i)


postJson
    :: (Yesod.Yesod site, Yesod.RedirectUrl site url)
    => url
    -> BSL.ByteString
    -> YesodExample site ()
postJson url body = request $ do
    setMethod "POST"
    setUrl url
    setRequestBody body
    addRequestHeader ("Content-Type", "application/json")

spec :: Spec
spec = withApp $ do
    let wordText = "foo" :: Text
    describe "Retrieval" $ do
        it "returns empty list on no content" $ do
            -- (this test is mostly useless)
            get WordR

            statusIs 200
            decodedResponseShouldSatisfy
                $ matchMaybe (null :: [MyWord] -> Bool)
                . parseMaybe (.: "data")

        it "returns list of elements" $ do
            _ <- runDB $ do
                _ <- insert $ MyWord "fst"
                insert $ MyWord "snd"

            get WordR

            statusIs 200
            decodedResponseShouldSatisfy
                $ matchMaybe (\(l :: [MyWord]) -> length l == 2)
                . parseMaybe (.: "data")


        it "returns word detail" $ do
            wordId <- runDB $ insert $ MyWord wordText
            get $ WordDetailR wordId
            statusIs 200
            decodedResponseShouldSatisfy $ matchMaybe (wordText ==) . parseMaybe
                (.: "text")

        it "returns 404 on unknown id" $ do
            wordId <- runDB $ insert $ MyWord wordText
            runDB $ delete wordId
            get $ WordDetailR wordId
            statusIs 404

    describe "Creation" $ do
        it "allows creating new words" $ do
            postJson WordR (encode (object ["text" .= wordText]))
            statusIs 201
            matchingWords <- runDB $ selectList [MyWordText ==. wordText] []
            liftIO $ matchingWords `shouldHaveLength` 1

        it "refuses to create duplicates" $ do
            postJson WordR (encode (object ["text" .= wordText]))
            postJson WordR (encode (object ["text" .= wordText]))
            statusIs 400

        it "returns bad request if data malformed" $ do
            postJson WordR (encode (object ["text" .= (123 :: Int)]))
            statusIs 400




-- TODO: write tests
-- TODO: basically port tests from scotty
