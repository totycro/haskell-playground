{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.WordSpec
    ( spec
    )
where

import           TestImport
import           Control.Lens                   ( (^..) )
import           Data.Aeson
import           Data.Aeson.QQ                  ( aesonQQ )
import           Data.Aeson.Types               ( parseMaybe
                                                , FromJSON
                                                )
import           Data.Aeson.Lens                ( values
                                                , key
                                                , _String
                                                )
import qualified Data.ByteString.Lazy          as BSL
import qualified Network.Wai.Test              as WAIT
import qualified Database.Persist
import qualified Network.HTTP.Types            as HTTPT

import qualified Yesod



matchMaybe :: (a -> Bool) -> Maybe a -> Bool
matchMaybe = maybe False


decodedResponseShouldSatisfy
    :: (Show a, FromJSON a) => (a -> Bool) -> YesodExample site ()
decodedResponseShouldSatisfy check = withResponse $ \response ->
    liftIO
        $               (decode . WAIT.simpleBody) response
        `shouldSatisfy` matchMaybe check


requestJson
    :: (Yesod.Yesod site, Yesod.RedirectUrl site url)
    => HTTPT.Method
    -> url
    -> BSL.ByteString
    -> YesodExample site ()
requestJson method url body = request $ do
    setMethod method
    setUrl url
    setRequestBody body
    addRequestHeader ("Content-Type", "application/json")


postJson
    :: (Yesod.Yesod site, Yesod.RedirectUrl site url)
    => url
    -> BSL.ByteString
    -> YesodExample site ()
postJson = requestJson HTTPT.methodPost


aDomain :: YesodExample App (Key Domain)
aDomain = do
    res <- runDB $ insertBy (Domain "animals" Nothing)
    return $ either entityKey id res

aPopulatedDomain :: YesodExample App (Key Domain)
aPopulatedDomain = runDB $ do
    domain <- insert $ Domain "populated domain" Nothing
    _      <- insert $ MyWord "word of populated domain" domain
    return domain

aWord :: YesodExample App (Key MyWord)
aWord = do
    domId <- aDomain
    runDB $ insert $ MyWord "foo" domId


deletedWordId :: SIO (YesodExampleData App) (Key MyWord)
deletedWordId = do
    wordId <- aWord
    runDB $ delete wordId
    return wordId


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
            domId <- aDomain
            _     <- aWord
            _     <- runDB $ insert $ MyWord "snd" domId

            get WordR

            statusIs 200
            decodedResponseShouldSatisfy
                $ matchMaybe (\(l :: [MyWord]) -> length l == 2)
                . parseMaybe (.: "data")


        it "returns word detail" $ do
            wordId <- aWord
            get $ WordDetailR wordId
            statusIs 200
            decodedResponseShouldSatisfy $ matchMaybe (wordText ==) . parseMaybe
                (.: "text")

        it "returns 404 on unknown id" $ do
            wordId <- deletedWordId
            get $ WordDetailR wordId
            statusIs 404


    describe "Creation" $ do

        it "allows creating new words" $ do
            domId <- aDomain
            postJson
                WordR
                (encode (object ["text" .= wordText, "domain" .= domId]))
            statusIs 201
            matchingCount <- runDB $ count [MyWordText ==. wordText]
            liftIO $ matchingCount `shouldBe` 1

        it "refuses to create duplicates" $ do
            postJson WordR (encode (object ["text" .= wordText]))
            postJson WordR (encode (object ["text" .= wordText]))
            statusIs 400

        it "returns bad request if data malformed" $ do
            postJson WordR (encode (object ["text" .= (123 :: Int)]))
            statusIs 400


    describe "Deletion" $ do

        it "deletes existing elements" $ do
            wordId <- aWord
            performMethod HTTPT.methodDelete $ WordDetailR wordId
            statusIs 204
            wordAfter <- runDB $ Database.Persist.get wordId
            liftIO $ wordAfter `shouldBe` Nothing

        it "returns 204 if element does not exist" $ do
            wordId <- deletedWordId
            performMethod HTTPT.methodDelete $ WordDetailR wordId
            statusIs 204  -- sometimes also 404 is used


    describe "Update" $ do

        it "updates an existing entry" $ do
            let newWordText = wordText <> " and so on"
            wordId <- aWord
            domId  <- aDomain
            requestJson
                HTTPT.methodPut
                (WordDetailR wordId)
                (encode $ object ["text" .= newWordText, "domain" .= domId])
            statusIs 200
            newWord <- runDB $ Database.Persist.get wordId
            liftIO $ newWord `shouldSatisfy` matchMaybe
                ((newWordText ==) . myWordText)

        it "returns 404 on non-existing entry" $ do
            domId  <- aDomain
            wordId <- deletedWordId
            requestJson
                HTTPT.methodPut
                (WordDetailR wordId)
                (encode $ object ["text" .= ("foo" :: Text), "domain" .= domId])
            statusIs 404

        it "refuses to update on invalid data" $ do
            wordId <- aWord
            requestJson HTTPT.methodPut
                        (WordDetailR wordId)
                        (encode $ object ["text" .= (3 :: Int)])
            statusIs 400


    describe "Domains" $ do

        it "shows words of domain" $ do
            let domName = "test words"
            let word1   = "foo"
            let word2   = "bar"
            domId <- runDB $ insert $ Domain domName Nothing
            _     <- runDB $ insert $ MyWord word1 domId
            _     <- runDB $ insert $ MyWord word2 domId

            get DomainR

            statusIs 200
            decodedResponseShouldSatisfy
                (== [aesonQQ| [
                        { name: #{domName}, words: [ #{word1}, #{word2} ] }
                    ] |]
                )


    describe "Empty Domains" $ do

        it "shows only empty domains" $ do
            let emptyDomName = "empty domain" :: Text
            _ <- runDB $ insert $ Domain emptyDomName Nothing
            _ <- aPopulatedDomain

            get EmptyDomainR

            statusIs 200

            withResponse $ \response ->
                liftIO
                    $          WAIT.simpleBody response
                    ^..        values
                    .          key "name"
                    .          _String
                    `shouldBe` [emptyDomName]
