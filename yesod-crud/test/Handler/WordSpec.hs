{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- TODO: possibly not have these in global scope if not necessary
parseDataList :: Object -> Maybe [MyWord]
parseDataList = parseMaybe (\obj -> obj .: "data" >>= return)

decodeResponse :: WAIT.SResponse -> Maybe Object
decodeResponse r = (decode (WAIT.simpleBody r) :: Maybe Object)

responseShouldSatisfy :: MonadIO m => (Object -> Bool) -> WAIT.SResponse -> m ()
responseShouldSatisfy f r =
    liftIO $ decodeResponse r `shouldSatisfy` (matchMaybe f)


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
    describe "Retrieval" $ do
        it "returns empty list on no content" $ do
            -- (this test is mostly useless)
            get WordR
            statusIs 200
            -- list should be empty
            withResponse
                $ responseShouldSatisfy (matchMaybe null . parseDataList)

        it "returns list of elements" $ do
            _ <- runDB $ do
                _ <- insert $ MyWord "fst"
                insert $ MyWord "snd"

            get WordR
            statusIs 200
            withResponse $ responseShouldSatisfy
                (matchMaybe (\l -> length l == 2) . parseDataList)

    describe "Creation" $ do
        it "allows creating new words" $ do
            let wordText = "foo" :: Text
            postJson WordR (encode (object ["text" .= wordText]))
            statusIs 201
            matchingWords <- (runDB $selectList [MyWordText ==. wordText] [])
            liftIO $ (matchingWords `shouldHaveLength` 1)

        it "refuses to create duplicates" $ do
            let wordText = "foo" :: Text
            postJson WordR (encode (object ["text" .= wordText]))
            postJson WordR (encode (object ["text" .= wordText]))
            statusIs 400

        it "returns bad request if data malformed" $ do
            postJson WordR (encode (object ["text" .= (123 :: Int)]))
            statusIs 400



-- TODO: write tests
-- TODO: basically port tests from scotty
