{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.WordSpec
    ( spec
    )
where

import           TestImport
import           Data.Aeson
import           Data.Aeson.Types

import           Network.Wai.Test              as WAIT


matchMaybe :: (a -> Bool) -> Maybe a -> Bool
matchMaybe = maybe False

-- TODO: possibly not have these in global scope if not necessary
parseDataList :: Object -> Maybe [MyWord]
parseDataList = parseMaybe (\obj -> obj .: "data" >>= return)

decodeResponse :: SResponse -> Maybe Object
decodeResponse r = (decode (WAIT.simpleBody r) :: Maybe Object)

responseShouldSatisfy :: MonadIO m => (Object -> Bool) -> SResponse -> m ()
responseShouldSatisfy f r =
    liftIO $ decodeResponse r `shouldSatisfy` (matchMaybe f)


spec :: Spec
spec = withApp $ do
    describe "Word retrieval" $ do
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


-- TODO: write tests
-- TODO: basically port tests from scotty
