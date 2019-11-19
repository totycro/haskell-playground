{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
    ( main
    )
where

import           Data.Aeson                     ( encode )
import           Data.Aeson.QQ                  ( aesonQQ )
import qualified Data.ByteString.Lazy          as BL
import qualified Network.Wreq                  as WREQ
import qualified Network.HTTP.Client.Internal  as HCI
import qualified Network.HTTP.Types            as HT
import           Test.Hspec

import           Categorer                      ( MonadHttp(..)
                                                , retrieveCategories
                                                )
import           Control.Monad.Trans.Class

-- Use Reader monad to specify a mock response
-- based on:
-- https://stackoverflow.com/questions/40975421/mock-a-wreq-response
-- https://hackage.haskell.org/package/pusher-http-haskell-0.3.0.0/src/test/HTTP.hs
newtype MockServer m a = MockServer
    { server :: ReaderT (WREQ.Response BL.ByteString) m a}
    deriving (Applicative, Functor, Monad, MonadTrans)

deriving instance Monad m => MonadReader (WREQ.Response BL.ByteString) (MockServer m)

runMockServer :: MockServer m a -> WREQ.Response BL.ByteString -> m a
runMockServer (MockServer s) = runReaderT s

instance Monad m => MonadHttp (MockServer m) where
    get _ = ask

succeededResponse :: WREQ.Response BL.ByteString
succeededResponse = HCI.Response
    { HCI.responseStatus    = HT.mkStatus 200 "success"
    , HCI.responseVersion   = HT.http11
    , HCI.responseHeaders   = []
    , HCI.responseBody = encode [aesonQQ|
        {"parse": {"categories": [
            {"*": "foo"},
            {"*": "bar"}
        ]}}|]
    , HCI.responseCookieJar = HCI.createCookieJar []
    , HCI.responseClose'    = HCI.ResponseClose (return () :: IO ())
    }

test :: Spec
test = describe "retrieveCategories" $ do
    it "returns categories from response " $ do
        runMockServer (retrieveCategories "foo") succeededResponse
            `shouldReturn` ["foo", "bar"]

main :: IO ()
main = hspec test
