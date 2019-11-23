{-# LANGUAGE QuasiQuotes #-}
module CategorerServerSpec
    ( spec
    )
where

import           Test.Hspec                     ( Spec
                                                , runIO
                                                , it
                                                , describe
                                                , around_
                                                , shouldBe
                                                , shouldSatisfy
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )

import           Network.HTTP.Types             ( Status(..)
                                                , status400
                                                )
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Data.List                      ( (!!) )
import           Data.Aeson.QQ                  ( aesonQQ )
import           Servant                        ( Proxy(..) )
import           Servant.Client                 ( client
                                                , parseBaseUrl
                                                , runClientM
                                                , mkClientEnv
                                                , responseStatusCode
                                                , ClientError(..)
                                                )
import           CategorerServer                ( CategorerAPI
                                                , app
                                                )
import           RetrieveCategories             ( Retrieve )
import           TestUtils                      ( mockResponse )

testAppPort :: Int
testAppPort = 8888

mockCategories :: [Text]
mockCategories = ["a", "b", "and c"]

mockRetrieveCategories :: Retrieve
mockRetrieveCategories _ = return $ mockResponse [aesonQQ|
    {"parse": {"categories": [
        {"*": #{mockCategories !! 0}},
        {"*": #{mockCategories !! 1}},
        {"*": #{mockCategories !! 2}}
    ]}}|]


withApp :: IO () -> IO ()
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
withApp action = bracket
    (liftIO $ forkIO $ run testAppPort $ app mockRetrieveCategories)
    killThread
    (const action)

errorsWithStatus :: Status -> Either ClientError a -> Bool
errorsWithStatus status (Left (FailureResponse _ response)) =
    responseStatusCode response == status
errorsWithStatus _ _ = False

spec :: Spec
spec = around_ withApp $ do
    let categoryAPI = client (Proxy :: Proxy CategorerAPI)

    baseUrl <- runIO $ parseBaseUrl ("http://localhost:" <> show testAppPort)
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    describe "CategorerServer" $ do

        it "provides categories for word" $ do
            result <- runClientM (categoryAPI $ Just "foo") clientEnv
            result `shouldBe` Right mockCategories

        it "error if no word given" $ do
            result <- runClientM (categoryAPI Nothing) clientEnv
            result `shouldSatisfy` errorsWithStatus status400
