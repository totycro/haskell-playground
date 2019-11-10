{-# LANGUAGE OverloadedStrings #-}

module WebApp where

import           Control.Monad.IO.Class         ( liftIO )
import           Network.HTTP.Types.Status
import           Data.Maybe
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as AT
import qualified Data.ByteString.Lazy          as BSL

-- TODO; not have this here?
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger

import qualified Web.Scotty                    as S

import           Database.PostgreSQL.Simple

import           InitDB                         ( initDb )
import qualified Types

import           Debug.Trace

import           Network.Wai                    ( Application )


dbConnection :: IO Connection
dbConnection = connect defaultConnectInfo { connectDatabase = "postgres"
                                          , connectHost     = "db"
                                          , connectPassword = "1234"
                                          }


webApp :: IO Application
webApp = do
    conn <- dbConnection
    initDb conn

    S.scottyApp $ do
        S.defaultHandler
            (\e -> do
                trace (show e) (liftIO $ putStrLn "error handler reached")
                S.text e
            )
        S.get "/word" $ do
            items <- liftIO
                (query_ conn "SELECT id, text FROM words;" :: IO [Types.MyWord])
            S.json items

        S.get "/word/:id" $ do
            wordId <- S.param "id"
            item   <-
                liftIO
                $ listToMaybe
                <$> (query conn
                           "SELECT id, text FROM words where id = ?;"
                           [wordId :: Integer] :: IO [Types.MyWord]
                    )
            maybe (S.status status404 >> S.text "not found") S.json item

        S.post "/word" $ do
            body <- S.body
            case parseWordText body of
                Just text -> do
                    liftIO $ () <$ execute
                        conn
                        "INSERT INTO words(text) VALUES(?);"
                        [text]
                    S.status status201
                Nothing -> S.status status400


parseWordText :: BSL.ByteString -> Maybe String
parseWordText s = A.decode s >>= AT.parseMaybe (A..: "text")


develMain :: IO ()
develMain = webApp >>= (run 8000 . logStdoutDev)
