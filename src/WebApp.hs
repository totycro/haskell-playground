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

wordListView :: Connection -> IO [Types.MyWord]
wordListView conn =
    query_ conn "SELECT id, text FROM words;" :: IO [Types.MyWord]


wordDetailView :: Connection -> Types.WordId -> IO (Maybe Types.MyWord)
wordDetailView conn wordId =
    listToMaybe
        <$> (query conn "SELECT id, text FROM words where id = ?;" [wordId] :: IO
                  [Types.MyWord]
            )


wordCreateView :: Connection -> BSL.ByteString -> Maybe (IO ())
wordCreateView conn body = createItem <$> parseWordText body
  where
    parseWordText :: BSL.ByteString -> Maybe String
    parseWordText s = A.decode s >>= AT.parseMaybe (A..: "text")

    createItem :: String -> IO ()
    createItem wordText =
        () <$ execute conn "INSERT INTO words(text) VALUES(?);" [wordText]



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
            wordList <- liftIO $ wordListView conn
            S.json wordList
        S.get "/word/:id" $ do
            wordId    <- Types.WordId <$> S.param "id"
            maybeWord <- liftIO $ wordDetailView conn wordId
            maybe (S.status status404 >> S.text "not found") S.json maybeWord
        S.post "/word" $ do
            body <- S.body
            S.status
                (maybe status400 (const status201) (wordCreateView conn body))


develMain :: IO ()
develMain = webApp >>= (run 8000 . logStdoutDev)
