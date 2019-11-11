{-# LANGUAGE OverloadedStrings #-}

module WebApp where

-- TODO: clean up all imports
import           Control.Exception              ( catchJust
                                                , throw
                                                )
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
import qualified Database.PostgreSQL.Simple.Errors
                                               as PGE

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


data CreationResult = Created | NotCreated
wordCreateView :: Connection -> BSL.ByteString -> IO CreationResult
wordCreateView conn body = maybe (return NotCreated) createItem
    $ parseWordText body
  where
    parseWordText :: BSL.ByteString -> Maybe String
    parseWordText s = A.decode s >>= AT.parseMaybe (A..: "text")

    createItem :: String -> IO CreationResult
    createItem wordText = do
        () <$ execute conn "INSERT INTO words(text) VALUES(?);" [wordText]
        return Created

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
            body           <- S.body
            creationResult <- liftIO $ catchJust PGE.constraintViolation
                                                 (wordCreateView conn body)
                                                 handleUniqueViolation
            -- TODO: error message: unique violation or missing key
            S.status $ case creationResult of
                NotCreated -> status400
                Created    -> status201


        -- TODO: update
        -- TODO: business logic: advanced word properties like providing adjectives for words (tag cloud-like)
        --
handleUniqueViolation :: PGE.ConstraintViolation -> IO CreationResult
handleUniqueViolation e = case e of
    PGE.UniqueViolation _ -> return NotCreated
    _                     -> throw e



develMain :: IO ()
develMain = webApp >>= (run 8000 . logStdoutDev)
