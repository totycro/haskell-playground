{-# LANGUAGE OverloadedStrings #-}

module WebApp where

import           Control.Exception              ( catchJust
                                                , throw
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Network.HTTP.Types.Status
import qualified Data.Maybe                    as M
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as AT
import qualified Data.ByteString.Lazy          as BSL


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
    M.listToMaybe
        <$> (query conn "SELECT id, text FROM words where id = ?;" wordId :: IO
                  [Types.MyWord]
            )


parseWordText :: BSL.ByteString -> Maybe String
parseWordText s = A.decode s >>= AT.parseMaybe (A..: "text")


data CreationResult = Created | NotCreated
wordCreateView :: Connection -> BSL.ByteString -> IO CreationResult
wordCreateView conn body = maybe (return NotCreated) createItem
    $ parseWordText body
  where
    createItem :: String -> IO CreationResult
    createItem wordText = do
        () <$ execute conn "INSERT INTO words(text) VALUES(?);" [wordText]
        return Created


data UpdateResult = Updated | UpdateNotFound | UpdateInvalidData
updateWord :: Connection -> Types.WordId -> BSL.ByteString -> IO UpdateResult
updateWord conn wordId body = case parseWordText body of
    Nothing      -> return UpdateInvalidData
    Just newText -> mapResult <$> execute
        conn
        "UPDATE words SET text = ? where id = ?;"
        (newText, wordId)
  where
    mapResult 0 = UpdateNotFound
    mapResult _ = Updated


data DeletionResult = Deleted | NotFound
deleteWord :: Connection -> Types.WordId -> IO DeletionResult
deleteWord conn wordId =
    mapResult <$> execute conn "DELETE FROM words WHERE id = ?;" wordId
  where
    mapResult 0 = NotFound
    mapResult _ = Deleted


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

        {- NOTE:
            this should use scotty-resource to not define urls multiple times and to
            provide 405 method not allowed on invalid methods, but that package isn't
            really mantained and requires and old version of base.
        -}

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
            -- TODO: response content on 201 (will need to return sth in wordCreateView)
            S.status $ case creationResult of
                NotCreated -> status400
                Created    -> status201

        S.put "/word/:id" $ do
            wordId <- Types.WordId <$> S.param "id"
            body   <- S.body
            result <- liftIO $ updateWord conn wordId body
            S.status $ case result of
                Updated           -> status200
                UpdateNotFound    -> status404
                UpdateInvalidData -> status400


        S.delete "/word/:id" $ do
            wordId <- Types.WordId <$> S.param "id"
            result <- liftIO $ deleteWord conn wordId
            S.status $ case result of
                Deleted  -> status204
                NotFound -> status404


        -- TODO: business logic: advanced word properties like providing adjectives for words (tag cloud-like)
        --
handleUniqueViolation :: PGE.ConstraintViolation -> IO CreationResult
handleUniqueViolation e = case e of
    PGE.UniqueViolation _ -> return NotCreated
    _                     -> throw e
