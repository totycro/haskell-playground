{-# LANGUAGE OverloadedStrings #-}

module WebApp where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception              ( catch )
import           Network.HTTP.Types.Status
import           Data.Maybe

-- TODO; not have this here?
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger

import qualified Web.Scotty                    as S

import           Database.PostgreSQL.Simple

import           InitDB                         ( initDb )
import           Types

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
                trace (show e) (liftIO $ print "error handler reached")
                S.text e
            )
        S.get "/word" $ do
            items <- liftIO
                (query_ conn "SELECT text FROM words;" :: IO [MyWord])
            S.json items

        S.get "/word/:id" $ do
            wordId <- S.param "id"
            item   <-
                liftIO
                $   listToMaybe
                <$> (query conn
                           "SELECT text FROM words where id = ?;"
                           [wordId :: Integer] :: IO [MyWord]
                    )
            maybe (S.status status404 >> S.text "not found") S.json item

        S.post "/word" $ do
            liftIO $ print "here"
            -- (possibly deal with Scotty error here and transform this string error from aeson to a bad request error )
            -- (tests!!)
            --word <- ((Left (S.jsonData :: S.ActionM MyWord)) `catch` (\a -> Right a)) :: Either Int Int
            word <- (S.jsonData :: S.ActionM MyWord)

            liftIO $ putStrLn "foo"
            liftIO $ print "there"
            -- TODO: 400 on invalid data, not 500
            liftIO
                $  ()
                <$ execute conn "INSERT INTO words(text) VALUES(?);" word
            S.status status201


develMain :: IO ()
develMain = webApp >>= (run 8000 . logStdoutDev)
