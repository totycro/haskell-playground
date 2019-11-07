{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class         ( liftIO )
import           Network.HTTP.Types.Status
import           Data.Maybe

import qualified Web.Scotty                    as S

import           Database.PostgreSQL.Simple

import           InitDB                         ( initDb )
import           Types


-- TODO: tests with hspec wai

-- TODO: errors
main :: IO ()
main = do
    conn <- connect defaultConnectInfo { connectDatabase = "postgres"
                                       , connectHost     = "db"
                                       , connectPassword = "1234"
                                       }
    initDb conn
    S.scotty 8000 $ do
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
            word <- S.jsonData :: S.ActionM MyWord
            -- TODO: 400 on invalid data, not 500
            liftIO
                $   const ()
                <$> execute conn "INSERT INTO words(text) VALUES(?);" word
            S.status status201
