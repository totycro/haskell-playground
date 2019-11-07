{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty                    as S
import           Database.PostgreSQL.Simple
import           Control.Monad.IO.Class         ( liftIO )

import           InitDB                         ( initDb )
import           Types


-- TODO: tests with hspec wai

main :: IO ()
main = do
    conn <- connect defaultConnectInfo { connectDatabase = "postgres"
                                       , connectHost     = "db"
                                       , connectPassword = "1234"
                                       }
    initDb conn
    S.scotty 8000 $ do
        S.get "/words" $ do
            items <- liftIO ( query_ conn "SELECT * FROM words;" :: IO [MyWord])
            S.json items

        S.post "/words" undefined
