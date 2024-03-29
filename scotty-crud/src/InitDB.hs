{-# LANGUAGE OverloadedStrings #-}

module InitDB where

import           Database.PostgreSQL.Simple     ( Connection
                                                , query_
                                                , execute_
                                                , Only
                                                , SqlError
                                                )


import           Control.Exception              ( catch )
import           Control.Monad                  ( void )


-- TODO: use DB wrapper such as persistent

initDb :: Connection -> IO ()
initDb conn = test `catch` createStuff
  where

    test :: IO ()
    test = void (query_ conn "select 1 from words;" :: IO [Only Int])

    createStuff :: SqlError -> IO ()
    createStuff _ = do
        putStrLn "Creating table"
        _ <- createTable conn
        _ <- insertData conn
        return ()


createTable :: Connection -> IO ()
createTable conn = void $ execute_
    conn
    "CREATE TABLE words (id SERIAL, text TEXT NOT NULL UNIQUE);"


deleteContent :: Connection -> IO ()
deleteContent conn = void $ execute_ conn "DELETE FROM words;"


insertData :: Connection -> IO ()
insertData conn = sequence_ $ execute_ conn <$> queries
  where
    queries =
        [ "INSERT INTO words(text) VALUES('narudo');"
        , "INSERT INTO words(text) VALUES('epur');"
        , "INSERT INTO words(text) VALUES('pisara');"
        ]
