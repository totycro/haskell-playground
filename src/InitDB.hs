module InitDB where

import           Database.PostgreSQL.Simple     ( Connection
                                                , query_
                                                , execute_
                                                , Only
                                                , SqlError
                                                )


import           Control.Exception              ( catch )

initDb :: Connection -> IO ()
initDb conn = test `catch` createStuff
  where

    test :: IO ()
    test = (query_ conn "select 1 from words;" :: IO [Only Int])
        >> putStrLn "NOT creating table"


    createStuff :: SqlError -> IO ()
    createStuff _ = do
        putStrLn "Creating table"
        _ <- execute_ conn "create table words (id SERIAL, text TEXT NOT NULL);"
        _ <- execute_ conn "insert into words(text) values('narudo');"
        _ <- execute_ conn "insert into words(text) values('epur');"
        return ()
