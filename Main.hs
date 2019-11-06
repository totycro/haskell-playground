{-# LANGUAGE OverloadedStrings #-}

import Control.Exception

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

--import Web.Scotty

newtype Word = Word {
  text :: String
} deriving (Show)

instance FromRow Main.Word where
    fromRow = Main.Word <$> field

unOnly :: Only a -> a
unOnly (Only str) = str

unnecessaryLambda :: Only a -> a
unnecessaryLambda  = \(Only str) -> str


initDb :: Connection -> IO ()
initDb conn = test `catch` createStuff
    where
        test :: IO ()
        test = (query_ conn "select 1 from words;" :: IO [Only Int]) >> putStrLn "NOT creating table"

        createStuff :: SqlError -> IO ()
        createStuff _ = do
            putStrLn "Creating table"
            _ <- execute_ conn "create table words (word text);"
            _ <- execute_ conn "insert into words values('narudo');"
            _ <- execute_ conn "insert into words values('epur');"
            return ()


main :: IO ()
main = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "postgres",
    connectHost = "db",
    connectPassword = "1234"
  }
  putStrLn "here"
  --_ <- execute_ conn "create table words (word text);"
  -- TODO: apparently have to use monad transfomers to handle failing IO
  -- https://stackoverflow.com/questions/52016330/haskell-common-pattern-to-deal-with-failure-inside-io-io-either-string-int
  initDb conn
  putStrLn "other"
  word <- query_ conn "select word from words;" :: IO [Only String]
  putStrLn $ "items plain: " <>  ((foldr (\a b -> a <> "; " <> b) "" $ unOnly <$> word) :: String)
  word' <- query_ conn "select word from words;" :: IO [Only String]
  putStrLn $ "items parsed : " <>  show ( Word . unOnly <$> word' ) 
  word'' <- query_ conn "select word from words;" :: IO [Main.Word]
  print word''
  mapM_ print =<< ( query_ conn "select * from words;" :: IO [Only String] )
  -- scotty 8000 $ do
  {- get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Beam, ", beam, " me up!</h1>"]
    -}
