{-# LANGUAGE OverloadedStrings #-}

import           Database.PostgreSQL.Simple

import           InitDB                         ( initDb )
import           Types                          ( MyWord(MyWord) )

--import Web.Scotty
--
unOnly :: Only a -> a
unOnly (Only str) = str


unnecessaryLambda :: Only a -> a
unnecessaryLambda = \(Only str) -> str


main :: IO ()
main = do
    conn <- connect defaultConnectInfo { connectDatabase = "postgres"
                                       , connectHost     = "db"
                                       , connectPassword = "1234"
                                       }
    putStrLn "here"
    initDb conn
    putStrLn "other"
    word <- query_ conn "select word from words;" :: IO [Only String]
    putStrLn
        $  "items plain: "
        <> ((foldr (\a b -> a <> "; " <> b) "" $ unOnly <$> word) :: String)
    word' <- query_ conn "select word from words;" :: IO [Only String]
    putStrLn $ "items parsed : " <> show (MyWord . unOnly <$> word')
    word'' <- query_ conn "select word from words;" :: IO [MyWord]
    print word''
    mapM_ print =<< (query_ conn "select * from words;" :: IO [Only String])
  -- scotty 8000 $ do
  {- get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Beam, ", beam, " me up!</h1>"]
    -}
