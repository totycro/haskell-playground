{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module Categorer
    ( appMain
    )
where

import           Control.Lens

import           Options.Generic
import           Data.Aeson.Lens                ( _String
                                                , key
                                                , values
                                                )
import qualified Network.Wreq                  as WREQ

data WordCategoryOptions = WCO
    { word :: Text <?> "Search for categories of this word"
    }
    deriving (Show, Generic)

instance ParseRecord WordCategoryOptions

constructUrl :: Text -> Text
constructUrl leWord =
    "https://en.wiktionary.org/w/api.php?action=parse&page="
        <> leWord
        <> "&prop=categories&format=json"

retrieveCategories :: Text -> IO [Text]
retrieveCategories leWord = do
    response <- WREQ.get $ unpack $ constructUrl leWord
    return
        $   response
        ^.. WREQ.responseBody
        .   key "parse"
        .   key "categories"
        .   values
        .   key "*"
        .   _String

appMain :: IO ()
appMain = do
    wco <- getRecord "Categorer"
    let leWord = unHelpful $ word (wco :: WordCategoryOptions)
    categories <- retrieveCategories leWord

    putStrLn $ "Categories of " <> leWord <> ":"
    mapM_ putStrLn categories
