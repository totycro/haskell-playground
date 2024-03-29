{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Categorer
    ( appMain
    )
where

import           RetrieveCategories             ( retrieveCategories )

import           Options.Generic                ( getRecord
                                                , ParseRecord
                                                , type (<?>)
                                                , unHelpful
                                                )
import qualified Network.Wreq                  as WREQ
data WordCategoryOptions = WCO
    { word :: Text <?> "Search for categories of this word"
    }
    deriving (Show, Generic)

instance ParseRecord WordCategoryOptions

appMain :: IO ()
appMain = do
    wco <- getRecord "Categorer"
    let leWord = unHelpful $ word (wco :: WordCategoryOptions)
    -- TODO: timing information about how long the request took
    categories <- retrieveCategories (WREQ.get . unpack) leWord

    putStrLn $ "Categories of " <> leWord <> ":"
    putStr $ unlines categories
