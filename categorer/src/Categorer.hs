{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module Categorer
    ( appMain
    )
where

import           Options.Generic

data WordCategoryOptions = WCO
    { word :: Text <?> "Search for categories of this word"
    }
    deriving (Show, Generic)

instance ParseRecord WordCategoryOptions

appMain :: IO ()
appMain = do
    wco <- getRecord "Categorer"
    print (wco :: WordCategoryOptions)
