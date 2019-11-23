{-# LANGUAGE RankNTypes #-}
module RetrieveCategories
    ( MonadHttp(..)
    , retrieveCategories
    , RetrieveCategories
    )
where

import           Control.Lens                   ( (^..) )
import qualified Data.ByteString.Lazy          as BL
import           Data.Aeson.Lens                ( _String
                                                , key
                                                , values
                                                )
import qualified Network.Wreq                  as WREQ


constructUrl :: Text -> Text
constructUrl leWord =
    "https://en.wiktionary.org/w/api.php?action=parse&page="
        <> leWord
        <> "&prop=categories&format=json"

class Monad m => MonadHttp m where
    get :: Text -> m (WREQ.Response BL.ByteString)

instance MonadHttp IO where
    get = WREQ.get . unpack

type RetrieveCategories = (forall  m . MonadHttp m => Text -> m [Text])

retrieveCategories :: RetrieveCategories
retrieveCategories leWord = do
    response <- get (constructUrl leWord)
    return
        $   response
        ^.. WREQ.responseBody
        .   key "parse"
        .   key "categories"
        .   values
        .   key "*"
        .   _String
