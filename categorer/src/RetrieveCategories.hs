module RetrieveCategories
    ( retrieveCategories
    , Retrieve
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

type Retrieve = Text -> IO (WREQ.Response BL.ByteString)

retrieveCategories :: Retrieve -> Text -> IO [Text]
retrieveCategories r leWord = do
    response <- r (constructUrl leWord)
    return
        $   response
        ^.. WREQ.responseBody
        .   key "parse"
        .   key "categories"
        .   values
        .   key "*"
        .   _String
