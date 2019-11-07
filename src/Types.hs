module Types where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

newtype MyWord = MyWord {
  text :: String
} deriving (Show)


instance FromRow MyWord where
    fromRow = MyWord <$> field
