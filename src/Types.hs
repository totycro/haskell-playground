{-# LANGUAGE DeriveGeneric #-}
module Types where

import           GHC.Generics

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField

import           Data.Aeson



data MyWord = MyWord {
  pk :: Int,
  text :: String
} deriving (Show, Generic)

instance FromRow MyWord where
    fromRow = MyWord <$> field <*> field

instance ToRow MyWord where
    toRow word = [toField $ pk word, toField $ text (word :: MyWord)]

instance ToJSON MyWord
instance FromJSON MyWord

instance Eq MyWord where
    a == b = (pk a == pk b) && (text (a :: MyWord) == text (b :: MyWord))
