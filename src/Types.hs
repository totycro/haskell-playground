{-# LANGUAGE DeriveGeneric #-}
module Types where

import           GHC.Generics

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField

import           Data.Aeson



newtype WordId = WordId Integer deriving (Show, Generic, Eq)

instance ToJSON WordId
instance FromJSON WordId
instance FromField WordId where
    fromField f mdata = WordId <$> fromField f mdata
instance ToField WordId where
    toField (WordId wId) = toField wId


data MyWord = MyWord {
  pk :: WordId,
  text :: String
} deriving (Show, Generic)

instance FromRow MyWord where
    fromRow = MyWord <$> field <*> field

instance ToRow MyWord where
    toRow word = [toField $ pk word, toField $ text word]

instance ToJSON MyWord
instance FromJSON MyWord

instance Eq MyWord where
    a == b = (pk a == pk b) && (text (a :: MyWord) == text (b :: MyWord))
