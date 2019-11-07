{-# LANGUAGE DeriveGeneric #-}
module Types where

import           GHC.Generics

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField

import           Data.Aeson
import           Data.Aeson.Types


newtype MyWord = MyWord {
  text :: String
} deriving (Show, Generic)

instance FromRow MyWord where
    fromRow = MyWord <$> field

instance ToRow MyWord where
    toRow word = [toField $ text word]

instance ToJSON MyWord
instance FromJSON MyWord
