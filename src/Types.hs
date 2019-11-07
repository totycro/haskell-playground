{-# LANGUAGE DeriveGeneric #-}
module Types where

import           GHC.Generics

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

import           Data.Aeson
import           Data.Aeson.Types


newtype MyWord = MyWord {
  text :: String
} deriving (Show, Generic)

instance FromRow MyWord where
    fromRow = MyWord <$> field

instance ToJSON MyWord
instance FromJSON MyWord
