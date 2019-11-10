{-# LANGUAGE DeriveGeneric #-}

module HelperTypes where

import           GHC.Generics

import           Data.Aeson


-- Used for receiving new words (they don't come with ids)

newtype TextOnly = TextOnly { text :: String } deriving (Generic)
instance FromJSON TextOnly
