{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lens where


import           Data.Text
import           Control.Lens


data MyWord = MyWord
    { _text :: Text
    , _rating :: Int
    }
    deriving (Show)


data Domain = Domain
    ---{  _name :: Text
    { _description :: Maybe Text
    , _words :: [MyWord]
    , _definingWord :: MyWord
    }
    deriving (Show)
makeLenses ''MyWord
makeLenses ''Domain

animals :: Domain
animals = Domain
    -- {  _name         = "animals"
    { _description  = Nothing
    , _words        = [ MyWord { _text = "bird", _rating = 4 }
                      , MyWord { _text = "cow", _rating = 3 }
                      ]
    , _definingWord = MyWord { _text = "fly", _rating = 5 }
    }



data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo

user1 :: User
user1 = User
    { _name     = "qiao.yifan"
    , _userid   = 103
    , _metadata = UserInfo { _numLogins     = 20
                           , _associatedIPs = ["52.39.193.61", "52.39.193.75"]
                           }
    }



f :: Int
f = view (metadata . numLogins) user1
