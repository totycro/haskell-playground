{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module CategorerServer
    ( CategorerAPI
    , appMain
    , app
    )
where

import           Prelude                 hiding ( Handler )
import           Servant                        ( QueryParam
                                                , JSON
                                                , (:>)
                                                , Get
                                                , Server
                                                , Proxy(..)
                                                , Application
                                                , serve
                                                , throwError
                                                , errBody
                                                , err400
                                                , Handler
                                                )
import           Network.Wai.Handler.Warp       ( run )

import           RetrieveCategories             ( retrieveCategories )

type CategorerAPI = "category" :> QueryParam "word" Text :> Get '[JSON] [Text]

categoryHandler :: Maybe Text -> Handler [Text]
categoryHandler (Just word) = liftIO $ retrieveCategories word
categoryHandler Nothing     = throwError $ err400
    { errBody =
        "You need to specify a word via query parameter, e.g. \"/category?word=myword\""
    }

categoryServer :: Server CategorerAPI
categoryServer = categoryHandler

categorerAPI :: Proxy CategorerAPI
categorerAPI = Proxy

app :: Application
app = serve categorerAPI categoryServer

appMain :: IO ()
appMain = do
    putStrLn $ "Running server on port " <> tshow port
    run port app
    where port = 8000
