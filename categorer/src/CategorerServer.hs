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
import qualified Network.Wreq                  as WREQ
import           Network.Wai.Handler.Warp       ( run )
import           RetrieveCategories             ( Retrieve
                                                , retrieveCategories
                                                )


type CategorerAPI = "category" :> QueryParam "word" Text :> Get '[JSON] [Text]

categoryHandler :: Retrieve -> Maybe Text -> Handler [Text]
-- pass retrieve function to be able to replace it in tests. TODO: possible use reader?
categoryHandler r (Just word) = liftIO $ retrieveCategories r word
categoryHandler _ Nothing     = throwError $ err400
    { errBody =
        "You need to specify a word via query parameter, e.g. \"/category?word=myword\""
    }

categoryServer :: Retrieve -> Server CategorerAPI
categoryServer = categoryHandler

categorerAPI :: Proxy CategorerAPI
categorerAPI = Proxy

app :: Retrieve -> Application
app = serve categorerAPI . categoryServer

appMain :: IO ()
appMain = do
    putStrLn $ "Running server on port " <> tshow port
    run port $ app (WREQ.get . unpack)
    where port = 8000
