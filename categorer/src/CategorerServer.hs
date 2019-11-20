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
    )
where

import           Servant                        ( QueryParam
                                                , JSON
                                                , (:>)
                                                , Get
                                                , Server
                                                , Proxy(..)
                                                , Application
                                                , serve
                                                )
import           Network.Wai.Handler.Warp       ( run )

type CategorerAPI = "category" :> QueryParam "word" Text :> Get '[JSON] [Text]

categoryServer :: Server CategorerAPI
categoryServer word = return $ (["foo", "bar"] :: [Text])

categorerAPI :: Proxy CategorerAPI
categorerAPI = Proxy

app :: Application
app = serve categorerAPI categoryServer

appMain :: IO ()
appMain = do
    putStrLn $ "Running server on port " <> tshow port
    run port app
    where port = 8000
