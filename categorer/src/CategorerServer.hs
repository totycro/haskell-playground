{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CategorerServer
    ( CategorerAPI
    )
where

import           Servant.API                    ( QueryParam
                                                , JSON
                                                , (:>)
                                                , Get
                                                )

type CategorerAPI = "category" :> QueryParam "word" :> Get '[JSON] [Text]
