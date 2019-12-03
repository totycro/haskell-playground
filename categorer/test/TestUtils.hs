module TestUtils
    ( mockResponse
    )
where

import qualified Network.Wreq                  as WREQ
import qualified Network.HTTP.Client.Internal  as HCI
import qualified Network.HTTP.Types            as HT
import qualified Data.ByteString.Lazy          as BL
import           Data.Aeson                     ( encode
                                                , ToJSON
                                                )

mockResponse :: ToJSON d => d -> WREQ.Response BL.ByteString
mockResponse d = HCI.Response
    { HCI.responseStatus    = HT.mkStatus 200 "success"
    , HCI.responseVersion   = HT.http11
    , HCI.responseHeaders   = []
    , HCI.responseBody      = encode d
    , HCI.responseCookieJar = HCI.createCookieJar []
    , HCI.responseClose'    = HCI.ResponseClose (return () :: IO ())
    }
