{-# LANGUAGE OverloadedStrings #-}

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger

import           WebApp                         ( webApp )


-- TODO: errors
main :: IO ()
main = print ("use some dev server" :: String)

-- develMain :: IO () develMain = webApp >>= (\app -> run 8000 $ logStdoutDev app)
