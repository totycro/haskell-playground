module Entrypoint where
import           WebApp                         ( webApp )
import qualified Network.Wai.Handler.Warp      as WARP
import qualified Network.Wai.Middleware.RequestLogger
                                               as RL


develMain :: IO ()
develMain = webApp >>= (WARP.run 8000 . RL.logStdoutDev)
