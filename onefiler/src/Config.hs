module Config
    ()
where

import           System.Directory

possiblePaths :: IO [Text]
possiblePaths = do
    home <- getHomeDirectory
    pure
        [ "/etc/myconf.conf"
        , pack home <> "/.myconf.conf"
        , pack home <> "/.config/myconf.conf"
        ]

newtype Config = Config (Maybe Int) deriving (Show)

instance Semigroup Config where
    _ <> (Config (Just c)) = Config (Just c)
    c <> _                 = c


instance Monoid Config where
    mempty = Config Nothing


getActualConfig1 :: IO Config
getActualConfig1 = do
    possiblePaths' <- possiblePaths
    fileContents   <- sequence (readFileUtf8 . show <$> possiblePaths')
    let possibleConfigs :: [Maybe Int] = readMay <$> fileContents
    let configs                        = Config <$> possibleConfigs
    pure $ mconcat configs


getActualConfig2 :: IO Config
getActualConfig2 = do
    possiblePaths' <- possiblePaths
    mconcat <$> sequence (parseConfig <$> possiblePaths')
  where
    parseConfig :: Text -> IO Config
    parseConfig path =
        Config
            .   readMay
            <$> (ignoreError (traceShowId <$> (readFileUtf8 $ show path)))

    ignoreError :: Monoid a => IO a -> IO a
    ignoreError io = io `catch` (\(_ :: IOError) -> pure mempty)
