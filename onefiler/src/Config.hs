{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config
    ()
where

import           System.Directory

possiblePaths :: IO [FilePath]
possiblePaths = do
    home <- getHomeDirectory
    pure
        [ "/etc/myconf.conf"
        , home <> "/.myconf.conf"
        , home <> "/.config/myconf.conf"
        ]

newtype Config = Config (Maybe Int) deriving (Show)

instance Semigroup Config where
    _ <> (Config (Just c)) = Config (Just c)
    c <> _                 = c


instance Monoid Config where
    mempty = Config Nothing

ignoreError :: Monoid a => IO a -> IO a
ignoreError io = io `catch` (\(_ :: IOError) -> pure mempty)

-- reads all files and then maps the parsing
getActualConfig1 :: IO Config
getActualConfig1 = do
    possiblePaths' <- possiblePaths
    fileContents   <- sequence (readFileUtf8 . show <$> possiblePaths')
    let possibleConfigs :: [Maybe Int] = readMay <$> fileContents
    let configs                        = Config <$> possibleConfigs
    pure $ mconcat configs


-- Similar to getActualConfig1, but parses each file individually, then composes
getActualConfig2 :: IO Config
getActualConfig2 = do
    possiblePaths' <- possiblePaths
    mconcat <$> sequence (parseConfig <$> possiblePaths')
  where
    parseConfig :: FilePath -> IO Config
    parseConfig path = Config . readMay <$> ignoreError (readFileUtf8 path)


class HasLempty a where
    lempty :: a

newtype Last a = Last { unLast :: a } deriving (Functor )

instance Applicative Last where
    pure = Last
    (Last mf) <*> (Last a) = Last (mf a)

instance Monad Last where
    return = pure
    (Last l) >>= f = f l


instance HasLempty Config where
    lempty = Config Nothing


instance (HasLempty a, Eq a) => Semigroup (Last a) where
    l <> l'@(Last la) | la == lempty = l
                      | otherwise    = l'

instance (HasLempty a, Eq a) => Monoid (Last a) where
    mempty = Last lempty

-- Similar to getActualConfig2, but doesn't rely on config monoid
getActualConfig3 :: IO Config
getActualConfig3 = do
    possiblePaths' <- possiblePaths
    unLast $ mconcat <$> sequence (Last . parseConfig <$> possiblePaths')
  where
    parseConfig :: FilePath -> IO Config
    parseConfig path = Config . readMay <$> ignoreError (readFileUtf8 path)
