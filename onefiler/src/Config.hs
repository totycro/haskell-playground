module Config
    ()
where

import           ClassyPrelude

possiblePaths :: [Text]
possiblePaths = ["/etc/myconf.conf", "~/.myconf.conf", "~/.config/myconf.conf"]


newtype Config = Config (Maybe Int)

instance Semigroup Config where
    (Config _      ) <> (Config (Just c)) = Config (Just c)
    (Config Nothing) <> d                 = d
--instance Monoid Config
