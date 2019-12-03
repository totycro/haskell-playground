{-# LANGUAGE ScopedTypeVariables #-}
module MonadT
    ()
where

newtype MyMT m a = MT { runMT :: m ( Maybe a ) }

instance (Monad m) => Functor (MyMT m) where
    fmap f (MT r) = MT $ do
        ma <- r
        return (f <$> ma)

instance (Monad m) => Applicative (MyMT m) where
    pure a = MT $ return (Just a)
    (MT mmf) <*> (MT mma) = MT $ do
        mf <- mmf
        ma <- mma
        return $ do
            f <- mf
            a <- ma
            Just $ f a


opx :: (Applicative m) => m (Maybe (a -> b)) -> m (Maybe a -> Maybe b)
opx = ((<*>) <$>)

op :: Applicative m => MyMT m (a -> b) -> MyMT m a -> MyMT m b
--op (MT fab) (MT mma) = MT $ ((<*>) <$> fab) <*> mma
op (MT fab) (MT mma) = MT $ opx fab <*> mma
--  m (Maybe (a -> b)) -> m (Maybe a) -> MyMT m b

instance (Monad m) => Monad (MyMT m) where
    return = pure
    (MT mma) >>= f = MT $ do
        ma <- mma
        case ma of
            Nothing  -> return Nothing
            Just (a) -> runMT $ f a


readStuff :: MyMT IO Text
readStuff = MT $ do
    l <- getLine
    return $ if length l < 3 then (Just l) else Nothing


ax :: IO (Maybe (Text, Text, Text))
ax = runMT $ do
    x <- readStuff
    y <- readStuff
    z <- readStuff
    return $ (x, y, z)
