{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module A
    ()
where

-- tell :: w -> Writer w ()

--runStateX :: State s a -> s -> (a, s)
--evalState :: State s a -> s -> a
--execState :: State s a -> s -> s

newtype XState s a = XState { runStateX :: s -> (a,s) }

evalState :: XState s a -> s -> a
evalState (XState run) s = fst (run s)



instance Functor (XState s) where
    fmap f (XState run) = XState (mapFst f . run)
        where mapFst g (a, b) = (g a, b)

instance Applicative (XState s) where
    pure x = XState $ \s -> (x, s)
    mf <*> ma =
        XState
            (\s -> let (fa, fs) = runStateX mf s in runStateX (fa <$> ma) fs)

instance Monad (XState s) where
    return = pure
    ma >>= f =
        XState (\s -> let (aa, as) = runStateX ma s in runStateX (f aa) as)


get :: XState s s
get = XState (\s -> (s, s))

put :: s -> XState s ()
put s = XState $ const ((), s)

modify :: (s -> s) -> XState s ()
modify f = XState (\s -> ((), f s))

modify' :: (s -> s) -> XState s ()
modify' f = do
    s <- get
    put $ f s

test :: XState Integer Integer
test = do
    put 3
    modify (+ 1)
    get


-- put
-- get
-- modify

newtype XWriter w a = XWriter { runWriter :: (a, w) } deriving (Show)

instance Functor (XWriter w) where
    fmap f ma = XWriter (f a, w) where (a, w) = runWriter ma

instance (Monoid w) => Applicative (XWriter w) where
    pure a = XWriter (a, mempty)
    mf <*> ma = XWriter (fa aa, wf `mappend` wa)
      where
        (aa, wa) = runWriter ma
        (fa, wf) = runWriter mf

instance (Monoid w) => Monad (XWriter w) where
    return = pure
    ma >>= f = XWriter (afa, wa `mappend` wfa)
      where
        (aa , wa ) = runWriter ma
        (afa, wfa) = runWriter $ f aa

tell :: w -> XWriter w ()
tell w = XWriter ((), w)

execWriter :: XWriter w a -> w
execWriter wr = snd $ runWriter wr

runWriterY :: XWriter w a -> (a, w)
runWriterY (XWriter (a, w)) = (a, w)


newtype XReader r a = XReader { runXReader :: r -> a }

    {-
asks :: (r -> a) -> XReader r a
local :: (r -> r) -> XReader r a -> XReader r a
runXReader :: XReader r a -> r -> a
-}


instance Functor (XReader r) where
    fmap f ma = XReader (f . runXReader ma)


instance Applicative (XReader r) where
    pure x = XReader (const x)
    (<*>) mf ma = XReader (\r -> (runXReader mf r) (runXReader ma r))


instance Monad (XReader r) where
    return = pure

    --(>>=) :: forall a b . XReader r a -> (a -> XReader r b) -> XReader r b
    ma >>= f = XReader (\r -> runXReader (f (runXReader ma r)) r)

ask :: XReader r r
ask = XReader id

asks :: (r -> a) -> XReader r a
asks f = XReader (\r -> f r)

local :: (r -> r) -> XReader r a -> XReader r a
local f mr = XReader (runXReader mr . f)
-- local f mr = XReader (\r -> runXReader mr (f r))

toString :: XReader Int String
toString = do
    n <- A.ask
    return $ show n



{-
-- newtype XXReader r a = XXReader { runXReader :: r -> a }


xseq :: Monad m => [m a] -> m [a]
xseq []         = return []
--xseq (ma : mas) = ma >>= (\a -> (xseq mas) >>= (\as -> return (a : as)))
xseq (ma : mas) = do
    a  <- ma
    as <- xseq mas
    pure (a : as)

xseq' :: [Maybe a] -> Maybe [a]
xseq' []               = return []
xseq' ((Just a) : mas) = xseq' mas >>= \as -> return $ a : as
xseq' (Nothing  : mas) = xseq' mas
-}
