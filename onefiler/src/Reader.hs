{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
module Reader where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )

--import           Control.Monad.Reader           ( MyReader , ask , runReader)


newtype MyReader r a = MR { myRunReader :: r -> a }

instance Functor (MyReader r) where
    -- fmap :: (a -> b) -> MyReader r a -> MyReader r b
    --fmap f MyReader = MR $ f . myRunReader reader
    -- fmap f (MR a) = MR $ f . a
    fmap f reader = MR $ \input -> f (myRunReader reader input)

instance Applicative (MyReader r) where
    pure item = MR $ const item
    -- (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
    readerWithFun <*> readerA = MR $ \input ->
        (myRunReader readerWithFun input) (myRunReader readerA input)

instance Monad (MyReader r) where
    return = pure
    readerA >>= funX =
        MR $ \input -> myRunReader (funX (myRunReader readerA input)) input

ask :: MyReader a a
ask = MR id

asks :: (e -> a) -> MyReader e a
asks = MR


foo :: MyReader c (IO c)
foo = MR (\a -> return a)


r1 = return 4 :: MyReader String Int
r2 = myRunReader r1 "foo"


mr1 = MR (\a -> 4) :: MyReader String Int
mr2 = myRunReader mr1 "foo"


newtype MyOtherInt = MOI Int deriving (Show)
newtype MyInt a = MI a deriving (Show)
instance Functor MyInt where
    fmap f (MI x) = MI $ f x

miInc (MI x) = MI $ x + 1

miInc2 :: MyInt Int -> MyInt Int
miInc2 = fmap (+ 1)


fun :: MyReader Int (MyInt Int)
fun = ask >>= \a -> return (MI $ a + 1)

x :: Int -> MyOtherInt
x = undefined

fun2 :: MyReader Int MyOtherInt
fun2 = do
    a <- asks (x)
    return a

fun3 :: MyReader Int MyOtherInt
fun3 = do
    a <- ask
    return $ MOI a

fen :: MyReader Int (MyInt Int)
fen = fun

fin :: MyReader Int (MyInt Int)
fin = fun >>= (\a -> return $ miInc2 a)

fon = myRunReader fun
fan = do
    a <- fen
    b <- fan
    c <- fin
    return $ a



--ask :: MyReader r a => a r ask = kkk

{-
class Monad m => MyReader r m | m -> r where
    myAsk :: m r
    myAsk = _

-- Imagine this is a directory
type Config = FilePath

load :: (MyReader Config m, MonadIO m) => String -> m String
load x = do
    config <- myAsk
    liftIO $ readFile (config ++ x)

loadRevision :: (MyReader Config m, MonadIO m) => Int -> m String
loadRevision x = load ("history" ++ show x ++ ".txt")

loadAll :: (MyReader Config m, MonadIO m) => Int -> String -> m (String, String)
loadAll x y = do
    a <- load y
    b <- loadRevision x
    return (a, b)
-}


myReader :: Reader Int Text
--myReader = ask >>= \x -> pure $ tshow x
myReader = reader tshow

myReader2 :: Reader Int Text
myReader2 = do
    x <- asks (+ 1)
    pure $ runReader myReader x

myReader3 :: Reader Int Text
myReader3 = (<> " yeah") <$> myReader2

myReader4 :: Reader Int Text
myReader4 = local (* 10) myReader

myReader5 :: Reader Int Bool
myReader5 = reader (> (0 :: Int))

mySeqA :: (Applicative f) => [f a] -> f [a]
mySeqA []       = pure []
mySeqA (x : xs) = (:) <$> x <*> mySeqA xs
