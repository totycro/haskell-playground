{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
module Reader where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )


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

myAsk :: MyReader a a
myAsk = MR id

myAsks :: (e -> a) -> MyReader e a
myAsks = MR


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
fun = myAsk >>= \a -> return (MI $ a + 1)

x :: Int -> MyOtherInt
x = undefined

fun2 :: MyReader Int MyOtherInt
fun2 = do
    a <- myAsks (x)
    return a

fun3 :: MyReader Int MyOtherInt
fun3 = do
    a <- myAsk
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


{-
myReader :: Reader Int Text
--myReader = myAsk >>= \x -> pure $ tshow x
myReader = reader tshow
-}

{-
myReader2 :: Reader Int Text
myReader2 = do
    x <- myAsks (+ 1)
    pure $ runReader myReader x
-}

-- myReader3 :: Reader Int Text
-- myReader3 = (<> " yeah") <$> myReader2

-- myReader4 :: Reader Int Text
-- myReader4 = local (* 10) myReader

{-
myReader5 :: Reader Int Bool
myReader5 = reader (> (0 :: Int))
-}

mySeqA :: (Applicative f) => [f a] -> f [a]
mySeqA []       = pure []
mySeqA (x : xs) = (:) <$> x <*> mySeqA xs
