module MTran
    ()
where

import           Control.Monad.State

newtype ReaderContent = RC [Int]
newtype StateContent = SC Int deriving Show


type Interpreter a = ReaderT ReaderContent (State StateContent) a

headDef :: a -> [a] -> a
headDef def = fromMaybe def . headMay

foo :: Interpreter Int
foo = do
    (RC rc) <- ask
    return $ headDef 99 rc

readNum :: Interpreter Text
readNum = do
    (SC sc) <- get
    (RC rc) <- ask
    put $ SC (sum (take sc rc))
    return "foo"


runInterpreter :: Interpreter a -> (a, StateContent)
runInterpreter interpreter =
    flip runState (SC 4) $ runReaderT interpreter (RC [1, 2, 3, 4, 5])
a = runInterpreter foo
b = runInterpreter readNum

