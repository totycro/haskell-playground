{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StackMachine
    ()
where

import           Control.Monad.Writer
import           Control.Monad.Reader
import           Control.Monad.State

type Stack = [Int]
type Output = [Int]
type Program = [Instr]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a = Comp { unComp :: VM a }
  deriving (MonadReader Program, MonadWriter Output, MonadState Stack)

instance Functor Comp where
    fmap f (Comp vm) = Comp $ fmap f vm
instance Applicative Comp where
    pure x = Comp (pure x)
    (Comp f) <*> (Comp a) = Comp (f <*> a)
instance Monad Comp where
    return = pure
    (Comp vm) >>= f = Comp $ do
        a <- vm
        unComp $ f a


data Instr = Push Int | Pop | Puts


getVMStack :: VM Stack
getVMStack = lift (lift get)

evalInstr :: Instr -> Comp ()
evalInstr (Push val) = modify (val :)
evalInstr Puts       = do
    curState <- get
    case headMay curState of
        (Just x) -> tell [x]
        Nothing  -> tell []
evalInstr Pop = modify (fromMaybe [] . tailMay)

eval :: Comp ()
eval = do
    prog <- ask
    case prog of
        []       -> return ()
        (i : is) -> do
            evalInstr i
            local (const is) eval

execVM :: Program -> Output
execVM prog = evalState (execWriterT (runReaderT (unComp eval) prog)) []


{-
execProgram = do
    program <- ask
    case program of
        []              -> undefined -- get writer ouptu
        (Push val) : xs -> do
            modify (val :)
            execProgram xs
-}


program :: Program
program = [Push 42, Push 27, Puts, Pop, Puts, Pop]

main :: IO ()
main = Prelude.mapM_ print $ execVM program
