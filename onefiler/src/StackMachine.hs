{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module StackMachine
    ()
where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State

type Stack = [Int]
type Output = [Int]
type Program = [Instr]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a = Comp { unComp :: VM a }
  deriving ( MonadReader Program, MonadWriter Output, MonadState Stack)

deriving instance Functor Comp
deriving instance Applicative Comp
deriving instance Monad Comp

data Instr = Push Int | Pop | Puts


execVM :: Program -> Output
execVM = undefined


program :: Program
program = [Push 42, Push 27, Puts, Pop, Puts, Pop]

main :: IO ()
main = Prelude.mapM_ print $ execVM program
