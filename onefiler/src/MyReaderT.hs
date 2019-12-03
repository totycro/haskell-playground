module MyReaderT
    ()
where

type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

data Expr
  = Val Int
  | Add Expr Expr
  | Var String
  deriving (Show)

eval :: Expr -> Eval Int
eval ex = case ex of
    (Val val  ) -> pure val
    (Add e1 e2) -> do
        val1 <- eval e1
        val2 <- eval e2
        pure $ val1 + val2
    (Var varName) -> do
        env <- ask
        let varEntry = find (\entry -> fst entry == varName) env
        lift $ snd <$> varEntry

run :: Expr -> Env -> Maybe Int
run ex env = runReaderT (eval ex) env

x1 = Val 1
x2 = Var "foo"
x3 = Add (Var "foo") (Val 3)
