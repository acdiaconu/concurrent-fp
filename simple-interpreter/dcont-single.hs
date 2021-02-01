module Toy where

import Environment
import Debug.Trace
import Control.Monad

newtype DCont w a = DCont{ runCont :: (a -> w) -> w } 

instance Monad (DCont w) where
    return x = DCont (\k -> k x)
    DCont m >>= f = DCont (\k -> m (\v -> runCont (f v) k))

instance Functor (DCont w) where
    fmap f (DCont m) = DCont (\k -> m (k . f))

instance Applicative (DCont w) where 
    pure = return
    m <*> a = m >>= \h -> fmap h a

runC :: DCont w w -> w
runC m = runCont m id

reset :: DCont a a -> DCont w a
reset = return . runC

shift :: ((a -> w) -> DCont w w) -> DCont w a
shift f = DCont (runC . f)

data Expr = 
        Number Int
    |   Variable String
    |   Apply Expr Expr
    |   If Expr Expr Expr
    |   Lambda String Expr
    |   Let Defn Expr

    |   Pipe Expr Expr
    |   Intrerrupt           -- should only appear in Parallel, easy to enforce
    |   Parallel [Expr]      -- just 2 components, we don't deal with nested parallelism here

data Defn = Val String Expr

data Value = 
        Unit
    |   IntVal Int
    |   BoolVal Bool
    |   Closure String Env Expr
    |   Suspended (Value -> Value)

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (Closure _ _ _) = "<closure>"
  show (Unit) = "unit"
  show (Suspended _) = "suspended?"

type Env = Environment Value

eval :: Expr -> Env -> DCont Value Value
eval (Number n) env = trace ("number" ++ show n) (return (IntVal n))
eval (Variable v) env = return (find env v)
eval (Apply f e) env = eval f env >>= (\(Closure id env' body) ->
    eval e env >>= (\v -> eval body (define env' id v))) 
eval (If e1 e2 e3) env =
  eval e1 env >>= (\b ->
    case b of
      BoolVal True -> eval e2 env
      BoolVal False -> eval e3 env
      _ -> error "boolean required in conditional")
eval (Lambda x e1) env = return $ Closure x env e1
eval (Let d e1) env =
  elab d env >>= (\env' -> eval e1 env')
eval (Pipe e1 e2) env = 
  eval e1 env >>= (\_ -> eval e2 env)

eval Intrerrupt env = shift (\k -> return $ (Suspended k))

eval (Parallel es) env = shift (\k -> scheduler (map (\e -> reset (eval e env)) es))

scheduler :: [DCont Value Value] -> (Value -> Value) -> DCont Value Value
scheduler []     = return Unit   
scheduler (k:ks) = k >>= (\v -> case v of 
    Suspended k -> scheduler (ks ++ [return (k Unit)])
    _           -> scheduler ks)

elab :: Defn -> Env -> DCont Value Env
elab (Val x e) env = 
  eval e env >>= (\v -> return (define env x v))
