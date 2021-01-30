module Toy where

import Environment
import Debug.Trace
import Control.Monad
import Control.Monad.CC

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
    |   Throw Expr
    |   TryCatch Expr Expr

data Defn = Val String Expr

data Value = 
        Unit
    |   IntVal Int
    |   BoolVal Bool
    |   Closure String Env Expr
    |   Exception Value
    |   Suspended (Value -> Value)

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (Closure _ _ _) = "<closure>"
  show (Unit) = "unit"
  show (Suspended _) = "suspended?"
  show (Exception e) = "exception " ++ show e

type Env = Environment Value

eval :: Expr -> Env -> CC i Value 
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

-- eval Intrerrupt env = shift (\k -> return $ (Suspended k))

-- eval (Parallel es) env = shift (\k -> scheduler (map (\e -> reset (eval e env)) es) k)

-- eval (Throw th) env = eval th env >>= (\v -> shift (\k -> return $ Exception v))

-- eval (TryCatch es ef) env = reset $ shift (\k -> (eval es env)) >>= 
--                                     (\(Exception v) -> eval ef env >>= 
--                                       (\(Closure x env' body) -> 
--                                         eval body (define env' x v) >>= (\v -> return v)))

-- scheduler :: [DCont Value Value] -> (Value -> Value) -> DCont Value Value
-- scheduler [] exit = return (exit Unit)   
-- scheduler (k:ks) exit = k >>= (\v -> case v of 
--     Suspended k -> scheduler (ks ++ [DCont (\k' -> (k'.k) Unit)]) exit
--     Exception v -> shift (\_ -> return $ exit (Exception v))
--     _           -> scheduler ks exit)

elab :: Defn -> Env -> CC i Env 
elab (Val x e) env = 
  eval e env >>= (\v -> return (define env x v))