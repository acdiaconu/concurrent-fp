{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    UndecidableInstances, FunctionalDependencies, FlexibleInstances, GADTs #-}

module Toy2 where

import Environment
import Debug.Trace
import Control.Monad
import Control.Monad.CC
import Control.Monad.Identity

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

data Value i = 
        Unit
    |   IntVal Int
    |   BoolVal Bool
    |   Closure String (Env i) Expr
    |   Exception (Value i)
    |   Suspended (CC i (Value i))

instance Show (Value i) where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (Closure _ _ _) = "<closure>"
  show (Unit) = "unit"
  show (Suspended _) = "suspended?"
  show (Exception e) = "exception " ++ show e

type Env i = Environment (Value i)

eval :: Expr -> Env i -> Prompt i (Value i) -> CC i (Value i)
eval (Number n) _ _ = trace ("number" ++ show n) (return (IntVal n))
eval (Variable v) env _ = return (find env v)
eval (Apply f e) env p = eval f env p >>= (\(Closure id env' body) ->
    eval e env p >>= (\v -> eval body (define env' id v) p))
eval (If e1 e2 e3) env p =
  eval e1 env p >>= (\b ->
    case b of
      BoolVal True -> eval e2 env p
      BoolVal False -> eval e3 env p
      _ -> error "boolean required in conditional")
eval (Lambda x e1) env _ = return $ Closure x env e1
eval (Let d e1) env p =
  elab d env p >>= (\env' -> eval e1 env' p)
eval (Pipe e1 e2) env p = 
  eval e1 env p >>= (\_ -> eval e2 env p)

eval Intrerrupt env p = (shift p $ \k -> (return (Suspended $ k (return Unit))))

eval (Parallel es) env p = reset $ \outer -> 
                             interleave (map (\e -> reset $ \each -> (eval e env each)) es) outer

eval (Throw th) env p = eval th env p >>= (\v -> (shift p (\k -> (return $ Exception v))))

eval (TryCatch es ef) env p = eval es env p >>= 
                                (\v -> (case v of
                                  Exception e -> (eval ef env p >>= (\(Closure x env' body) -> 
                                                  eval body (define env' x v) p))
                                  _           -> return v))

interleave :: [CC i (Value i)] -> Prompt i (Value i) -> CC i (Value i) 
interleave [] _ = return Unit 
interleave (k:ks) outer = k >>= (\v -> case v of 
    Suspended k -> interleave (ks ++ [k]) outer
    Exception e -> abort outer (return $ Exception e)
    _           -> interleave ks outer)

elab :: Defn -> Env i -> Prompt i (Value i) -> CC i (Env i)
elab (Val x e) env p = 
  eval e env p >>= (\v -> return (define env x v))

main = y >>= (\v -> case v of
  IntVal n -> return (IntVal n)
  Unit -> return Unit)
  where y = newPrompt >>= (\p -> (eval ex empty_env p))
        ex = TryCatch (Parallel [Pipe (Pipe (Intrerrupt) (Throw (Number 5))) (Number 42), 
                                 Pipe (Pipe (Number 2) ex') (Number 6)]) 
             (Lambda "x" (Number 52))
        ex' = Parallel [TryCatch (Parallel [Pipe (Intrerrupt) (Number 6), Pipe (Pipe (Number 5) (Throw (Number 10))) (Number 6969)]) (Lambda "x" (Number 52)), Number 50] 