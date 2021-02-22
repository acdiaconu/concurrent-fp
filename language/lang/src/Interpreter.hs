{- 
  Monadic definitional interpreter.
-}

module Interpreter(obey, init_cst, init_env) where

import Parsing
import FunSyntax
import FunParser
import Environment
import CState

import Data.List (intercalate)

import Control.Monad
import Control.Monad.Trans (lift)
import CCExc

import Debug.Trace

-- TODO: A more algebraic approach, to allow easy combination of ``pure"
--       algebraic effects (i.e. create a class for state as a first step)

-- State monad

newtype State s a = State { runS :: s -> (a, s) }

-- Instances to make it a monad

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (State sm) >>= f = State $ \s -> let (a, newS) = sm s
                                       (State rs) = f a 
                                   in rs newS

instance Functor (State s) where
  fmap f (State m) = State $ \s -> let (a, newS) = m s
                                    in (f a, newS)

instance Applicative (State s) where
  pure = return

-- Operations for the state monad

get :: Location -> State CST (CType Value Suspended)
get l = State $ \(CST chs) -> (contents chs l, CST chs)

put :: Location -> CType Value Suspended -> State CST ()
put l ct = State $ \(CST chs) -> ((), CST $ update chs l ct)

new :: State CST Location
new = State $ \(CST chs) -> let (l, chs') = fresh chs in (l, CST chs')

-- Helpers

type Env = Environment Value

newtype CST = CST (ChanState Value Suspended)

type Suspended = CC PromptCX (State CST) Value
type PromptCX = P2 Value Value
type ProgState = (Env, CST)
type Arg = String
type Name = String
type Kont = CC PromptCX (State CST)

-- Value domain

data Value =
    Unit
  | IntVal Integer
  | BoolVal Bool
  | ChanHandler Location
  | Closure Arg Env Expr
  | Injection String [Value]    
  | Tuple [Value]
  | Waiting Location              
  | Exception Value
  | Resume Suspended

-- TODO: custom exceptions -> just use the environment and have custom exc
--                            declarations, and make them
--
--       change the exception mechanism to be the same as ocaml, try ... with 
--       and the pattern matching for the exception

-- Some useful instances

instance Eq Value where
  IntVal a == IntVal b         = a == b
  BoolVal a == BoolVal b       = a == b
  Unit == Unit                 = True
  Exception e1 == Exception e2 = e1 == e2
  _ == _                       = error "Not comparable"

instance Show Value where
  show (IntVal n)          = show n
  show (BoolVal b)         = if b then "true" else "false"
  show (ChanHandler a)     = "<handler " ++ show a ++ ">"
  show (Closure _ _ _)     = "<fundef>"
  show (Exception v )      = "<exception -> " ++ show v ++ ">"
  show (Tuple vs)          = "(" ++ intercalate "," (map show vs) ++ ")"
  show Unit                = "unit"
  show (Injection name vs) = name ++ " " ++ intercalate " " (map show vs)
  show _                   = error "should not be shown"

-- Evaluator

eval :: Expr -> Env -> Kont Value
eval (Number n) _ = return (IntVal n)

eval (Variable v) env = return (find env v)

eval (Apply f e) env = 
  eval f env >>= (\ (Closure id env' body) ->
      eval e env >>= (\ v -> eval body (define env' id v))
  )

eval (If e1 e2 e3) env =
  eval e1 env >>= (\b ->
    case b of
      BoolVal True -> eval e2 env
      BoolVal False -> eval e3 env
      _ -> error "Boolean required in conditional")

eval (Lambda x e1) env = return $ Closure x env e1

eval (Injector name args) env = values evs >>= (\vs -> 
  return $ Injection name vs)
  where evs = map (`eval` env) args

eval (Match ex []) env = return $ Injection "ExcMatch" []
eval (Match ex ((Pattern patCtor pex):pats)) env = 
  eval ex env >>= (\ inj -> 
    case trydefine patCtor inj env of
      Just env' -> eval pex env'
      Nothing -> eval (Match ex pats) env
  )

eval (Let d e1) env =
  elab d env >>= (\env' -> eval e1 env')

eval (Pipe e1 e2) env = 
  eval e1 env >>= (\_ -> eval e2 env)

eval (Send ce ve) env = 
  eval ce env >>= (\(ChanHandler l) -> 
    eval ve env >>= (\v -> 
      shiftP p2L $ \sk -> 
      lift $ get l >>= (\cst -> case cst of 
        Empty   -> put l (WR v $ sk) >>= (\() -> return (Waiting l))
        WW rk   -> put l (Ready $ rk v) >>= (\() -> 
                     return $ Resume (sk Unit)
                   )
        Closed  -> return (Injection "ExcClosed" [])  -- built in exc
        _       -> return (Injection "ExcInvalid" []) -- built in exc
      )
    )
  )

eval (Receive ce) env = 
  eval ce env >>= 
    (\(ChanHandler l) -> 
      shiftP p2L $ \rk -> 
      lift $ get l >>= (\ cst -> case cst of 
          Empty   -> put l (WW $ rk) >>= (\() -> return (Waiting l))
          WR v sk -> put l (Ready $ sk Unit) >>= (\() -> 
                       return $ Resume (rk v)
                     )
          Closed  -> return (Injection "ExcClosed" [])  -- built in exc
          _       -> return (Injection "ExcInvalid" []) -- built in exc
    ))

eval (Parallel es) env = 
  pushPrompt p2R $ 
    interleave ((map (\e -> pushPrompt p2L (eval e env)) es), []) 0

eval (Throw th) env = 
  eval th env >>= (\v -> case v of
    Injection n vs -> shiftP p2R (\k -> return $ Exception v)
    _              -> error "Must throw a sum type")
    

eval NewChan env = lift $  
  new >>= (\l -> 
    put l Empty >>= (\() -> return $ ChanHandler l))

eval (Close c) env = eval c env >>= (\(ChanHandler l) -> 
    lift $ put l Closed >>= (\() -> return Unit))

eval (TryCatch es ef) env = 
  eval es env >>= (\v -> case v of
      Exception e -> (eval ef env >>= (\(Closure x env' body) -> 
                        eval body (define env' x v)))
      _           -> return v
  )

eval (BinPrim bop e1 e2) env = case bop of
  Plus -> eval e1 env >>= (\(IntVal n1) -> 
            eval e2 env >>= (\(IntVal n2) -> 
              return $ IntVal (n1 + n2)))
  Minus ->  eval e1 env >>= (\(IntVal n1) -> 
              eval e2 env >>= (\(IntVal n2) -> 
                return $ IntVal (n1 - n2)))
  Times ->  eval e1 env >>= (\(IntVal n1) -> 
              eval e2 env >>= (\(IntVal n2) -> 
                return $ IntVal (n1 * n2)))
  Div -> eval e1 env >>= (\(IntVal n1) -> 
           eval e2 env >>= (\(IntVal n2) -> 
             return $ IntVal (n1 `div` n2)))
  Mod -> eval e1 env >>= (\(IntVal n1) -> 
           eval e2 env >>= (\(IntVal n2) -> 
             return $ IntVal (n1 `mod` n2)))
  Equal -> eval e1 env >>= (\v1 -> 
            eval e2 env >>= (\v2 -> 
              return $ BoolVal (v1 == v2)))
  And -> eval e1 env >>= (\(BoolVal b1) -> 
          eval e2 env >>= (\(BoolVal b2) -> 
            return $ BoolVal (b1 && b2)))
  Or -> eval e1 env >>= (\(BoolVal b1) -> 
          eval e2 env >>= (\(BoolVal b2) -> 
            return $ BoolVal (b1 || b2)))

eval (MonPrim mop e) env = case mop of
  Neg -> eval e env >>= (\(IntVal n) -> return $ IntVal (-n))

elab :: Defn -> Env -> Kont Env
elab (Val x e) env =
  eval e env >>= (\v -> return (define env x v))
elab (Rec x e) env =
  case e of
    Lambda fp body ->
      return env' where env' = define env x (Closure fp env' body)
    _ ->
      error "RHS of letrec must be a lambda"
elab (Data _ ctors) env = foldM (\env' cdef -> elab cdef env') env ctors

interleave :: ([Kont Value], [Kont Value]) -> Int -> Kont Value
interleave ([], rs) w = if w == 0 
                        then values rs >>= (\vs -> return $ Tuple (reverse vs))
                        else interleave (reverse rs, []) w
interleave ((k:ks), rs) w = k >>= (\v -> case v of 
    Exception e  -> abortP p2R (return $ Exception e)
    Resume res   -> interleave (res:ks, rs) w
    Waiting l    -> lift (get l >>= (\cst -> case cst of 
                      Ready sk -> return (1, sk)
                      _            -> return (2, (return $ Waiting l))
                    )) >>= (\(n, r) -> case n of 
                      1 -> interleave ((r:ks), rs) (w - 1)
                      2 -> interleave (ks, (r:rs)) (w + 1)
                    )
    v            -> interleave (ks, (return v:rs)) w
  )

values [] = return []
values (c:cvs) = c >>= (\v -> values cvs >>= (\vs -> return (v:vs)))

trydefine (VarCtor n vars) (Injection n' vals) env =
  if n == n'
  then Just $ defargs env vars vals
  else Nothing

-- Initial environment, which only exposes primitive data
-- We deal with primitive operations during parsing, by converting them into
-- non-application expressions, similar to OCaml

init_env :: Env
init_env =
  make_env [
    -- some primitive data 
    ("true", BoolVal True), 
    ("false", BoolVal False),
    ("unit", Unit),
    -- some primitive constructors for the exc data
    ("ExcClosed", Injection "ExcClosed" []),
    ("ExcInvalid", Injection "ExcInvalid" []),
    ("ExcMatch", Injection "ExcMatch" [])]

init_cst :: CST
init_cst = CST empty_cst  -- initial empty channel state

-- -- MAIN PROGRAM

-- Deal with top-state exprs and defs

-- Observe the super nice compositionality, how we uncover each layer 
-- independently.
-- This might actually be what Filinski hinted at in his monadic reflection.
obey :: Phrase -> ProgState -> (String, ProgState)
obey (Calculate exp) (env, mem) =
  let (v, mem') = runS (runCC $ eval exp env) mem in 
  (show v, (env, mem'))
obey (Define def) (env, mem) =
  let x = def_lhs def in
  let (env', mem') = runS (runCC $ elab def env) mem in 
  ("", (env', mem'))
