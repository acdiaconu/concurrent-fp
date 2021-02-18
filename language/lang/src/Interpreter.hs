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

init_cst = CST empty_cst  -- initial empty channel state
init_env = empty_env      -- initial empty environment

-- Value domain

data Value =
    Unit
  | IntVal Integer
  | BoolVal Bool
  | ChanHandler Location
  | Closure Arg Env Expr
  | Injection String [Value]    
  | Tuple [Value]
  | Waiting                    
  | Exception Value
  | Resume Suspended Suspended  -- resume a write and a read

-- Some useful instances

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (ChanHandler a) = "<chan handler " ++ show a ++ ">"
  show (Closure _ _ _) = "<closure>"
  show (Exception v ) = "<exception -> " ++ show v ++ ">"
  show (Tuple vs) = "(" ++ intercalate "," (map show vs) ++ ")"
  show Unit = "unit"
  show (Injection name vs) = name ++ " " ++ intercalate " " (map show vs)

-- Evaluator

eval :: Expr -> Env -> CC PromptCX (State CST) Value
eval (Number n) _ = return (IntVal n)

eval (Variable v) env = return (find env v)

eval (Apply f e) env = 
  eval f env >>= (\ (Closure id env' body) ->
    eval e env >>= (\ v -> eval body (define env' id v)))

eval (If e1 e2 e3) env =
  eval e1 env >>= (\b ->
    case b of
      BoolVal True -> eval e2 env
      BoolVal False -> eval e3 env
      _ -> error "boolean required in conditional")

eval (Lambda x e1) env = return $ Closure x env e1

eval (Injector name args) env = values evs >>= (\vs -> 
  return $ Injection name vs)
  where evs = map (`eval` env) args

eval (Match ex []) env = return $ Exception Unit
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
      lift $ get l >>= (\ cst -> case cst of 
          Empty   -> put l (WR v $ sk) >>= (\() -> return Waiting)
          WW rk   -> put l Empty >>= (\() -> return $ Resume (rk v) (sk Unit))
          WR _ _  -> error "not allowed"
  )))

eval (Receive ce) env = 
  eval ce env >>= 
    (\(ChanHandler l) -> 
      shiftP p2L $ \rk -> 
      lift $ get l >>= (\ cst -> case cst of 
          Empty   -> put l (WW $ rk) >>= (\() -> return Waiting)
          WR v sk -> put l Empty >>= (\() -> return $ Resume (sk Unit) (rk v))
          WW _    -> error "not allowed"
    ))

eval (Parallel es) env = 
  pushPrompt p2R $ 
    interleave (map (\e -> pushPrompt p2L (eval e env)) es) [] 0

eval (Throw th) env = 
  eval th env >>= (\v -> 
    shiftP p2R (\k -> return $ Exception v))

eval NewChan env = lift $  
  new >>= (\l -> 
    put l Empty >>= (\() -> return $ ChanHandler l))

eval (TryCatch es ef) env = 
  eval es env >>= 
    (\v -> (case v of
      Exception e -> (eval ef env >>= (\(Closure x env' body) -> 
                        eval body (define env' x v)))
      _           -> return v))

elab :: Defn -> Env -> CC PromptCX (State CST) Env
elab (Val x e) env =
  eval e env >>= (\v -> return (define env x v))
elab (Rec x e) env =
  case e of
    Lambda fp body ->
      return env' where env' = define env x (Closure fp env' body)
    _ ->
      error "RHS of letrec must be a lambda"
elab (Data _ ctors) env = foldM (\env' cdef -> elab cdef env') env ctors

interleave :: [CC PromptCX (State CST) Value] -> [Value] -> Int ->
              CC PromptCX (State CST) Value
interleave [] rs w = if w == 0 
                     then return (Tuple rs)
                     else error "detected deadlock" 
interleave (k:ks) rs w = k >>= (\v -> case v of 
    Exception e  -> abortP p2R (return $ Exception e)
    Resume k1 k2 -> interleave ((k1:ks) ++ [k2]) rs (w - 1)
    Waiting      -> interleave ks rs (w + 1)
    -- otherwise, final value, add to final list
    v            -> interleave ks (v:rs) w
  )

values [] = return []
values (c:cvs) = c >>= (\v -> values cvs >>= (\vs -> return (v:vs)))

trydefine (VarCtor n vars) (Injection n' vals) env =
  if n == n'
  then Just $ defargs env vars vals
  else Nothing

-- Initial environment, TODO

-- init_env :: Env
-- init_env =
--   make_env [ constant "true" (BoolVal True), constant "false" (BoolVal False),
--     pureprim "+" (\ (IntVal a) -> Function (\(IntVal b) -> result $ IntVal (a + b)))]
--     -- pureprim "-" (\ (IntVal a) (IntVal b) -> IntVal (a - b)),
--     -- pureprim "*" (\ (IntVal a) (IntVal b) -> IntVal (a * b)),
--     -- pureprim "div" (\ (IntVal a) (IntVal b) ->
--     --   if b == 0 then error "Dividing by zero" else IntVal (a `div` b)),
--     -- pureprim "mod" (\ (IntVal a) (IntVal b) ->
--     --   if b == 0 then error "Dividing by zero" else IntVal (a `mod` b)),
--     -- pureprim "~" (\ [IntVal a] -> IntVal (- a)),
--     -- pureprim "<" (\ (IntVal a) (IntVal b) -> BoolVal (a < b)),
--     -- pureprim "<=" (\ (IntVal a) (IntVal b) -> BoolVal (a <= b)),
--     -- pureprim ">" (\ (IntVal a) (IntVal b) -> BoolVal (a > b)),
--     -- pureprim ">=" (\ (IntVal a) (IntVal b) -> BoolVal (a >= b)),
--     -- pureprim "=" (\ a b -> BoolVal (a == b)),
--     -- pureprim "<>" (\ a b -> BoolVal (a /= b)),
--     -- pureprim "integer" (\ a ->
--     --   case a of IntVal _ -> BoolVal True; _ -> BoolVal False),
--     -- pureprim "head" (\ (Cons h t) -> h),
--     -- pureprim "tail" (\ (Cons h t) -> t),
--     -- pureprim ":" (\ a b -> Cons a b),
--     -- pureprim "list" (\ xs -> foldr Cons Nil xs),
--     -- primitive "print" (\ v -> output (show v) $> (\ () -> result v))]
--     where
--     constant x v = (x, v)
--     primitive x f = (x, Function (primwrap x f))
--     pureprim x f = (x, Function (primwrap x (\args -> result (f args))))

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
  ({-print_defn env' x-}"", (env', mem'))
