module Interpreter(dialog, init_mem, empty_env, funParser) where

import Parsing
import FunSyntax
import FunParser
import Environment
import Memory

import Control.Monad
import Control.Monad.CC

import Debug.Trace

-- State monad

-- newtype State s a = State { runState :: s -> (a, s) }  

-- instance Monad (State s) where
--   return a = State $ \s -> (a, s)
--   (State sm) >>= f = State $ \s -> let (a, newS) = sm s
--                                        (State rs) = f a 
--                                    in newS rs

-- new :: State Location
-- new (mem, res) (Cont kx) (Cont ks) = let (a, mem') = fresh mem in ks a (mem', res)

-- get :: Location -> State (CState Env Value (Cont Value))
-- get a (mem, resume) (Cont kx) (Cont ks) = ks (contents mem a) (mem, resume)

-- put :: Location -> CState Env Value (Cont Value) -> M ()
-- put a v (mem, resume) (Cont kx) (Cont ks) = ks () (update (mem, resume) a v)

-- SEMANTIC DOMAINS

type Env i = Environment (Value i)
type Chs i = ChansState (Value i)

data Value i = 
    Unit
  | IntVal Integer
  | BoolVal Bool
  | ChanHandler Location
  | Closure String (Env i) Expr
  | WaitRecvVal (Value i)
  | WaitSendVal (Value i) (Value i)
  | Exception (Value i)
  | Suspended (CC i (Value i))

-- -- AUXILIARY FUNCTIONS ON VALUES

instance Eq (Value i) where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  _ == _ = False

instance Show (Value i) where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (ChanHandler a) = "<chan handler " ++ show a ++ ">"
  show (Closure _ _ _) = "<closure>"
  show (Exception v ) = "<exception -> " ++ show v ++ ">"

-- EVALUATOR

eval :: Expr -> Env i -> Prompt i (Value i) -> CC i (Value i)
eval (Number n) _ _ = trace ("number" ++ show n) (return (IntVal n))

eval (Variable v) env _ = return (find env v)

eval (Apply f e) env p = 
  eval f env p >>= (\ (Closure id env' body) ->
    eval e env p >>= (\ v -> eval body (define env' id v) p))

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

eval Stop env p = (shift p $ \k -> (return (Suspended $ k (return Unit))))

eval (Parallel es) env p = reset $ \outer -> 
                             interleave (map (\e -> reset $ \each -> (eval e env each)) es) outer

eval (Throw th) env p = eval th env p >>= (\v -> (shift p (\k -> (return $ Exception v))))

  -- | Send Expr Expr
  -- | Receive Expr
  -- | Stop
  -- | NewChan  

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
elab (Rec x e) env p =
  case e of
    Lambda fp body ->
      return env' where env' = define env x (Closure fp env' body)
    _ ->
      error "RHS of letrec must be a lambda"

-- -- INITIAL ENVIRONMENT

-- init_env :: Env
-- init_env =
--   make_env [constant "nil" Nil, 
--     constant "true" (BoolVal True), constant "false" (BoolVal False),
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

type ProgState i = (Env i, Chs)
type Answer i = (String, ProgState i)

-- obey :: Phrase -> ProgState i -> (String, ProgState i)
-- obey _ p = ("done", p)
-- obey (Calculate exp) (env, mem) =
--   eval exp env mem
--     (Cont $ \ () mem' -> ("***exit in main program***", (env, mem')))
--     (Cont $ \ v mem' -> (print_value v, (env, mem')))
-- obey (Define def) (env, mem) =
--   let x = def_lhs def in
--   elab def env mem
--     (Cont $ \ () mem' -> ("***exit in definition***", (env, mem')))
--     (Cont $ \ env' mem' -> (print_defn env' x, (env', mem')))
