module Interpreter(dialog, init_mem, init_env, obey, funParser) where

import Parsing
import FunSyntax
import FunParser
import Environment
import Memory
import Debug.Trace

-- MONAD

type M a = Mem -> (String, a, Mem)

result :: a -> M a
result x mem = ("", x, mem)

($>) :: M a -> (a -> M b) -> M b
(xm $> f) mem =
  let (s1, x, mem') = xm mem in
  let (s2, y, mem'') = (f x) mem' in
  (s1++s2, y, mem'')

output :: String -> M ()
output s mem = (s, (), mem)

new :: M Location
new mem = let (a, mem') = fresh mem in ("", a, mem')

get :: Location -> M Value
get a mem = ("", contents mem a, mem)

put :: Location -> Value -> M ()
put a v mem = ("", (), update mem a v)


-- SEMANTIC DOMAINS

data Value =
    IntVal Integer
  | BoolVal Bool
  | Addr Location
  | Nil | Cons Value Value
  | Function (Value -> M Value)

type Env = Environment Value
type Mem = Memory Value

-- EVALUATOR

eval :: Expr -> Env -> M Value
eval (Number n) env = result (IntVal n)
eval (Variable x) env = result (find env x)

eval (Apply f e) env =
  eval f env $> (\fv ->
    eval e env $> (\arg ->
      apply fv arg))
eval (If e1 e2 e3) env =
  eval e1 env $> (\b ->
    case b of
      BoolVal True -> eval e2 env
      BoolVal False -> eval e3 env
      _ -> error "boolean required in conditional")
eval (Lambda x e1) env =
  result (abstract x e1 env)
eval (Let d e1) env =
  elab d env $> (\env' -> eval e1 env')

eval (Pipe e1 e2) env = trace (show $ Pipe e1 e2) (result Nil)
eval (Par e1 e2) env = trace (show $ Par e1 e2) (result Nil)
eval (Send e1 e2) env = trace (show $ Send e1 e2) (result Nil)
eval (Receive e) env = trace (show $ Receive e) (result Nil)
eval NewChan env = trace (show NewChan) (result Nil)

abstract :: Ident -> Expr -> Env -> Value
abstract x e env =
  Function (\arg -> eval e (define env x arg))

apply :: Value -> Value -> M Value
apply (Function f) arg = f arg
apply _ args = error "applying a non-function"

elab :: Defn -> Env -> M Env
elab (Val x e) env = 
  eval e env $> (\v -> result (define env x v))
elab (Rec x e) env =
  case e of
    Lambda fps body ->
      result env' where env' = define env x (abstract fps body env')
    _ ->
      error "RHS of letrec must be a lambda"


-- INITIAL ENVIRONMENT

init_env :: Env
init_env =
  make_env [constant "nil" Nil, 
    constant "true" (BoolVal True), constant "false" (BoolVal False),
    pureprim "+" (\ (IntVal a) -> Function (\(IntVal b) -> result $ IntVal (a + b)))]
    -- pureprim "-" (\ (IntVal a) (IntVal b) -> IntVal (a - b)),
    -- pureprim "*" (\ (IntVal a) (IntVal b) -> IntVal (a * b)),
    -- pureprim "div" (\ (IntVal a) (IntVal b) ->
    --   if b == 0 then error "Dividing by zero" else IntVal (a `div` b)),
    -- pureprim "mod" (\ (IntVal a) (IntVal b) ->
    --   if b == 0 then error "Dividing by zero" else IntVal (a `mod` b)),
    -- pureprim "~" (\ [IntVal a] -> IntVal (- a)),
    -- pureprim "<" (\ (IntVal a) (IntVal b) -> BoolVal (a < b)),
    -- pureprim "<=" (\ (IntVal a) (IntVal b) -> BoolVal (a <= b)),
    -- pureprim ">" (\ (IntVal a) (IntVal b) -> BoolVal (a > b)),
    -- pureprim ">=" (\ (IntVal a) (IntVal b) -> BoolVal (a >= b)),
    -- pureprim "=" (\ a b -> BoolVal (a == b)),
    -- pureprim "<>" (\ a b -> BoolVal (a /= b)),
    -- pureprim "integer" (\ a ->
    --   case a of IntVal _ -> BoolVal True; _ -> BoolVal False),
    -- pureprim "head" (\ (Cons h t) -> h),
    -- pureprim "tail" (\ (Cons h t) -> t),
    -- pureprim ":" (\ a b -> Cons a b),
    -- pureprim "list" (\ xs -> foldr Cons Nil xs),
    -- primitive "print" (\ v -> output (show v) $> (\ () -> result v))]
    where
    constant x v = (x, v)
    primitive x f = (x, Function (primwrap x f))
    pureprim x f = (x, Function (primwrap x (\args -> result (f args))))


-- AUXILIARY FUNCTIONS ON VALUES

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = h1 == h2 && t1 == t2
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show (Addr a) = "<address " ++ show a ++ ">"
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h t) = ", " ++ show h ++ shtail t
      shtail x = " . " ++ show x
  show (Function _) = "<function>"


-- MAIN PROGRAM

type GloState = (Env, Mem)

obey :: Phrase -> GloState -> (String, GloState)
obey (Calculate exp) (env, mem) =
  let (out, v, mem') = eval exp env mem in
  (out ++ print_value v, (env, mem'))
obey (Define def) (env, mem) =
  let x = def_lhs def in
  let (out, env', mem') = elab def env mem in
  (out ++ print_defn env' x, (env', mem'))