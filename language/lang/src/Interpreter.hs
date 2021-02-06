module Interpreter(dialog, init_mem, empty_env, funParser, obey) where

import Parsing
import FunSyntax
import FunParser
import Environment
import CState

import Control.Monad
import Control.Monad.Trans
import CCExc

import Debug.Trace

-- State monad
-- TODO: A more algebraic approach, to allow easy combination of ``pure"
--       algebraic effects (i.e. create a class for state as a first step)

newtype State s a = State { runState :: s -> (a, s) }

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

-- Ops

get :: Location -> State (ChanState i) (CType i)
get l = State $ \chs -> (contents chs l, chs)

put :: Location -> CType i -> State (ChanState i) ()
put l ct = State $ \chs -> ((), update chs l ct)

new :: State (ChanState i) Location
new = State $ \chs -> let (l, chs') = fresh chs in (l, chs')

rState comp init = (runState comp) init

-- Helper Types

type Env = Environment Value
type CST = ChanState Value
type PromptCX = P2 Value Value
type Responses = [Value]
type SuspendedComputation = CC PromptCX (State CST) Value

-- Value domain

data Value =
    Unit
  | IntVal Integer
  | BoolVal Bool
  | ChanHandler Location
  | Closure String Env Expr
  | Waiting                         -- waiting read or write
  | Exception Value
  | Intrerrupt SuspendedComputation -- delimC computation that was halted

-- -- AUXILIARY FUNCTIONS ON VALUES

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

-- Evaluator

eval :: Expr -> Env -> CC PromptCX (State CST) Value
eval (Number n) _ = trace ("number" ++ show n) (return (IntVal n))

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

eval (Let d e1) env =
  elab d env >>= (\env' -> eval e1 env')

eval (Pipe e1 e2) env = 
  eval e1 env >>= (\_ -> eval e2 env)

eval Stop env = shiftP p2L $ \k -> return (Intrerrupt $ k Unit)

eval (Send ce ve) env = eval ce env >>= (\(ChanHandler l) -> 
                          eval ve env >>= (\v -> 
                            shiftP p2L $ \k -> 
                            lift $ get l >>= (\cst -> case cst of 
                                Empty  -> put l (WR v $ Intrerrupt (k Unit)) >>= (\() -> (return Waiting))
                                WR _ _ -> error "not allowed"
                        )))

eval (Receive ce) env = eval ce env >>= (\(ChanHandler l) -> 
                          shiftP p2L $ \k -> 
                          lift $ get l >>= (\cst -> case cst of 
                              Empty  -> put l (WW $ Intrerrupt (k Unit)) >>= (\() -> (return Waiting))
                              WR _ _ -> error "not allowed"
                        ))

eval (Parallel es) env = pushPrompt p2R $ 
                           interleave (map (\e -> pushPrompt p2L (eval e env)) es)

eval (Throw th) env = eval th env >>= (\v -> (shiftP p2R (\k -> (return $ Exception v))))

eval NewChan env = lift (new >>= (\l -> put l Empty >>= (\() -> return $ ChanHandler l)))

eval (TryCatch es ef) env = eval es env >>= 
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

interleave :: [CC PromptCX (State CST) Value] -> CC PromptCX (State CST) Value
interleave [] = return Unit 
interleave (k:ks) = k >>= (\v -> case v of 
    Intrerrupt k -> interleave (ks ++ [k])
    Exception e -> abortP p2R (return $ Exception e)
    _           -> interleave ks)

-- INITIAL ENVIRONMENT

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

type ProgState = (Env, CST)

obey :: Phrase -> ProgState -> (String, ProgState)
obey (Calculate exp) (env, mem) =
  let (v, mem') = rState (runCC $ eval ex env) mem in (show v, (env, mem'))
  where 
    ex = TryCatch (Parallel [Pipe (Pipe (Stop) (Throw (Number 5))) (Number 42), 
                                 Pipe (Pipe (Number 2) ex') (Number 6)]) 
             (Lambda "x" (Number 52))
    ex' = Parallel [TryCatch (Parallel [Pipe (Stop) (Number 6), Pipe (Pipe (Number 5) (Throw (Number 10))) (Number 6969)]) (Lambda "x" (Number 52)), Number 50] 
obey (Define def) (env, mem) =
  let x = def_lhs def in
  let (env', mem') = rState (runCC $ elab def env) mem in (print_defn env' x, (env', mem'))
