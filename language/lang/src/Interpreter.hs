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

instance MonadFail (State s) where
  fail = error "state matching failed"

-- Operations for the state monad

get :: Location -> State CST (CType Value Suspended)
get l = State $ \(CST chs) -> (contents chs l, CST chs)

put :: Location -> CType Value Suspended -> State CST ()
put l ct = State $ \(CST chs) -> ((), CST $ update chs l ct)

new :: State CST Location
new = State $ \(CST chs) -> let (l, chs') = fresh chs in (l, CST chs')

-- Helper types

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
  | Exception Value
  
  -- denotable, not expressible
  | Resume Suspended
  | Waiting Location              
  | Halted Location

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
  show (Exception v )      = "<unhandled exception -> " ++ show v ++ ">"
  show (Tuple vs)          = "(" ++ intercalate "," (map show vs) ++ ")"
  show Unit                = "unit"
  show (Injection name vs) = name ++ " " ++ intercalate " " (map show vs)

  show (Waiting _)        = error "*Waiting* should not be printed"
  show (Halted _)         = error "*Halted* should not be printed"
  show (Resume _)         = error "*Resume* should not be printed"

-- Evaluator

eval :: Expr -> Env -> Kont Value

-------------------------- Basics
eval (Number n) _ = return (IntVal n)

eval (Variable v) env = return (find env v)

eval (Apply f e) env = 
  do Closure id env' body <- eval f env
     v                    <- eval e env
     eval body (define env' id v)

eval (If cond et ef) env =
  do cond <- eval cond env  
     case cond of
       BoolVal True -> eval et env
       BoolVal False -> eval ef env
       _ -> error "Boolean required in conditional"

eval (Lambda x e1) env = return $ Closure x env e1

eval (Pipe e1 e2) env = 
  do eval e1 env -- we discard the first expression's result
     eval e2 env

eval (Let d e1) env =
  do env' <- elab d env 
     eval e1 env'
--------------------------

-------------------------- Pattern matching
eval (Injector name args) env = values evs >>= (\vs -> 
  return $ Injection name vs)
  where evs = map (`eval` env) args

eval (Match ex pats) env = 
  do v <- eval ex env 
     case trymatch v pats env of
       Just (pex, env') -> eval pex env'
       Nothing          -> return $ Injection "ExcMatch" []
--------------------------

-------------------------- Concurrency
eval (Send ce ve) env =
  do v <- eval (SendP ce ve) env
     case v of 
       Exception _ -> shift pX $ \_ -> return v
       _           -> return v

eval (SendP ce ve) env = 
  shift pP$ \rest -> 
  do 
    ChanHandler l <- eval ce env 
    v <- eval ve env 
    sus <- lift $ -- descend to the state monad, to see if we suspend or not
      get l >>= \ chanState -> 
        -- based on the state, we decide what the next state is
        case chanState of 
          Empty -> 
            do put l (WR v rest)
               return $ Halted l
          Ready res _ -> 
            do put l $ Ready res (WR v rest)
               return $ Halted l
          WW rk -> 
            do put l $ Ready (rk v) Empty
               return $ Resume (rest Unit)
          Closed -> 
            return (Resume $ rest (Exception $ Injection "ExcClosed" []))
    case sus of 
      Resume res -> res              -- resume execution, no scheduler involved
      Halted init_cst -> return sus  -- return to scheduler

-- The code for Receive is almost identical to the one for Send
eval (Receive ce) env =
  do v <- eval (ReceiveP ce) env
     case v of 
       Exception _ -> (shift pX $ \_ -> return v)
       _           -> return v

eval (ReceiveP ce) env = 
  shift pP$ \rest ->
  do 
    ChanHandler l <- eval ce env
    sus <- lift $ 
      get l >>= \ chanState -> 
        case chanState of 
          Empty -> 
            do put l (WW rest)
               return $ Halted l
          Ready res _ -> 
            do put l $ Ready res (WW rest)
               return $ Halted l
          WR v sk -> 
            do put l (Ready (sk Unit) Empty) 
               return (Resume $ rest v)
          Closed -> 
            return (Resume $ rest (Exception $ Injection "ExcClosed" []))
    case sus of 
      Resume res -> res 
      Halted _ -> return sus 

eval (Parallel cs) env = scheduler (components, []) 0
  where components = map (`createComponent` env) cs
        createComponent c env = pushPrompt pP (eval c env)

eval NewChan env = 
  lift $  
  do l <- new 
     put l Empty
     return $ ChanHandler l

eval (Close c) env = 
  do
    ChanHandler l <- eval c env
    lift $ 
      get l >>= \ chanState -> case chanState of 
        Empty        -> put l Closed
        Closed       -> error "already closed"
        Ready res _  -> put l (Ready res Closed)
        WR _ wk      -> put l (Ready (wk (Exception (Injection "ExcClosed" [])))
                                     Closed)
        WW rk        -> put l (Ready (rk (Exception (Injection "ExcClosed" [])))
                                     Closed)
    return Unit
------------------------------

------------------------------ Exception handling
eval (TryCatch ex pats) env =   
  do 
    -- First we delimit the context in which we evaluate the expression. If 
    -- we end in an error, we essentially discard this computation, since we 
    -- discard everything up to the closest `px' prompt.
    val <- pushPrompt pX (eval ex env)
    -- Check whether we have ended with an exception or not.
    case val of
      Exception e -> case trymatch e pats env of
                      Just (pex, env') -> eval pex env'
                      -- If no handler handles our error, propagate
                      Nothing          -> shift pX $ \_ -> return $ Exception e
      _           -> return val -- No error, so just return the value.
    
eval (Throw th) env = 
  shift pX $ \_ ->
  do v <- eval th env 
     case v of
       Injection n vs -> return $ Exception v
       _              -> error "Must throw a sum type"
------------------------------

------------------------------ Primitive operations
eval (BinPrim bop e1 e2) env = case bop of
  Plus -> arithmeticBOP (+) e1 e2 env
  Minus -> arithmeticBOP (-) e1 e2 env
  Times -> arithmeticBOP (*) e1 e2 env
  Div -> arithmeticBOP (div) e1 e2 env
  Mod -> arithmeticBOP (mod) e1 e2 env
  And -> logicBOP (&&) e1 e2 env
  Or -> logicBOP (||) e1 e2 env
  Equal -> do v1 <- eval e1 env 
              v2 <- eval e2 env 
              return $ BoolVal (v1 == v2)

eval (MonPrim mop e) env = 
  case mop of
    Neg -> 
      do IntVal n <- eval e env 
         return $ IntVal (-n)

-- Helper functions that abstract the pattern of evaluation for 
-- binary primitive operations

arithmeticBOP :: (Integer -> Integer -> Integer) -> 
                 Expr -> Expr -> Env -> Kont Value
arithmeticBOP op e1 e2 env = 
  do IntVal n1 <- eval e1 env 
     IntVal n2 <- eval e2 env  
     return $ IntVal (op n1 n2)

logicBOP :: (Bool -> Bool -> Bool) -> 
            Expr -> Expr -> Env -> Kont Value
logicBOP funcop e1 e2 env = 
  do BoolVal b1 <- eval e1 env 
     BoolVal b2 <- eval e2 env 
     return $ BoolVal (funcop b1 b2)
------------------------------

-- Environment expansion
elab :: Defn -> Env -> Kont Env
elab (Val x e) env =
  do v <- eval e env 
     return (define env x v)
elab (Rec x e) env =
  case e of
    Lambda fp body -> return env' 
      where env' = define env x (Closure fp env' body)
    _ -> error "RHS of letrec must be a lambda"
elab (Data _ ctors) env = foldM (\ env' cdef -> elab cdef env') env ctors

-- Scheduler
scheduler :: ([Kont Value], [Kont Value]) -> Int -> Kont Value
scheduler ([], rs) w = if w == 0 
                       then 
                        do vs <- values rs
                           return $ Tuple (reverse vs)
                       else scheduler (reverse rs, []) w
scheduler ((k:ks), rs) w = k >>= (\v -> case v of 
    Halted l     -> scheduler (ks, (return $ Waiting l):rs) (w + 1)
    Waiting l    -> lift (get l >>= (\cst -> case cst of 
                      Ready sk next -> put l next >>= (\() -> return $ Left sk)
                      _             -> return $ Right (return $ Waiting l)
                    )) >>= (\val -> case val of 
                      Left r -> scheduler ((r:ks), rs) (w - 1)
                      Right r -> scheduler (ks, (r:rs)) w
                    )
    v            -> scheduler (ks, (return v:rs)) w
  )

-- Helpers

values :: [Kont Value] -> Kont [Value]
values [] = return []
values (c:cvs) = 
  do v <- c 
     vs <- values cvs 
     return (v:vs)

-- TODO: Lazyness makes it seem like undefined exception are fine, 
--       since they are not evaluated
trymatch :: Value -> [Pattern] -> Env -> Maybe (Expr, Env)
trymatch v [] env = Nothing
trymatch v ((Pattern patCtor pex):ps) env = 
  case (trydefine v patCtor env) of
    Just env' -> Just (pex, env')
    Nothing   -> trymatch v ps env

trydefine :: Value -> VarCtor -> Env -> Maybe Env
trydefine (Injection n' vals) (VarCtor n vars) env =
  if n == n'
  then Just $ defargs env vars vals
  else Nothing
trydefine a b c = error (show a ++ show b)

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
    -- some primitive exceptions
    ("ExcClosed", Injection "ExcClosed" []),
    ("ExcInvalid", Injection "ExcInvalid" []),
    ("ExcMatch", Injection "ExcMatch" [])]

init_cst :: CST
init_cst = CST empty_cst  

-- Deal with top-state exprs and defs. Observe the nice compositionality for
-- that happens when we run the computations.
obey :: Phrase -> ProgState -> (String, ProgState)
obey (Calculate exp) (env, mem) =
  let (v, mem') = (runS . runCC) (pushPrompt pX (eval exp env)) mem in 
  (show v, (env, mem'))
obey (Define def) (env, mem) =
  let x = def_lhs def in
  let (env', mem') = (runS . runCC) (elab def env) mem in 
  (print_defn env' x, (env', mem'))
