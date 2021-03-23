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

-- TODO:
-- * create standalone function for matching
-- * use that for match and trycatch eval
-- * implement the primes example!!!

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
type PromptCX = TP2 Value Value Value
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

  show (Waiting _)        = "waiting"
  show (Halted _)         = "halted"
  show (Resume _)         = "resume"

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

eval (Match ex pats) env = eval ex env >>= (\v -> 
    case trymatch v pats env of
      Just (pex, env') -> eval pex env'
      Nothing          -> return $ Injection "ExcMatch" []
  )

eval (Let d e1) env =
  elab d env >>= (\env' -> eval e1 env')

eval (Pipe e1 e2) env = 
  eval e1 env >>= (\_ -> eval e2 env)

-------------------------- Only work here, with bombs
eval (Send ce ve) env =
  eval (SendP ce ve) env >>= (\v -> case v of 
    Exception _ -> (shiftP tp2R $ \_ -> return v)
    _           -> return v)

eval (SendP ce ve) env = 
  shiftP tp2L $ \rest -> 
  eval ce env >>= (\(ChanHandler l) -> 
    eval ve env >>= (\v -> 
      lift (get l >>= (\cst -> case cst of 
        Empty  -> put l (WR v rest) >>= (\() -> return $ Halted l)
        WW rk  -> put l (Ready (rk v) Nothing) >>= (\() -> return (Resume $ rest Unit)) -- calling rest here will continue execution, which is also what i do in scheduler! do it here!
        Closed  -> return (Resume $ rest (Exception (Injection "ExcClosed" [])))
        Ready res _ -> put l (Ready res (Just (WR v rest))) >>= (\() -> return $ Halted l)
      )) >>= (\v -> case v of 
        Resume res -> res
        Halted init_cst -> return v) 
  ))

eval (Receive ce) env =
  eval (ReceiveP ce) env >>= (\v -> case v of 
    Exception _ -> (shiftP tp2R $ \_ -> return v)
    _           -> return v)

eval (ReceiveP ce) env = 
  shiftP tp2L $ \rest ->
  eval ce env >>= (\(ChanHandler l) -> 
    lift (get l >>= (\ cst -> case cst of 
      Empty   -> put l (WW rest) >>= (\() -> return $ Halted l)
      WR v sk -> put l (Ready (sk Unit) Nothing) >>= (\() -> return (Resume $ rest v))
      Closed  -> return (Resume $ rest (Exception (Injection "ExcClosed" [])))
      Ready res _ -> put l (Ready res (Just (WW rest))) >>= (\() -> return $ Halted l)
    )) >>= (\v -> case v of 
      Resume res -> res
      Halted _ -> return $ v)
  )
------------------------------

eval (Parallel es) env = 
  scheduler (map (\e -> pushPrompt tp2L (eval e env)) es, []) 0

eval (TryCatch ex pats) env =   
  (\v -> case v of
    Exception e -> case trymatch e pats env of
                     Just (pex, env') -> eval pex env'
                     Nothing          -> (shiftP tp2R $ \normal -> return $ Exception e)
    _           -> return v
  ) =<< (pushPrompt tp2R (eval ex env))
    
eval (Throw th) env = 
  shiftP tp2R $ \normal ->
  eval th env >>= (\v -> case v of
    Injection n vs -> return $ Exception v
    _              -> error "Must throw a sum type")

eval NewChan env = lift $  
  new >>= (\l -> 
    put l Empty >>= (\() -> return $ ChanHandler l))

eval (Close c) env = eval c env >>= (\(ChanHandler l) -> 
    (lift $ get l >>= \cst -> case cst of 
      Empty        -> put l Closed
      Closed       -> error "already closed"
      Ready res _  -> put l (Ready res (Just Closed))
      WR _ wk      -> put l (Ready (wk (Exception (Injection "ExcClosed" [])))
                                   (Just Closed))
      WW rk        -> put l (Ready (rk (Exception (Injection "ExcClosed" [])))
                                   (Just Closed))
    ) >>= (\() -> return Unit))

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

-- scheduler has no knowledge of other prompts

scheduler :: ([Kont Value], [Kont Value]) -> Int -> Kont Value
scheduler ([], rs) w = if w == 0 
                       then values rs >>= (\vs -> return $ Tuple (reverse vs))
                       else scheduler (reverse rs, []) w
scheduler ((k:ks), rs) w = k >>= (\v -> case v of 
    Halted l     -> scheduler (ks, (return $ Waiting l):rs) (w + 1)
    Waiting l    -> lift (get l >>= (\cst -> case cst of 
                      Ready sk next -> (
                          case next of 
                            Just chn -> put l chn
                            Nothing  -> put l Empty
                        ) >>= (\() -> return (1, sk))
                      _             -> return (2, return $ Waiting l)
                    )) >>= (\(n, r) -> case n of 
                      1 -> scheduler ((r:ks), rs) (w - 1)
                      2 -> scheduler (ks, (r:rs)) w
                    )
    v            -> scheduler (ks, (return v:rs)) w
  )

values [] = return []
values (c:cvs) = c >>= (\v -> values cvs >>= (\vs -> return (v:vs)))

-- TODO: if exception undefined in the patterns it 
--       doesnt yeield an error from env

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
init_cst = CST empty_cst  -- initial empty channel state

-- MAIN PROGRAM

-- Deal with top-state exprs and defs

obey :: Phrase -> ProgState -> (String, ProgState)
obey (Calculate exp) (env, mem) =
  -- nice compositionality
  let (v, mem') = runS (runCC $ pushPrompt tp2R (eval exp env)) mem in 
  (show v, (env, mem'))
obey (Define def) (env, mem) =
  let x = def_lhs def in
  let (env', mem') = runS (runCC $ elab def env) mem in 
  ("", (env', mem'))
