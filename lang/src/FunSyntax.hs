module FunSyntax where

type Ident = String

data Phrase =
    Calculate Expr
  | Define Defn
  deriving Show

data Expr =                   
    Number Integer           
  | Variable Ident          
  | Apply Expr Expr         
  | If Expr Expr Expr       
  | Lambda Ident Expr
  | BinPrim BOP Expr Expr
  | MonPrim MOP Expr
  | Injector String [Expr]
  | Let Defn Expr
  | Match Expr [Expr]
  | Case Expr Expr
  | Pipe Expr Expr		      
  | Parallel [Expr]
  | Send Expr Expr
  | Receive Expr
  | SendP Expr Expr
  | ReceiveP Expr
  | NewChan
  | Close Expr
  | TryCatch Expr [Expr]
  | Throw Expr
  deriving Show

data BOP = Plus | Minus | Times | Div | Mod | Equal | And | Or deriving Show
data MOP = Neg deriving Show

data Defn =                  
    Val Ident Expr           
  | Rec Ident Expr          
  | Data Ident [Defn]
  deriving Show

def_lhs (Val x _) = x
def_lhs (Rec x _) = x
def_lhs (Data x _) = x