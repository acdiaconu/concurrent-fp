{-# OPTIONS_GHC -fno-warn-tabs #-}
module FunSyntax where

import Types

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
  | Injector String [Expr]
  | Let Defn Expr
  | Pipe Expr Expr		      
  | Parallel [Expr]
  | Send Expr Expr
  | Receive Expr
  | NewChan
  | Close Expr
  | TryCatch Expr Expr
  | Throw Expr
  deriving Show

data Defn =                  
    Val Ident Expr           
  | Rec Ident Expr          
  | Data Ident [Defn]
  deriving Show

def_lhs (Val x _) = x
def_lhs (Rec x _) = x
def_lhs (Data x _) = x