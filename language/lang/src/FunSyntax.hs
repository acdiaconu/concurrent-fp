{-# OPTIONS_GHC -fno-warn-tabs #-}
module FunSyntax where

import Environment

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
  | Let Defn Expr                
  | Pipe Expr Expr		      
  | Parallel [Expr]
  | Send Expr Expr
  | Receive Expr
  | Stop
  | NewChan   
  | TryCatch Expr Expr
  | Throw Expr
  deriving Show

data Defn =                  
    Val Ident Expr           
  | Rec Ident Expr           
  deriving Show

type Ident = String

def_lhs (Val x e) = x
def_lhs (Rec x e) = x