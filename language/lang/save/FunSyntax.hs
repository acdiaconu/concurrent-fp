{-# OPTIONS_GHC -fno-warn-tabs #-}
module FunSyntax where

import Data.Char(isAlpha)
import Data.List(intersperse)

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
  | OrElse Expr Expr         
  | Pipe Expr Expr		      
  | Par Expr Expr
  | Send Expr Expr
  | Receive Expr
  | NewChan          
  deriving Show

data Defn =                  
    Val Ident Expr           
  | Rec Ident Expr           
  deriving Show

type Ident = String

def_lhs (Val x e) = x
def_lhs (Rec x e) = x