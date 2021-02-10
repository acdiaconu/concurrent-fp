module Types where

type TVar = Char

data Type = TVar TVar
          | Int
          | Bool
          | Exception
          | Arrow Type Type      
          | Tuple [Type]
          deriving (Show, Eq)

data Scheme = Scheme [TVar] Type