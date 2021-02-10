module CState(
  Location, ChanState, CType(..),    -- types
  empty_cst, contents, update, fresh  -- operations
) where

import qualified Data.Map as Map

newtype Location = Loc Integer deriving Eq

instance Show Location where 
  show (Loc u) = show u

data CType v k = Empty 
               | WR v (v -> k)
               | WW (v -> k) 

newtype ChanState v k = Mem (Integer, Map.Map Integer (CType v k))

empty_cst :: ChanState v k
empty_cst = Mem (0, Map.empty)

contents :: ChanState v k -> Location -> CType v k
contents (Mem (n, s)) (Loc u) = 
  case Map.lookup u s of
    Just chs -> chs
    Nothing -> error ("non-existent location " ++ show u)

update :: ChanState v k -> Location -> CType v k -> ChanState v k
update (Mem (n, s)) (Loc u) chs = 
  case Map.lookup u s of
    Just _ -> Mem (n, Map.insert u chs s)
    Nothing -> error ("non-existent location " ++ show u)

fresh :: ChanState v k -> (Location, ChanState v k)
fresh (Mem (n, s)) = (Loc n, Mem (n+1, Map.insert n Empty s))
