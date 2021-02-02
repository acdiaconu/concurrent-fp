module Memory(Location, ChansState, CState, init_mem, contents, update, fresh) where

import qualified Data.Map as Map

newtype Location = Loc Integer deriving Eq

instance Show Location where show (Loc u) = show u

-- Invalid locations can be non-existent, or they can exist 
-- but be uninitialised.  You can't update a non-existent location,
-- and you cant get the contents of an uninitialised location.

data CState v = Empty 
              | WR v v
              | WW v

newtype ChansState v k = Mem (Integer, Map.Map Integer (CState v k))

init_mem :: ChansState v k
init_mem = Mem (0, Map.empty)

contents :: ChansState v k -> Location -> CState v k
contents (Mem (n, s)) (Loc u) = 
  case Map.lookup u s of
    Just chs -> chs
    Nothing -> error ("non-existent location " ++ show u)

update :: ChansState v k -> Location -> CState v k -> ChansState v k
update (Mem (n, s)) (Loc u) chs = 
  case Map.lookup u s of
    Just _ -> Mem (n, Map.insert u chs s)
    Nothing -> error ("non-existent location " ++ show u)

fresh :: ChansState v k -> (Location, ChansState v k)
fresh (Mem (n, s)) = (Loc n, Mem (n+1, Map.insert n Empty s))

-- array :: Memory e v k -> Integer -> (Location, Memory a)
-- array (Mem (n, s)) k = (Loc n, Mem (n+k, s))

-- index :: Memory e v k -> Location -> Integer -> Location
-- index mem (Loc a) i = Loc (a+i)

-- dump :: Show a => Memory e v k -> String
-- dump (Mem (n, s)) = show (Map.toList s)
