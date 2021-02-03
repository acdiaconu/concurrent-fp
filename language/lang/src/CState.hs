module CState(Location, ChanState, CType(..), init_mem, contents, update, fresh) where

import qualified Data.Map as Map

newtype Location = Loc Integer deriving Eq

instance Show Location where show (Loc u) = show u

-- Invalid locations can be non-existent, or they can exist 
-- but be uninitialised.  You can't update a non-existent location,
-- and you cant get the contents of an uninitialised location.

data CType v = Empty 
             | WR v v
             | WW v

newtype ChanState v = Mem (Integer, Map.Map Integer (CType v))

init_mem :: ChanState v
init_mem = Mem (0, Map.empty)

contents :: ChanState v -> Location -> CType v
contents (Mem (n, s)) (Loc u) = 
  case Map.lookup u s of
    Just chs -> chs
    Nothing -> error ("non-existent location " ++ show u)

update :: ChanState v -> Location -> CType v -> ChanState v
update (Mem (n, s)) (Loc u) chs = 
  case Map.lookup u s of
    Just _ -> Mem (n, Map.insert u chs s)
    Nothing -> error ("non-existent location " ++ show u)

fresh :: ChanState v -> (Location, ChanState v)
fresh (Mem (n, s)) = (Loc n, Mem (n+1, Map.insert n Empty s))

-- array :: Memory e v k -> Integer -> (Location, Memory a)
-- array (Mem (n, s)) k = (Loc n, Mem (n+k, s))

-- index :: Memory e v k -> Location -> Integer -> Location
-- index mem (Loc a) i = Loc (a+i)

-- dump :: Show a => Memory e v k -> String
-- dump (Mem (n, s)) = show (Map.toList s)
