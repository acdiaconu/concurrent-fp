module CState(
  ChanID, ChanState, CType(..),       -- types
  empty_cst, contents, update, fresh  -- operations
) where

import qualified Data.Map as Map

-- Channel IDs are integers
newtype ChanID = ChanID Integer deriving Eq

instance Show ChanID where
  show (ChanID u) = show u

data CType v k = Empty 
               | WR v (v -> k)
               | WW (v -> k)
               | Ready k (CType v k)
               | Closed

-- Our state will contain two pieces of information:
--   * an integer used for unique IDs
--   * a mapping from channel IDs to they channel type
newtype ChanState v k = Mem (Integer, Map.Map Integer (CType v k))

empty_cst :: ChanState v k
empty_cst = Mem (0, Map.empty)

----- Operations on the channel state -----

-- Gets the content of a channel from the state
contents :: ChanState v k -> ChanID -> CType v k
contents (Mem (n, s)) (ChanID u) = 
  case Map.lookup u s of
    Just chs -> chs
    Nothing -> error ("non-existent location " ++ show u)

-- Updates the specified channel to the given new type
update :: ChanState v k -> ChanID -> CType v k -> ChanState v k
update (Mem (n, s)) (ChanID u) chs = 
  case Map.lookup u s of
    Just _ -> Mem (n, Map.insert u chs s)
    Nothing -> error ("non-existent location " ++ show u)

-- Creates a new channel and returns its handler, 
-- together with the update state
fresh :: ChanState v k -> (ChanID, ChanState v k)
fresh (Mem (n, s)) = (ChanID n, Mem (n+1, Map.insert n Empty s))
