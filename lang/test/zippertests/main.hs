import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Debug.Trace

mytree = Node 1 [Node 2 [
                    Node 6 [], Node 7 [], Node 8 []], 
                 Node 3 [
                    Node 4 [
                        Node 10 [], Node 11 [], Node 12 []], 
                    Node 5 []]]

small = Node 100 [Node 101 [Node 2 []], Node 102 []]

traverseAndModify focus f =
    if isLeaf focus
    then modifyTree f focus
    else allKids ((fromJust.firstChild) focus)
    where
       allKids crt = let modif = traverseAndModify crt f in
                     case next modif of
                        Nothing -> case parent modif of
                            Nothing -> modif
                            Just p  -> p
                        Just nx -> allKids nx

oknode n = case n of 
     Node _ [] -> True
     _         -> False

func (Node l ls) =
    if all oknode ls
    then Node l []
    else Node 

    if l `mod` 2 == 0 
    then Node l [Node 0 [], Node 1 []]
    else 