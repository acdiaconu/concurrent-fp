module TreeUtils(traverseAndModify) where

import Data.Tree
import Data.Tree.Zipper
import Data.Maybe

-- Main traversal function. Is used in Interpreter to
--   1) access leaves and execute them (leaves represent the "active" 
--      components)
--   2) look at nodes that are parents of only leaf nodes that are simple
--      values, in which case we resume the parent
traverseAndModify :: TreePos Full a -> (Tree a -> Bool) -> 
                     (TreePos Full a -> TreePos Full a) -> TreePos Full a
traverseAndModify focus cond modif =
    if cond (tree focus)
    then modif focus
    else traverseChildren ((fromJust.firstChild) focus)
    where
        traverseChildren crt = 
            let crtMod = traverseAndModify crt cond modif in
            case next crtMod of
                Nothing -> 
                    case parent crtMod of
                        Nothing -> crtMod
                        Just p  -> p
                Just nx -> traverseChildren nx
       