module Scheduler(ComponentState(..),
                 SchedulerST, empty_sched) where

import TreeUtils
import Data.Tree
import Data.Tree.Zipper

data ComponentState v k = 
    Par (v -> k) -- we feed the tuple value to resume a paralell composition
  | Run k
  | Exc (v -> k)
  | Done v

newtype SchedulerST v k = SchedulerST (TreePos Full (ComponentState v k))

empty_sched :: k -> SchedulerST v k
empty_sched k = SchedulerST $ fromTree (Node (Exc k) [])

leftMost :: SchedulerST v k -> SchedulerST v k
leftMost = id

nextActive :: SchedulerST v k -> SchedulerST v k
nextActive = id

attachPar :: SchedulerST v k -> SchedulerST v k
attachPar = id

checkDone :: SchedulerST v k -> SchedulerST v k
checkDone = id

nearestExc :: SchedulerST v k -> SchedulerST v k
nearestExc = id

