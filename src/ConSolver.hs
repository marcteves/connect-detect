module ConSolver where

import           ConSolver.Types
import           Data.Array      hiding (index)

solve :: Time -> Int -> [NetEvent] -> ResultTable
solve tol numNodes events = let
    init_table = listArray (1, numNodes) $ replicate numNodes $ listArray (1, numNodes) $ replicate numNodes 0
    in solve_ tol [] events init_table

solve_ :: Time -> [(Index, Time)] -> [NetEvent] -> ResultTable -> ResultTable
solve_ tol active (current : events) res = let
    now = time current
    active_curr = if (up current)
                    then (index current, time current) : active
                    else active
    remaining_active = filter (expire now) active_curr
    new_results = if (up current)
                    then res
                    else updateRes remaining_active current res
    in solve_ tol remaining_active events new_results
    where expire c (_,t) = (c - t) < tol

solve_ _ _ [] res = res

updateRes :: [(Index, Time)] -> NetEvent -> ResultTable -> ResultTable
updateRes active event res = let
    i = index event
    active_i = map fst active
    in foldr (updateTable i) res active_i

updateTable i j r = let
    c = r ! i ! j
    row = r ! i // [(j, c + 1)]
    in r // [(i, row)]
