module Main where

import           ConSolver
import           ConSolver.Types
import           Data.Array
import           EventGen
import           System.Random
import           Text.Layout.Table

-- testdata :: [NetEvent]
-- testdata = map construct [
--     (0, True, 1),
--     (0.4, False, 2),
--     (0.5, False, 3),
--     (0.6, True, 3),
--     (0.7, False, 4),
--     (0.8, False, 4)
--     ]
--     where construct (a, b, c) = NetEvent a b c

pairs = [(1,4), (2,1), (2,2), (3,2), (5,3)]
number = 6

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn $ formatTable number $ solve 0.5 number  $ randomEvents gen 1000 pairs

formatTable numNodes table = let
    rows = map rowG $ zipWith (:) [show x | x <- [1..numNodes]] (map (map show) (map elems (elems table)))
    in tableString (replicate (numNodes + 1) def) asciiS def rows
