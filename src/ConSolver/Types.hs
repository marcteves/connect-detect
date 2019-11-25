module ConSolver.Types where

import           Data.Array hiding (index)

type Count = Int
type Index = Int
type Time = Float
type ResultTable = Array Index (Array Index Count)

data NetEvent = NetEvent
    { time  :: Time
    , up    :: Bool
    , index :: Index
    }
