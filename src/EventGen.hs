module EventGen where

import           ConSolver.Types
import           System.Random

randomEvents gen num pairs =
    let chosenpairs = map ((!!) pairs) $  take num $ randomRs (0, (length pairs) - 1) gen
        randomTimes = take (2 * num) $ randomRs (0.1, 0.2) gen
    in randomEvents_ chosenpairs randomTimes 0 []

randomEvents_ ((a, b) : chosen) (t_1 : t_2 : times) time_now events =
    let ev1 = NetEvent time_now True a
        ev2 = NetEvent (time_now + t_1) False b
        in randomEvents_ chosen times (time_now + t_1 + t_2) (ev2 : ev1 : events)

randomEvents_ [] _ _ events                                    = reverse events
