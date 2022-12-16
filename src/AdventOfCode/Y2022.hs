module AdventOfCode.Y2022 (Solver(..), solvers) where
import AdventOfCode.Solver (Solver(..))
import Data.Map qualified as Map
import Prelude

import AdventOfCode.Y2022.Day1 qualified as Day1
import AdventOfCode.Y2022.Day2 qualified as Day2
import AdventOfCode.Y2022.Day3 qualified as Day3
import AdventOfCode.Y2022.Day4 qualified as Day4
import AdventOfCode.Y2022.Day5 qualified as Day5
import AdventOfCode.Y2022.Day6 qualified as Day6
import AdventOfCode.Y2022.Day7 qualified as Day7
import AdventOfCode.Y2022.Day8 qualified as Day8
import AdventOfCode.Y2022.Day9 qualified as Day9
import AdventOfCode.Y2022.Day10 qualified as Day10
import AdventOfCode.Y2022.Day11 qualified as Day11
import AdventOfCode.Y2022.Day12 qualified as Day12
import AdventOfCode.Y2022.Day13 qualified as Day13
import AdventOfCode.Y2022.Day14 qualified as Day14
import AdventOfCode.Y2022.Day15 qualified as Day15
import AdventOfCode.Y2022.Day16 qualified as Day16
import AdventOfCode.Y2022.Day17 qualified as Day17
import AdventOfCode.Y2022.Day18 qualified as Day18
import AdventOfCode.Y2022.Day19 qualified as Day19
import AdventOfCode.Y2022.Day20 qualified as Day20
import AdventOfCode.Y2022.Day21 qualified as Day21
import AdventOfCode.Y2022.Day22 qualified as Day22
import AdventOfCode.Y2022.Day23 qualified as Day23
import AdventOfCode.Y2022.Day24 qualified as Day24
import AdventOfCode.Y2022.Day25 qualified as Day25

solvers :: Map.Map String Solver
solvers =
  Map.fromList
    [ ("day/1", Day1.solver)
    , ("day/2", Day2.solver)
    , ("day/3", Day3.solver)
    , ("day/4", Day4.solver)
    , ("day/5", Day5.solver)
    , ("day/6", Day6.solver)
    , ("day/7", Day7.solver)
    , ("day/8", Day8.solver)
    , ("day/9", Day9.solver)
    , ("day/10", Day10.solver)
    , ("day/11", Day11.solver)
    , ("day/12", Day12.solver)
    , ("day/13", Day13.solver)
    , ("day/14", Day14.solver)
    , ("day/15", Day15.solver)
    , ("day/16", Day16.solver)
    , ("day/17", Day17.solver)
    , ("day/18", Day18.solver)
    , ("day/19", Day19.solver)
    , ("day/20", Day20.solver)
    , ("day/21", Day21.solver)
    , ("day/22", Day22.solver)
    , ("day/23", Day23.solver)
    , ("day/24", Day24.solver)
    , ("day/25", Day25.solver)
    ]
