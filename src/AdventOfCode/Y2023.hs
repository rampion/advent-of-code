module AdventOfCode.Y2023 (Solver (..), solvers) where

import AdventOfCode.Solver (Solver (..))
import AdventOfCode.Y2023.Day1 qualified as Day1
import AdventOfCode.Y2023.Day2 qualified as Day2
import AdventOfCode.Y2023.Day3 qualified as Day3
-- import AdventOfCode.Y2023.Day4 qualified as Day4
-- import AdventOfCode.Y2023.Day5 qualified as Day5
-- import AdventOfCode.Y2023.Day6 qualified as Day6
-- import AdventOfCode.Y2023.Day7 qualified as Day7
-- import AdventOfCode.Y2023.Day8 qualified as Day8
-- import AdventOfCode.Y2023.Day9 qualified as Day9
-- import AdventOfCode.Y2023.Day10 qualified as Day10
-- import AdventOfCode.Y2023.Day11 qualified as Day11
-- import AdventOfCode.Y2023.Day12 qualified as Day12
-- import AdventOfCode.Y2023.Day13 qualified as Day13
-- import AdventOfCode.Y2023.Day14 qualified as Day14
-- import AdventOfCode.Y2023.Day15 qualified as Day15
-- import AdventOfCode.Y2023.Day16 qualified as Day16
-- import AdventOfCode.Y2023.Day17 qualified as Day17
-- import AdventOfCode.Y2023.Day18 qualified as Day18
-- import AdventOfCode.Y2023.Day19 qualified as Day19
-- import AdventOfCode.Y2023.Day20 qualified as Day20
-- import AdventOfCode.Y2023.Day21 qualified as Day21
-- import AdventOfCode.Y2023.Day22 qualified as Day22
-- import AdventOfCode.Y2023.Day23 qualified as Day23
-- import AdventOfCode.Y2023.Day24 qualified as Day24
-- import AdventOfCode.Y2023.Day25 qualified as Day25
import Prelude

solvers :: [(String, Solver)]
solvers =
  [ ("2023/day/1", Day1.solver)
  , ("2023/day/2", Day2.solver)
  , ("2023/day/3", Day3.solver)
  -- , ("2023/day/4", Day4.solver)
  -- , ("2023/day/5", Day5.solver)
  -- , ("2023/day/6", Day6.solver)
  -- , ("2023/day/7", Day7.solver)
  -- , ("2023/day/8", Day8.solver)
  -- , ("2023/day/9", Day9.solver)
  -- , ("2023/day/10", Day10.solver)
  -- , ("2023/day/11", Day11.solver)
  -- , ("2023/day/12", Day12.solver)
  -- , ("2023/day/13", Day13.solver)
  -- , ("2023/day/14", Day14.solver)
  -- , ("2023/day/15", Day15.solver)
  -- , ("2023/day/16", Day16.solver)
  -- , ("2023/day/17", Day17.solver)
  -- , ("2023/day/18", Day18.solver)
  -- , ("2023/day/19", Day19.solver)
  -- , ("2023/day/20", Day20.solver)
  -- , ("2023/day/21", Day21.solver)
  -- , ("2023/day/22", Day22.solver)
  -- , ("2023/day/23", Day23.solver)
  -- , ("2023/day/24", Day24.solver)
  -- , ("2023/day/25", Day25.solver)
  ]
