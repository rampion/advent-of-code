module AdventOfCode.Y2023 (Solver (..), solvers) where

import AdventOfCode.Solver (Solver (..))
import AdventOfCode.Y2023.Day1 qualified as Day1
import AdventOfCode.Y2023.Day2 qualified as Day2
import Prelude

solvers :: [(String, Solver)]
solvers =
  [ ("2023/day/1", Day1.solver)
  , ("2023/day/2", Day2.solver)
  ]
