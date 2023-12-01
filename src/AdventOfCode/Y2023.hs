module AdventOfCode.Y2023 (Solver (..), solvers) where

import AdventOfCode.Solver (Solver (..))
import AdventOfCode.Y2023.Day1 qualified as Day1
import Prelude

solvers :: [(String, Solver)]
solvers =
  [ ("2023/day/1", Day1.solver)
  ]
