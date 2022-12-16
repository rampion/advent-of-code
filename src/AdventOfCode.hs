module AdventOfCode (Solver(..), solvers) where

import AdventOfCode.Solver (Solver)
import AdventOfCode.Y2022 qualified as Y2022
import Prelude

solvers :: [(String, Solver)]
solvers = concat
  [ Y2022.solvers
  ]
