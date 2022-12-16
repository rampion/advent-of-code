module AdventOfCode (Solver(..), solvers) where

import AdventOfCode.Solver (Solver)
import AdventOfCode.Y2022 qualified as Y2022
import Data.Map qualified as Map

solvers :: Map.Map String Solver
solvers = Data.Map.unions
  [ Map.mapKeys ("2022/" <>) Y2022.solvers
  ]
