module AdventOfCode (Solver(..), solvers) where

import AdventOfCode.Solver (Solver)
import AdventOfCode.Y2022 qualified as Y2022
import Data.Map qualified as Map
import Prelude

solvers :: Map.Map String Solver
solvers = Map.unions
  [ Map.mapKeys ("2022/" <>) Y2022.solvers
  ]
