module AdventOfCode.Solver where

import Prelude
import Text.Parsec

type Parser = Parsec String ()

data Solver where
  Solver :: Show b => Parser a -> (a -> b) -> (a -> b) -> Solver
