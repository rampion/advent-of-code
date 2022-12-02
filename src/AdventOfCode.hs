module AdventOfCode
  ( module AdventOfCode
  , Solver(..)
  ) where

import Prelude
import Data.Map (Map, fromList)
import AdventOfCode.Solver
import Text.Parsec
import Control.Applicative
import Data.Ord (Down(..))
import Data.Coerce (coerce)
import Data.List (sort)

solvers :: Map String Solver
solvers = fromList
  [ ( "2022/day/1", Solver day1parser day1part1 day1part2)
  ]

day1parser :: Parser [[Int]]
day1parser = inventory `sepBy` newline where
  calories = read @Int <$> some digit
  inventory = calories `endBy` newline

day1part1 :: [[Int]] -> Int
day1part1 = maximum . map sum

day1part2 :: [[Int]] -> Int
day1part2 =
  sum . take 3 . coerce @([Down Int] -> [Down Int]) sort . map sum
