module AdventOfCode.Day1 where

import AdventOfCode.Solver
import Control.Applicative (some)
import Data.Coerce (coerce)
import Data.List (sort)
import Data.Ord (Down(..))
import Prelude
import Text.Parsec hiding ((<|>))

day1parser :: Parser [[Int]]
day1parser = inventory `sepBy` newline where
  calories = read @Int <$> some digit
  inventory = calories `endBy` newline

day1part1 :: [[Int]] -> Int
day1part1 = maximum . map sum

day1part2 :: [[Int]] -> Int
day1part2 =
  sum . take 3 . coerce @([Down Int] -> [Down Int]) sort . map sum
