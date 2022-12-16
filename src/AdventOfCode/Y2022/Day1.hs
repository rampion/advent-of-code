module AdventOfCode.Y2022.Day1 where

import AdventOfCode.Y2022.Prelude
import Data.Coerce (coerce)
import Data.List (sort)
import Data.Ord (Down(..))

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = check parser part1 part2 Example
    { raw = [text|
        1000
        2000
        3000

        4000

        5000
        6000

        7000
        8000
        9000

        10000
      |]
    , parsed = 
      [ [1000, 2000, 3000]
      , [4000]
      , [5000, 6000]
      , [7000, 8000, 9000]
      , [10000]
      ]
    , part1output = 24_000
    , part2output = 45_000
    }
  }

parser :: Parser [[Int]]
parser = inventory `sepBy` newline where
  calories = read @Int <$> some digit
  inventory = calories `endBy` newline

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 =
  sum . take 3 . coerce @([Down Int] -> [Down Int]) sort . map sum
