module AdventOfCode.Y2022.Day4 where

import AdventOfCode.Y2022.Prelude

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = check solver Example
    { raw = [text|
        2-4,6-8
        2-3,4-5
        5-7,7-9
        2-8,3-7
        6-6,4-6
        2-6,4-8
      |]
    , parsed = 
      [ ((2, 4), (6, 8))
      , ((2, 3), (4, 5))
      , ((5, 7), (7, 9))
      , ((2, 8), (3, 7))
      , ((6, 6), (4, 6))
      , ((2, 6), (4, 8))
      ]
    , part1output = 2
    , part2output = 4
    }
  }

parser :: Parser [(Range,Range)]
parser = rangePair `endBy` newline where
  rangePair = (,) <$> range <* char ',' <*> range
  range = (,) <$> int <* char '-' <*> int
  int = read <$> some digit

type Range = (Int,Int)

part1 :: [(Range,Range)] -> Int
part1 = length . filter \(a,b) -> a `contains` b || b `contains` a where
  contains (olo,ohi) (ilo,ihi) = olo <= ilo && ihi <= ohi

part2 :: [(Range,Range)] -> Int
part2 = length . filter \(a,b) -> a `overlaps` b where
  overlaps (alo,ahi) (blo,bhi) = alo <= bhi && blo <= ahi
