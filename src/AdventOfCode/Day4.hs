module AdventOfCode.Day4 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec
import Control.Applicative (some)

day4parser :: Parser [(Range,Range)]
day4parser = rangePair `endBy` newline where
  rangePair = (,) <$> range <* char ',' <*> range
  range = (,) <$> int <* char '-' <*> int
  int = read <$> some digit

type Range = (Int,Int)

day4part1 :: [(Range,Range)] -> Int
day4part1 = length . filter \(a,b) -> a `contains` b || b `contains` a where
  contains (olo,ohi) (ilo,ihi) = olo <= ilo && ihi <= ohi

day4part2 :: [(Range,Range)] -> Int
day4part2 = length . filter \(a,b) -> a `overlaps` b where
  overlaps (alo,ahi) (blo,bhi) = alo <= bhi && blo <= ahi
