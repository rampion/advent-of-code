module AdventOfCode.Day15 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec hiding ((<|>), optional)
import Control.Applicative
import Data.List (foldl',sort)
import Data.Maybe (mapMaybe)

data Report = Report { sensor :: XY, beacon :: XY }
  deriving stock (Show, Eq)

data XY = XY { x :: Int, y :: Int }
  deriving stock (Show, Eq, Ord)

day15parser :: Parser [Report]
day15parser = report `endBy` newline where
  report = Report <$> sensor <* string ": " <*> beacon
  sensor = string "Sensor " *> xy
  beacon = string "closest beacon is " *> xy
  xy = XY <$ string "at x=" <*> int <* string ", y=" <*> int
  int = sign <*> nat
  sign = maybe id (const negate) <$> optional (char '-')
  nat = read <$> some digit

day15part1y :: Int -> [Report] -> Int
day15part1y y = sum . map segmentSize . findSegments y

findSegments' :: Bool -> Int -> [Report] -> [Segment]
findSegments' b y = unify . mapMaybe (overlapY' b y)

findSegments :: Int -> [Report] -> [Segment]
findSegments = findSegments' False

unify :: [Segment] -> [Segment]
unify = reverse . foldl' step [] . sort where
  step [] p = [p]
  step ps@((lo,hi):pt) p@(ll,hh)
    | ll <= hi = (lo, max hi hh) : pt
    | otherwise = p : ps

segmentSize :: Segment -> Int
segmentSize (lo,hi) = hi - lo + 1

overlapY :: Int -> Report -> Maybe Segment
overlapY = overlapY' False

overlapY' :: Bool -> Int -> Report -> Maybe Segment
overlapY' bb yy r@Report{sensor=XY{x=sx,y=sy},beacon=XY{x=bx,y=by}}
  | dx < 0    = Nothing
  | yy /= by  || bb = Just (lo,hi)
  | lo == hi  = Nothing
  | lo == bx  = Just (bx+1,hi)
  | otherwise = Just (lo,hi-1)
  where
    lo = sx - dx
    hi = sx + dx
    dx = radius r - dy
    dy = abs (yy - sy)

radius :: Report -> Int
radius Report{sensor,beacon} = abs (x sensor - x beacon) + abs (y sensor - y beacon)

type Segment = (Int, Int)

day15part1 :: [Report] -> Int
day15part1 = day15part1y 2_000_000

day15part2s :: Int -> [Report] -> [Int]
day15part2s s = fmap tuningFrequency . locate s

locate :: Int -> [Report] -> [XY]
locate n reports = do
  y <- [0 .. n]
  let ps = findSegments' True y reports
  ((_,lo),(hi,_)) <- zip ((-1,-1):ps) (ps ++ [(n+1,n+1)])
  x <- [lo + 1 .. hi - 1]
  pure XY{x,y}

tuningFrequency :: XY -> Int
tuningFrequency XY{x,y} = 4_000_000 * x + y

day15part2 :: [Report] -> [Int]
day15part2 = day15part2s 4_000_000
