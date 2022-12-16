module AdventOfCode.Y2022.Day15 where

import AdventOfCode.Y2022.Prelude
import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = tellSpec do
        let exampleInput =
              [ Report {sensor = XY {x = 2, y = 18}, beacon = XY {x = -2, y = 15}}
              , Report {sensor = XY {x = 9, y = 16}, beacon = XY {x = 10, y = 16}}
              , Report {sensor = XY {x = 13, y = 2}, beacon = XY {x = 15, y = 3}}
              , Report {sensor = XY {x = 12, y = 14}, beacon = XY {x = 10, y = 16}}
              , Report {sensor = XY {x = 10, y = 20}, beacon = XY {x = 10, y = 16}}
              , Report {sensor = XY {x = 14, y = 17}, beacon = XY {x = 10, y = 16}}
              , Report {sensor = XY {x = 8, y = 7}, beacon = XY {x = 2, y = 10}}
              , Report {sensor = XY {x = 2, y = 0}, beacon = XY {x = 2, y = 10}}
              , Report {sensor = XY {x = 0, y = 11}, beacon = XY {x = 2, y = 10}}
              , Report {sensor = XY {x = 20, y = 14}, beacon = XY {x = 25, y = 17}}
              , Report {sensor = XY {x = 17, y = 20}, beacon = XY {x = 21, y = 22}}
              , Report {sensor = XY {x = 16, y = 7}, beacon = XY {x = 15, y = 3}}
              , Report {sensor = XY {x = 14, y = 3}, beacon = XY {x = 15, y = 3}}
              , Report {sensor = XY {x = 20, y = 1}, beacon = XY {x = 15, y = 3}}
              ]

        runCheck
          parser
          part1
          part2
          Example
            { raw =
                [text|
            Sensor at x=2, y=18: closest beacon is at x=-2, y=15
            Sensor at x=9, y=16: closest beacon is at x=10, y=16
            Sensor at x=13, y=2: closest beacon is at x=15, y=3
            Sensor at x=12, y=14: closest beacon is at x=10, y=16
            Sensor at x=10, y=20: closest beacon is at x=10, y=16
            Sensor at x=14, y=17: closest beacon is at x=10, y=16
            Sensor at x=8, y=7: closest beacon is at x=2, y=10
            Sensor at x=2, y=0: closest beacon is at x=2, y=10
            Sensor at x=0, y=11: closest beacon is at x=2, y=10
            Sensor at x=20, y=14: closest beacon is at x=25, y=17
            Sensor at x=17, y=20: closest beacon is at x=21, y=22
            Sensor at x=16, y=7: closest beacon is at x=15, y=3
            Sensor at x=14, y=3: closest beacon is at x=15, y=3
            Sensor at x=20, y=1: closest beacon is at x=15, y=3
          |]
            , parsed = exampleInput
            , part1output = 26
            , part2output = [56_000_011]
            }

        it "reports the correct ineligble segments for part 1 of the example" do
          findSegments 10 exampleInput `shouldBe` [(-2, 1), (3, 24)]

        it "detects correct radiuses for part 1 of the example" do
          map radius exampleInput
            `shouldBe` [ 7 -- Report {sensor=XY{x=2, y=18}, beacon=XY{x= -2, y=15}}
                       , 1 -- Report {sensor=XY{x=9, y=16}, beacon=XY{x=10, y=16}}
                       , 3 -- Report {sensor=XY{x=13, y=2}, beacon=XY{x=15, y=3}}
                       , 4 -- Report {sensor=XY{x=12, y=14}, beacon=XY{x=10, y=16}}
                       , 4 -- Report {sensor=XY{x=10, y=20}, beacon=XY{x=10, y=16}}
                       , 5 -- Report {sensor=XY{x=14, y=17}, beacon=XY{x=10, y=16}}
                       , 9 -- Report {sensor=XY{x=8, y=7}, beacon=XY{x=2, y=10}}
                       , 10 -- Report {sensor=XY{x=2, y=0}, beacon=XY{x=2, y=10}}
                       , 3 -- Report {sensor=XY{x=0, y=11}, beacon=XY{x=2, y=10}}
                       , 8 -- Report {sensor=XY{x=20, y=14}, beacon=XY{x=25, y=17}}
                       , 6 -- Report {sensor=XY{x=17, y=20}, beacon=XY{x=21, y=22}}
                       , 5 -- Report {sensor=XY{x=16, y=7}, beacon=XY{x=15, y=3}}
                       , 1 -- Report {sensor=XY{x=14, y=3}, beacon=XY{x=15, y=3}}
                       , 7 -- Report {sensor=XY{x=20, y=1}, beacon=XY{x=15, y=3}}
                       ]

        it "detects correct overlaps for part 1 of the example" do
          map (overlapY 10) exampleInput
            `shouldBe` [ Nothing -- Report {sensor=XY{x=2, y=18}, beacon=XY{x= -2, y=15}}
                       , Nothing -- Report {sensor=XY{x=9, y=16}, beacon=XY{x=10, y=16}}
                       , Nothing -- Report {sensor=XY{x=13, y=2}, beacon=XY{x=15, y=3}}
                       , Just (12, 12) -- Report {sensor=XY{x=12, y=14}, beacon=XY{x=10, y=16}}
                       , Nothing -- Report {sensor=XY{x=10, y=20}, beacon=XY{x=10, y=16}}
                       , Nothing -- Report {sensor=XY{x=14, y=17}, beacon=XY{x=10, y=16}}
                       , Just (3, 14) -- Report {sensor=XY{x=8, y=7}, beacon=XY{x=2, y=10}}
                       , Nothing -- Report {sensor=XY{x=2, y=0}, beacon=XY{x=2, y=10}}
                       , Just (-2, 1) -- Report {sensor=XY{x=0, y=11}, beacon=XY{x=2, y=10}}
                       , Just (16, 24) -- Report {sensor=XY{x=20, y=14}, beacon=XY{x=25, y=17}}
                       , Nothing -- Report {sensor=XY{x=17, y=20}, beacon=XY{x=21, y=22}}
                       , Just (14, 18) -- Report {sensor=XY{x=16, y=7}, beacon=XY{x=15, y=3}}
                       , Nothing -- Report {sensor=XY{x=14, y=3}, beacon=XY{x=15, y=3}}
                       , Nothing -- Report {sensor=XY{x=20, y=1}, beacon=XY{x=15, y=3}}
                       ]

        it "reports the correct coordinates for part 2 of the example" do
          locate 20 exampleInput `shouldBe` [XY 14 11]
    }

type Report :: Type
data Report = Report {sensor :: XY, beacon :: XY}
  deriving stock (Show, Eq)

type XY :: Type
data XY = XY {x :: Int, y :: Int}
  deriving stock (Show, Eq, Ord)

parser :: Parser [Report]
parser = report `endBy` newline
  where
    report = Report <$> sensor <* string ": " <*> beacon
    sensor = string "Sensor " *> xy
    beacon = string "closest beacon is " *> xy
    xy = XY <$ string "at x=" <*> int <* string ", y=" <*> int
    int = sign <*> nat
    sign = maybe id (const negate) <$> optional (char '-')
    nat = read <$> some digit

part1y :: Int -> [Report] -> Int
part1y y = sum . map segmentSize . findSegments y

findSegments' :: Bool -> Int -> [Report] -> [Segment]
findSegments' b y = unify . mapMaybe (overlapY' b y)

findSegments :: Int -> [Report] -> [Segment]
findSegments = findSegments' False

unify :: [Segment] -> [Segment]
unify = reverse . foldl' step [] . sort
  where
    step [] p = [p]
    step ps@((lo, hi) : pt) p@(ll, hh)
      | ll <= hi = (lo, max hi hh) : pt
      | otherwise = p : ps

segmentSize :: Segment -> Int
segmentSize (lo, hi) = hi - lo + 1

overlapY :: Int -> Report -> Maybe Segment
overlapY = overlapY' False

overlapY' :: Bool -> Int -> Report -> Maybe Segment
overlapY' bb yy r@Report {sensor = XY {x = sx, y = sy}, beacon = XY {x = bx, y = by}}
  | dx < 0 = Nothing
  | yy /= by || bb = Just (lo, hi)
  | lo == hi = Nothing
  | lo == bx = Just (bx + 1, hi)
  | otherwise = Just (lo, hi - 1)
  where
    lo = sx - dx
    hi = sx + dx
    dx = radius r - dy
    dy = abs (yy - sy)

radius :: Report -> Int
radius Report {sensor, beacon} = abs (x sensor - x beacon) + abs (y sensor - y beacon)

type Segment :: Type
type Segment = (Int, Int)

part1 :: [Report] -> Int
part1 = part1y 2_000_000

part2s :: Int -> [Report] -> [Int]
part2s s = fmap tuningFrequency . locate s

locate :: Int -> [Report] -> [XY]
locate n reports = do
  y <- [0 .. n]
  let ps = findSegments' True y reports
  ((_, lo), (hi, _)) <- zip ((-1, -1) : ps) (ps ++ [(n + 1, n + 1)])
  x <- [lo + 1 .. hi - 1]
  pure XY {x, y}

tuningFrequency :: XY -> Int
tuningFrequency XY {x, y} = 4_000_000 * x + y

part2 :: [Report] -> [Int]
part2 = part2s 4_000_000
