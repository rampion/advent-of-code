module AdventOfCode.Y2022.Day9 where

import AdventOfCode.Y2022.Prelude
import Data.Set qualified as Set
import Data.List (scanl')

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = tellSpec do
      runCheck solver Example
        { raw = [text|
            R 4
            U 4
            L 3
            D 1
            R 4
            D 1
            L 5
            R 2
          |]
        , parsed = 
            [ (R, 4)
            , (U, 4)
            , (L, 3)
            , (D, 1)
            , (R, 4)
            , (D, 1)
            , (L, 5)
            , (R, 2)
            ]
        , part1output = 13
        , part2output = 1
        }

      let exampleInput2 = 
            [ (R, 5)
            , (U, 8)
            , (L, 8)
            , (D, 3)
            , (R, 17)
            , (D, 10)
            , (L, 25)
            , (U, 20)
            ]

      it "reports the correct output for part 2 of the larger example" do
        part2 exampleInput2 `shouldBe` 36
  }


type Motion = (Direction, Int)
data Direction = L | U | R | D
  deriving stock (Show, Eq, Read)

parser :: Parser [Motion]
parser = motion `endBy` newline where
  motion = (,) <$> direction <* char ' ' <*> distance
  direction = read . pure <$> oneOf "LURD"
  distance = read <$> some digit

{-------------------------------------------------------------------------------
 Two algorithms leap to mind.

 1) Rasterization

    Turn the instructions for the head into a list of coordinates for the head.
    Use that to get a list of tail coordinates, then turn the list into a set
    and get the size of the set.

    We can compute the tail coordinates pretty easily with

      data Coord a = Coord a a
        deriving Functor, Foldable, Num

      magnitude :: (Num a, Ord a) => Coord a -> a
      magnitude = maximum . abs

      step :: Num a => Coord a -> Coord a
      step = signum

      diff = newHead - oldTail
      newTail = oldTail +
        if magnitude diff > 1
        then step diff
        else 0

    This is O(# points) so it could be slow, depending on the input.

 2) Vectorization

    Turn the instructions for the head into instructions for the tail, and then
    convert that into a list of line segments. Bin the line segments by
    orientation (n-s,e-w,ne-se,nw-sw), combining overlapping segments.
    Calculate the points of overlap between the bins, with multiplicity. Sum
    the segment lengths and subtract the extra points from the intersections.

    This has a lot more steps, but is only O((# line segments)²) [assuming a
    crappy intersection algorithm], so could potentially be much faster for
    long segments.
      
 -------------------------------------------------------------------------------}

part1 :: [Motion] -> Int
part1 = Set.size . Set.fromList . toTailCoords . toHeadCoords 

toHeadCoords :: [Motion] -> [Coord Int]
toHeadCoords = scanl' step (Coord 0 0) . concatMap expand
  where expand (d, n) = replicate n d
        step (Coord x y) = \case
          L -> Coord (x - 1) y
          U -> Coord x (y + 1)
          R -> Coord (x + 1) y
          D -> Coord x (y - 1)

toTailCoords :: [Coord Int] -> [Coord Int]
toTailCoords = scanl1 follow where
  follow t@(Coord tx ty) (Coord hx hy)
    | abs dx `max` abs dy < 2 = t
    | otherwise = Coord (tx + signum dx) (ty + signum dy)
    where dx = hx - tx
          dy = hy - ty

data Coord a = Coord !a !a
  deriving stock (Show, Eq, Ord)

part2 :: [Motion] -> Int
part2 = Set.size . Set.fromList . toTailCoords9 . toHeadCoords 
  where toTailCoords9 = iterate (toTailCoords .) id !! 9
