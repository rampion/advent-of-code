{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day17 where

import AdventOfCode.Y2023.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Control.Monad (guard, (>=>))
import Data.Maybe (catMaybes)
-- import Debug.Trace (traceShow)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      let example1 =
            [ [ 2, 4, 1, 3, 4, 3, 2, 3, 1, 1, 3, 2, 3 ]
            , [ 3, 2, 1, 5, 4, 5, 3, 5, 3, 5, 6, 2, 3 ]
            , [ 3, 2, 5, 5, 2, 4, 5, 6, 5, 4, 2, 5, 4 ]
            , [ 3, 4, 4, 6, 5, 8, 5, 8, 4, 5, 4, 5, 2 ]
            , [ 4, 5, 4, 6, 6, 5, 7, 8, 6, 7, 5, 3, 6 ]
            , [ 1, 4, 3, 8, 5, 9, 8, 7, 9, 8, 4, 5, 4 ]
            , [ 4, 4, 5, 7, 8, 7, 6, 9, 8, 7, 7, 6, 6 ]
            , [ 3, 6, 3, 7, 8, 7, 7, 9, 7, 9, 6, 5, 3 ]
            , [ 4, 6, 5, 4, 9, 6, 7, 9, 8, 6, 8, 8, 7 ]
            , [ 4, 5, 6, 4, 6, 7, 9, 9, 8, 6, 4, 5, 3 ]
            , [ 1, 2, 2, 4, 6, 8, 6, 8, 6, 5, 5, 6, 3 ]
            , [ 2, 5, 4, 6, 5, 4, 8, 8, 8, 7, 7, 3, 5 ]
            , [ 4, 3, 2, 2, 6, 7, 4, 6, 5, 5, 5, 3, 3 ]
            ]
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            2413432311323
            3215453535623
            3255245654254
            3446585845452
            4546657867536
            1438598798454
            4457876987766
            3637877979653
            4654967986887
            4564679986453
            1224686865563
            2546548887735
            4322674655533
          |]
        , parsed = example1
        , part1output = Just 102
        , part2output = Just 94
        }
  }

type Input :: Type
type Input = [[Int]]

parser :: Parser Input
parser = some int `endBy` newline where
  int = read . pure <$> digit

part1 :: Input -> Maybe Int
part1 = (search 1 3 <*> fst . Map.findMax) . cacheCosts

part2 :: Input -> Maybe Int
part2 = (search 4 10 <*> fst . Map.findMax) . cacheCosts

cacheCosts :: Input -> Map.Map (Int,Int) Int
cacheCosts rows = Map.fromList do
  (y, cols) <- zip @Int [0..] rows
  (x, cost) <- zip @Int [0..] cols
  pure ((y,x), cost)

type X :: Type
type X = Int

type Y :: Type
type Y = Int

type Cost :: Type
type Cost = Int

type Order :: Type
type Order = Int

type Path :: Type
type Path = Map.Map (Y, X, Direction) Order

search :: Int -> Int -> Map.Map (Y,X) Cost -> (Y,X) -> Maybe Cost
search lo hi costs end = loop Set.empty initial where

  initial = Set.fromList do
    let (y,x) = end
        total = costs Map.! end - costs Map.! (0,0)
    d <- [North,West]
    pure (total, y, x, d)

  loop visited = Set.minView >=> \case
    ((total, 0, 0, _), _) -> pure total
    ((total, y, x, d), q) -> loop (Set.insert (y,x,d) visited) do
      q `Set.union` next visited total y x d

  next visited total y x d = Set.fromList do
    ((y, x, ds), c) <- drop (lo - 1) (withCost (step y x d))
    d <- ds
    let p = (y, x, d)
    guard do not (Set.member p visited)
    pure (total + c, y, x, d)

  step y x d = case d of
    North -> [(y - dy, x, [West,East]) | dy <- [1..hi]]
    West  -> [(y, x - dx, [North,South]) | dx <- [1..hi]]
    South -> [(y + dy, x, [West,East]) | dy <- [1..hi]]
    East  -> [(y, x + dx, [North,South]) | dx <- [1..hi]]

  withCost = zip <*> scanl1 (+) . catMaybes . map \(y,x,_) -> Map.lookup (y,x) costs

type Direction :: Type
data Direction
  = North
  | West
  | South
  | East
  deriving stock (Eq, Ord, Show)
