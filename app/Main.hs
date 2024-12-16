{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

-- import Control.Concurrent (threadDelay)
-- import Data.Ratio ((%))
-- import Control.Monad qualified as Monad
-- import Control.Monad.Trans.State.Strict (State)
-- import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable qualified as Foldable
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (fromGregorian)
import NeatInterpolation (text)
import SolveDay (solveDay)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec hiding (State)
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 16

type Puzzle = [[Tile]]

data Tile = Wall | Vacant | Start | End
  deriving (Eq, Show)

parse1 :: Parser Puzzle
parse1 = (many tile `endBy1` newline) <* eof where
  tile = Foldable.asum [ Wall <@ '#', Vacant <@ '.', Start <@ 'S', End <@ 'E' ]

  (<@) :: a -> Char -> Parser a
  (<@) a c = a <$ char c

firstExample :: Puzzle
firstExample =
  [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  , [ Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, End, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Wall, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Wall, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Wall, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Start, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Wall ]
  , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  ]

secondExample :: Puzzle
secondExample =
  [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  , [ Wall, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, End, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Vacant, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Vacant, Wall, Wall, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Vacant, Wall, Wall, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Wall, Vacant, Wall ]
  , [ Wall, Vacant, Wall, Vacant, Wall, Vacant, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Vacant, Wall ]
  , [ Wall, Start, Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Wall ]
  , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  ]

data Scenario = Scenario
  { walls :: [Point]
  , start :: Point
  , facing :: Direction
  , end :: Point
  }

data Point = Point { x :: X, y :: Y }
  deriving (Eq, Ord, Show)

data Direction = North | South | West | East
  deriving (Eq, Ord, Show)

newtype X = X { getX :: Int }
  deriving newtype (Show, Eq, Ord, Enum, Num)

newtype Y = Y { getY :: Int }
  deriving newtype (Show, Eq, Ord, Enum, Num)

part1 :: Puzzle -> Int
part1 = maybe -1 fst . bfs . scenario

part2 :: Puzzle -> Int
part2 = maybe -1 (Set.size . snd) . bfs . scenario

bfs :: Scenario -> Maybe (Int, Set Point)
bfs Scenario{walls,start,facing,end}
  = minPath (allFacings [end]) (allFacings walls)
  . Map.singleton 0
  . Map.singleton (start, facing)
  $ Set.singleton start

allFacings :: [Point] -> Set (Point, Direction)
allFacings ps = Set.fromList [(p,d) | p <- ps, d <- [North,South,East,West]]

minPath :: Set (Point, Direction) -> Set (Point, Direction) -> Map Int Paths -> Maybe (Int, Set Point)
minPath ends known queued = do
  ((cost, (`Map.withoutKeys` known) -> paths), queued) <- Map.minViewWithKey queued
  case Map.elems $ Map.restrictKeys paths ends of
    [] ->
      minPath ends (known `Set.union` Map.keysSet paths)
      . Map.unionWith combinePaths queued
      $ Map.fromListWith combinePaths do
          ((p,d), ps) <- Map.toList paths
          (c, p', d') <- [(cost + 1000, p, turnLeft d), (cost + 1000, p, turnRight d), (cost + 1, forward d p, d)]
          pure (c, Map.singleton (p',d') (Set.insert p' ps))
    sets -> pure (cost, mconcat sets)

type Paths = Map (Point, Direction) (Set Point)

combinePaths :: Paths -> Paths -> Paths
combinePaths = Map.unionWith Set.union

scenario :: Puzzle -> Scenario
scenario rows = Scenario
  { walls = [ p | (p, Wall) <- indexed ]
  , start = fst . Maybe.fromJust $ List.find ((== Start) . snd) indexed
  , facing = East
  , end = fst . Maybe.fromJust $ List.find ((== End) . snd) indexed
  }
  where
    indexed = [(Point{x,y}, t) | (y,row) <- zip [0..] rows, (x,t) <- zip [0..] row]

turnLeft :: Direction -> Direction
turnLeft = \case
  North -> West
  West -> South
  South -> East
  East -> North

turnRight :: Direction -> Direction
turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

forward :: Direction -> Point -> Point
forward = \case
  North -> \p -> p { y = p.y - 1 }
  South -> \p -> p { y = p.y + 1 }
  East -> \p -> p { x = p.x + 1 }
  West -> \p -> p { x = p.x - 1 }

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            ###############
            #.......#....E#
            #.#.###.#.###.#
            #.....#.#...#.#
            #.###.#####.#.#
            #.#.#.......#.#
            #.#.#####.###.#
            #...........#.#
            ###.#.#####.#.#
            #...#.....#.#.#
            #.#.#.###.#.#.#
            #.....#...#.#.#
            #.###.#.#.#.#.#
            #S..#.....#...#
            ###############
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 7036

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 45

  it "parses the second example" do
    let raw =
          [text|
            #################
            #...#...#...#..E#
            #.#.#.#.#.#.#.#.#
            #.#.#.#...#...#.#
            #.#.#.#.###.#.#.#
            #...#.#.#.....#.#
            #.#.#.#.#.#####.#
            #.#...#.#.#.....#
            #.#.#####.#.###.#
            #.#.#.......#...#
            #.#.###.#####.###
            #.#.#...#.....#.#
            #.#.#.#####.###.#
            #.#.#.........#.#
            #.#.#.#########.#
            #S#.............#
            #################
          |]
            <> "\n"

    parse parse1 "second example" raw `shouldBe` Right secondExample

  it "solves part one with the second example" do
    part1 secondExample `shouldBe` 11048

  it "solves part two with the second example" do
    part2 secondExample `shouldBe` 64
