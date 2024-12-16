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
-- import Data.Maybe qualified as Maybe
-- import Data.Set (Set)
-- import Data.Set qualified as Set
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
  solveDay parse1 part2 $ fromGregorian 2024 12 15

data Puzzle = Puzzle
  { warehouse :: [[Tile]]
  , directions :: [Direction]
  }
  deriving (Eq, Show)

data Tile = Wall | Vacant | Box | Robot
  deriving (Eq, Show)

data Direction = North | South | West | East
  deriving (Eq, Show)

parse1 :: Parser Puzzle
parse1 = Puzzle <$> warehouse <* newline <*> directions <* eof
  where
    warehouse = row `endBy` newline
    row = many1 tile
    tile = Foldable.asum [ Wall <@ '#', Vacant <@ '.', Box <@ 'O', Robot <@ '@' ]
    directions = concat <$> (many direction `endBy` newline)
    direction = Foldable.asum [ North <@ '^', South <@ 'v', West <@ '<', East <@ '>' ]

    (<@) :: a -> Char -> Parser a
    (<@) a c = a <$ char c

data Layout tile = Layout
  { objects :: Map Point tile
  , robot :: Point
  }
  deriving (Show, Eq)

data Point = Point { x :: X, y :: Y }
  deriving (Eq, Ord, Show)

newtype X = X { getX :: Int }
  deriving newtype (Show, Eq, Ord, Enum, Num)

newtype Y = Y { getY :: Int }
  deriving newtype (Show, Eq, Ord, Enum, Num)

part1 :: Puzzle -> Int
part1 Puzzle{warehouse,directions} = score1 $ List.foldl' step1 (layout1 warehouse) directions

part2 :: Puzzle -> Int
part2 Puzzle{warehouse,directions} = score2 $ List.foldl' step2 (layout2 warehouse) directions

layout1 :: [[Tile]] -> Layout Tile
layout1 rows = Layout {objects, robot} where
  objects = Map.fromList [ (Point{x,y}, t) | (y, row) <- zip [0..] rows, (x, t) <- zip [0..] row ]
  robot = case [ p | (p, Robot) <- Map.toList objects ] of
            [p] -> p
            _ -> error "bad puzzle"

data Tile' = Wall' | Vacant' | Robot' | WestBox' | EastBox'

layout2 :: [[Tile]] -> Layout Tile'
layout2 rows = Layout {objects, robot} where
  objects = Map.fromList do
    (y, row) <- zip [0..] rows
    (x, t) <- zip [0..] row
    let (t1,t2) = case t of
                    Wall -> (Wall', Wall')
                    Vacant -> (Vacant', Vacant')
                    Robot -> (Robot', Vacant')
                    Box -> (WestBox', EastBox')
    [(Point{x=2*x,y}, t1),(Point{x=2*x+1,y}, t2)]

  robot = case [ p | (p, Robot') <- Map.toList objects ] of
            [p] -> p
            _ -> error "bad puzzle"

step1 :: Layout Tile -> Direction -> Layout Tile
step1 layout@Layout{objects,robot=old} (delta -> d) =
  case span (maybe False movable . snd) . map (\p -> (p, Map.lookup p objects)) $ iterate d old of
    (path, (q, Just Vacant):_) ->
      Layout
        { robot = d old
        , objects = Map.fromList [ (p, t) | let (ps, ts) = unzip path, (p, Just t) <- zip (ps ++ [q]) (Just Vacant : ts) ] `Map.union` objects
        }
    _ -> layout

step2 :: Layout Tile' -> Direction -> Layout Tile'
step2 layout@Layout{objects,robot} dir@(delta -> d) = batch (Map.insert robot Vacant' objects) (Map.singleton robot'  Robot') where
  robot' = d robot

  batch objects moves = push objects Map.empty moves (Map.toList moves)

  push objects next curr = \case
    []
      | Map.null next -> Layout {robot=robot', objects}
      | otherwise -> batch objects next
    (p, t):rest ->
      let objects' = Map.insert p t objects
          next' = Map.insert (d p) t' next
          t' = objects Map.! p

          pushBox px tx
            | dir `elem` [West,East] || Map.member px curr = push objects' next' curr rest
            | otherwise = push (Map.insert px Vacant' objects') (Map.insert (d px) tx next') curr rest
      in
      case t' of
        Wall' -> layout
        Vacant' -> push objects' next curr rest
        Robot' -> push objects' next curr rest
        WestBox' -> pushBox p { x = p.x + 1 } EastBox'
        EastBox' -> pushBox p { x = p.x - 1 } WestBox'


movable :: Tile -> Bool
movable = \case
  Robot -> True
  Box -> True
  Vacant -> False
  Wall -> False

delta :: Direction -> Point -> Point
delta = \case
  North -> \p -> p { y = p.y - 1 }
  South -> \p -> p { y = p.y + 1 }
  West -> \p -> p { x = p.x - 1 }
  East -> \p -> p { x = p.x + 1 }


score1 :: Layout Tile -> Int
score1 Layout{objects} = sum [ 100*y + x | (Point (X x) (Y y), Box) <- Map.toList objects ]

score2 :: Layout Tile' -> Int
score2 Layout{objects} = sum [ 100*y + x | (Point (X x) (Y y), WestBox') <- Map.toList objects ]

firstExample :: Puzzle
firstExample = Puzzle
  { warehouse =
      [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
      , [ Wall, Vacant, Vacant, Box, Vacant, Vacant, Box, Vacant, Box, Wall ]
      , [ Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Box, Vacant, Wall ]
      , [ Wall, Vacant, Box, Box, Vacant, Vacant, Box, Vacant, Box, Wall ]
      , [ Wall, Vacant, Vacant, Box, Robot, Vacant, Vacant, Box, Vacant, Wall ]
      , [ Wall, Box, Wall, Vacant, Vacant, Box, Vacant, Vacant, Vacant, Wall ]
      , [ Wall, Box, Vacant, Vacant, Box, Vacant, Vacant, Box, Vacant, Wall ]
      , [ Wall, Vacant, Box, Box, Vacant, Box, Vacant, Box, Box, Wall ]
      , [ Wall, Vacant, Vacant, Vacant, Vacant, Box, Vacant, Vacant, Vacant, Wall ]
      , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
      ]
  , directions =
      [ West, South, South, East, North, West, South, North, East, South, East, North, South, South, North, South, East, South
      , West, East, South, North, South, West, South, West, North, South, South, West, West, West, North, East, West, West
      , East, West, East, East, South, West, South, South, South, West, East, North, South, North, East, North, West, West
      , West, East, West, West, South, West, West, West, South, North, South, South, North, South, East, North, South, South
      , South, West, West, North, East, North, South, North, North, East, West, West, East, East, East, West, East, North
      , West, West, East, West, North, South, South, North, North, West, East, South, South, South, West, East, East, West
      , North, North, South, East, North, East, South, South, West, East, South, West, West, West, West, South, West, North, South
      , East, North, West, North, North, East, East, East, North, West, South, West, South, East, West, East, South, South
      , East, South, North, South, North, West, East, East, West, East, East, East, East, West, North, North, East, South
      , South, East, South, West, North, North, North, East, East, South, North, South, North, West, North, North, East, South, North, North
      , East, South, North, West, North, South, East, South, West, East, East, South, North, South, North, West, South, East
      , South, North, North, West, North, North, South, South, West, West, West, South, West, North, East, East, North, North, North, North
      , East, East, East, South, North, West, East, South, South, South, North, East, West, South, West, West, West
      , East, North, North, North, South, South, North, West, South, South, South, East, North, East, South, West, North, North, North, North
      , South, West, East, North, East, South, South, South, South, East, West, East, East, South, North, West, West, North
      , North, North, North, North, North, East, West, North, East, West, East, East, East, West, East, North, North, West, West, North
      , North, South, East, East, East, West, North, West, South, East, North, West, South, South, East, East, South, East
      , East, East, North, South, East, West, East, North, South, East, West, West, West, West, South, East, East
      , South, West, South, West, South, East, South, South, South, East, North, West, East, West, West, East, North, East
      , West, North, East, East, West, East, North, South, West, East, West, North, South, South, South, West, North, North, West
      , East, West, South, West, West, West, West, West, East, West, North, South, West, West, West, East, West, West
      , West, North, North, West, South, West, North, North, North, East, West, North, East, East, North, West, South, North, East, West
      , West, West, North, East, East, North, South, West, South, North, South, West, South, North, East, North, East, East, North
      , South, East, South, South, East, North, West, West, North, South, West, East, East, West, West, East, West, West
      , South, West, West, South, East, West, East, South, West, North, South, South, West, West, West, East, North, North
      , South, North, East, North, North, East, East, East, West, West, North, South, East, East, South, North, South, East
      , West, North, North, East, East, North, West, East, South, South, North, West, East, West, North, North, East, North, North, North
      , West, East, West, South, South, South, South, South, North, South, West, South, West, West, East, North, South, West
      , South, East, South, West, West, North, East, West, West, East, West, West, East, West, West, West, North, North
      , West, West, West, North, West, West, East, East, West, West, East, West, North, North, North, East, North, North, West
      , East, North, East, South, West, East, North, North, East, South, South, West, North, South, North, South, West, South, South
      , East, North, West, East, West, South, West, North, South, East, North, North, North, East, East, East, North, North, South
      , South, South, North, East, South, South, South, West, East, East, East, North, West, North, East, East, East
      , East, East, North, West, West, North, South, East, North, South, South, South, West, East, North, West, East, West
      , West, South, East, South, North, North, East, East, East, West, West, North, North, West, East, East, North, South, North
      , West, South, North, South, South, West, East, South, North, West, West, East, North, West, North, South, North, South, East
      , West, North, West, West, West, East, West, West, North, West, South, East, West, South, West, East, South, South
      , East, East, South, East, West, South, North, West, South, South, West, East, South, North, West, West, North
      ]
  }

secondExample :: Puzzle
secondExample = Puzzle
  { warehouse =
      [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
      , [ Wall, Vacant, Vacant, Box, Vacant, Box, Vacant, Wall ]
      , [ Wall, Wall, Robot, Vacant, Box, Vacant, Vacant, Wall ]
      , [ Wall, Vacant, Vacant, Vacant, Box, Vacant, Vacant, Wall ]
      , [ Wall, Vacant, Wall, Vacant, Box, Vacant, Vacant, Wall ]
      , [ Wall, Vacant, Vacant, Vacant, Box, Vacant, Vacant, Wall ]
      , [ Wall, Vacant, Vacant, Vacant, Vacant, Vacant, Vacant, Wall ]
      , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
      ]
  , directions =
      [ West, North, North, East, East, East, South, South, West, South, East, East, South, West, West ]
  }

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            ##########
            #..O..O.O#
            #......O.#
            #.OO..O.O#
            #..O@..O.#
            #O#..O...#
            #O..O..O.#
            #.OO.O.OO#
            #....O...#
            ##########

            <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
            vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
            ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
            <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
            ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
            ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
            >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
            <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
            ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
            v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 10092

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 9021

  it "parses the second example" do
    let raw =
          [text|
            ########
            #..O.O.#
            ##@.O..#
            #...O..#
            #.#.O..#
            #...O..#
            #......#
            ########

            <^^>>>vv<v>>v<<
          |]
            <> "\n"

    parse parse1 "second example" raw `shouldBe` Right secondExample

  it "solves part one with the second example" do
    part1 secondExample `shouldBe` 2028
