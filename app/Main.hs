{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import SolveDay (solveDay)
import Control.Monad (guard)
import Data.Time (fromGregorian)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Maybe (fromJust, listToMaybe)
import Data.List qualified as List
import NeatInterpolation (text)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 06

type Puzzle = [[Cell]]

data Cell = Feature Tile | Guard Direction
  deriving (Show, Eq)

data Tile = Blank | Obstacle
  deriving (Show, Eq)

data Direction = North | East | South | West
  deriving (Show, Eq, Ord)

parse1 :: Parser Puzzle
parse1 = row `endBy1` newline
  where
    row = many cell
    cell = feature <|> guard
    feature = Feature <$> tile
    tile = blank <|> obstacle
    blank = Blank <$ char '.'
    obstacle = Obstacle <$ char '#'
    guard = Guard <$> direction
    direction = north <|> east <|> south <|> west
    north = North <$ char '^'
    east = East <$ char '>'
    south = South <$ char 'v'
    west = West <$ char '<'

part1 :: Puzzle -> Int
part1 = Set.size . Set.fromList . map fst . route

part2 :: Puzzle -> Int
part2 p = length do
  let cs = cells p
      g = grid cs
      s = start cs
  p <- Set.toList . Set.fromList . map fst . drop 1 $ path g s
  guard $ containsDup $ path (Map.insert p Obstacle g) s

containsDup :: Ord a => [a] -> Bool
containsDup = loop Set.empty where
  loop _ [] = False
  loop seen (x:xs)
    | Set.member x seen = True
    | otherwise = loop (Set.insert x seen) xs


route :: Puzzle -> [((Int, Int), Direction)]
route = (path <$> grid <*> start) . cells

type Cells = [(Int, Int, Cell)]

cells :: Puzzle -> Cells
cells = concat . zipWith labelRow [0..] where
  labelRow y = zipWith (labelCell y) [0..]
  labelCell y x c = (x,y,c)

start :: Cells -> ((Int, Int), Direction)
start cells = fromJust $ listToMaybe do
  (x,y,Guard d) <- cells
  pure ((x,y),d)

grid :: Cells -> Map (Int, Int) Tile
grid cells = Map.fromList do
  (x,y,c) <- cells
  pure ((x,y),case c of Feature t -> t ; _ -> Blank)

path :: Map (Int, Int) Tile -> ((Int, Int), Direction) -> [((Int, Int), Direction)]
path g = \t -> t : List.unfoldr (uncurry walk) t where
  walk p d = do
    let test = step p d
    Map.lookup test g >>= \case
      Blank -> pure ((test, d), (test, d))
      Obstacle -> walk p (turn d)
  step (x,y) = \case
    North -> (x, y - 1)
    East  -> (x + 1, y)
    South -> (x, y + 1)
    West  -> (x - 1, y)
  turn = \case
    North -> East
    East -> South
    South -> West
    West -> North

firstExample :: Puzzle
firstExample =
  [ [Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Obstacle, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank]
  , [Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Obstacle]
  , [Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank]
  , [Feature Blank, Feature Blank, Feature Obstacle, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank]
  , [Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Obstacle, Feature Blank, Feature Blank]
  , [Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank]
  , [Feature Blank, Feature Obstacle, Feature Blank, Feature Blank, Guard North, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank]
  , [Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Obstacle, Feature Blank]
  , [Feature Obstacle, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank]
  , [Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Blank, Feature Obstacle, Feature Blank, Feature Blank, Feature Blank]
  ]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            ....#.....
            .........#
            ..........
            ..#.......
            .......#..
            ..........
            .#..^.....
            ........#.
            #.........
            ......#...
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 41

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 6
