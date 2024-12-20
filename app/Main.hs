{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing -Wno-ambiguous-fields #-}

module Main where

-- import Control.Monad ((>=>))
import Control.Monad qualified as Monad
-- import Control.Monad.State.Strict (MonadState)
-- import Control.Monad.State.Strict qualified as MonadState
-- import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
-- import Control.Monad.Trans.Maybe qualified as MaybeT
-- import Control.Monad.Trans.State.Strict (StateT (runStateT))
-- import Control.Monad.Trans.Writer.Strict (Writer, runWriter)
-- import Control.Monad.Writer.Strict qualified as MonadWriter
-- import Control.Concurrent (threadDelay)
-- import Data.Bits (xor)
import Data.Foldable qualified as Foldable
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.Functor.Const (Const (Const, getConst))
-- import Data.Functor.Identity (Identity (Identity, runIdentity))
-- import Data.Functor.Product (Product (Pair))
-- import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
-- import Data.Ratio ((%))
-- import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (fromGregorian)
-- import Debug.Trace (traceShow)
-- import Data.Vector (Vector)
-- import Data.Vector qualified as Vector
import NeatInterpolation (text)
import SolveDay (solveDay)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec hiding (State)
import Text.Parsec.Text (Parser)
import Prelude

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 20

type Puzzle = [[Tile]]

data Tile = Wall | Track | Start | End
  deriving (Show, Eq)

parse1 :: Parser Puzzle
parse1 = rows <* eof
  where
    rows = many1 tile `endBy1` newline
    tile =
      Foldable.asum
        [ Wall <$ char '#'
        , Track <$ char '.'
        , Start <$ char 'S'
        , End <$ char 'E'
        ]

firstExample :: Puzzle
firstExample =
  [ [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  , [Wall, Track, Track, Track, Wall, Track, Track, Track, Wall, Track, Track, Track, Track, Track, Wall]
  , [Wall, Track, Wall, Track, Wall, Track, Wall, Track, Wall, Track, Wall, Wall, Wall, Track, Wall]
  , [Wall, Start, Wall, Track, Track, Track, Wall, Track, Wall, Track, Wall, Track, Track, Track, Wall]
  , [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Track, Wall, Track, Wall, Track, Wall, Wall, Wall]
  , [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Track, Wall, Track, Wall, Track, Track, Track, Wall]
  , [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Track, Wall, Track, Wall, Wall, Wall, Track, Wall]
  , [Wall, Wall, Wall, Track, Track, End, Wall, Track, Track, Track, Wall, Track, Track, Track, Wall]
  , [Wall, Wall, Wall, Track, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Track, Wall, Wall, Wall]
  , [Wall, Track, Track, Track, Wall, Wall, Wall, Track, Track, Track, Wall, Track, Track, Track, Wall]
  , [Wall, Track, Wall, Wall, Wall, Wall, Wall, Track, Wall, Track, Wall, Wall, Wall, Track, Wall]
  , [Wall, Track, Wall, Track, Track, Track, Wall, Track, Wall, Track, Wall, Track, Track, Track, Wall]
  , [Wall, Track, Wall, Track, Wall, Track, Wall, Track, Wall, Track, Wall, Track, Wall, Wall, Wall]
  , [Wall, Track, Track, Track, Wall, Track, Track, Track, Wall, Track, Track, Track, Wall, Wall, Wall]
  , [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  ]

-- $> main

part1 :: Puzzle -> Int
part1 = sum . countShortcuts 2 100

part2 :: Puzzle -> Int
part2 = sum . countShortcuts 20 100

countShortcuts :: Int -> Int -> Puzzle -> Map Int Int
countShortcuts n l = Map.fromListWith (+) . map (,1) . allShortcuts n l

allShortcuts :: Int -> Int -> Puzzle -> [Int]
allShortcuts n l = (shortcuts n l <*> distances) . course

data Direction = North | South | East | West

next :: Direction -> [Direction]
next = \case
  North -> [North, East, West]
  South -> [South, East, West]
  East -> [East, North, South]
  West -> [West, North, South]

step :: Direction -> Point -> Point
step = flip \p -> \case
  North -> p {y = p.y - 1}
  South -> p {y = p.y + 1}
  West -> p {x = p.x - 1}
  East -> p {x = p.x + 1}

printShortcut :: Puzzle -> [Point] -> IO ()
printShortcut rows skips = putStr . unlines $ do
  let codes = Map.fromList $ zip skips ['1' ..]
  (y, cols) <- zip [0 ..] rows
  pure do
    (x, tile) <- zip [0 ..] cols
    let p = Point x y
    pure case tile of
      _ | Just code <- Map.lookup p codes -> code
      Wall -> '#'
      Start -> 'S'
      End -> 'E'
      Track -> '.'

shortcuts :: Int -> Int -> Course -> Map Point Int -> [Int]
shortcuts n l _course distances = Map.elems $ Map.fromListWith max do
  (p, d) <- Map.toList distances
  dy <- [0 .. n]
  dx <- [0 .. n - dy]
  y <- [p.y - Y dy, p.y + Y dy]
  x <- [p.x - X dx, p.x + X dx]
  let p' = Point {x, y}
  d' <- Maybe.maybeToList $ Map.lookup p' distances
  let saved = d' - d - dy - dx
  Monad.guard $ saved >= l
  pure ((p, p'), saved)

data Course = Course
  { start :: Point
  , end :: Point
  , walls :: Map Point Bool
  }

data Point = Point {x :: X, y :: Y}
  deriving (Eq, Show, Ord)

newtype X = X {getX :: Int}
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

newtype Y = Y {getY :: Int}
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

distances :: Course -> Map Point Int
distances course = loop (Map.singleton course.end 0) (Set.singleton course.end) 1
  where
    loop ds ps !d =
      let update = Map.fromList do
            p <- Set.toList ps
            n <- (`step` p) <$> [North, South, East, West]
            Monad.guard do
              Just False == Map.lookup n course.walls && not (Map.member n ds)
            pure (n, d)
       in if Set.null ps
            then ds
            else loop (Map.union ds update) (Map.keysSet update) (d + 1)

course :: Puzzle -> Course
course rows =
  Course
    { start = only [p | (p, Start) <- labeled]
    , end = only [p | (p, End) <- labeled]
    , walls = Map.fromList [(p, t == Wall) | (p, t) <- labeled]
    }
  where
    only = \case
      [a] -> a
      _ -> error "no unique element"
    labeled = do
      (y, cols) <- zip [0 ..] rows
      (x, tile) <- zip [0 ..] cols
      pure (Point {x, y}, tile)

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            ###############
            #...#...#.....#
            #.#.#.#.#.###.#
            #S#...#.#.#...#
            #######.#.#.###
            #######.#.#...#
            #######.#.###.#
            ###..E#...#...#
            ###.#######.###
            #...###...#...#
            #.#####.#.###.#
            #.#...#.#.#...#
            #.#.#.#.#.#.###
            #...#...#...###
            ###############
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    countShortcuts 2 1 firstExample `shouldBe` Map.fromList [(2, 14), (4, 14), (6, 2), (8, 4), (10, 2), (12, 3), (20, 1), (36, 1), (38, 1), (40, 1), (64, 1)]

  it "solves part two with the first example" do
    countShortcuts 20 50 firstExample `shouldBe` Map.fromList [(50, 32), (52, 31), (54, 29), (56, 39), (58, 25), (60, 23), (62, 20), (64, 19), (66, 12), (68, 14), (70, 12), (72, 22), (74, 4), (76, 3)]
