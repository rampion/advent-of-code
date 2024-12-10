{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Monad (guard)
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (fromGregorian)
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import NeatInterpolation (text)
import SolveDay (solveDay)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 10

type Puzzle = [[Height]]

type Height = Int

type X = Int

type Y = Int

parse1 :: Parser Puzzle
parse1 = row `endBy1` newline <* eof
  where
    row = many height
    height = read . pure <$> digit

part1 :: Puzzle -> Int
part1 puzz = sum [Set.size (starts Map.! p) | (p, _) <- lasts]
  where
    hs = heights puzz
    (heads, q) = List.span ((== 0) . snd) $ List.sortOn snd hs
    seed = Map.fromList [(p, Set.singleton p) | (p, _) <- heads]
    grid = Map.fromList hs
    starts = bfs grid seed q
    lasts = List.dropWhile ((< 9) . snd) q

part2 :: Puzzle -> Int
part2 puzz = sum [trails Map.! p | (p, _) <- lasts]
  where
    hs = heights puzz
    (heads, q) = List.span ((== 0) . snd) $ List.sortOn snd hs
    seed = Map.fromList [(p, 1) | (p, _) <- heads]
    grid = Map.fromList hs
    trails = bfs2 grid seed q
    lasts = List.dropWhile ((< 9) . snd) q

bfs2 :: Map (X, Y) Height -> Map (X, Y) Int -> [((X, Y), Height)] -> Map (X, Y) Int
bfs2 grid = foldl' \known (p@(x, y), h) ->
  Map.insert
    p
    do
      sum do
        p' <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        maybeToList do
          h' <- Map.lookup p' grid
          guard $ h' + 1 == h
          Map.lookup p' known
    known

bfs :: Map (X, Y) Height -> Map (X, Y) (Set (X, Y)) -> [((X, Y), Height)] -> Map (X, Y) (Set (X, Y))
bfs grid = foldl' \known (p@(x, y), h) ->
  Map.insert
    p
    do
      Set.unions do
        p' <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        maybeToList do
          h' <- Map.lookup p' grid
          guard $ h' + 1 == h
          Map.lookup p' known
    known

heights :: Puzzle -> [((X, Y), Height)]
heights = concat . zipWith row [0 ..]
  where
    row y = zipWith (cell y) [0 ..]
    cell y x h = ((x, y), h)

firstExample :: Puzzle
firstExample =
  [ [8, 9, 0, 1, 0, 1, 2, 3]
  , [7, 8, 1, 2, 1, 8, 7, 4]
  , [8, 7, 4, 3, 0, 9, 6, 5]
  , [9, 6, 5, 4, 9, 8, 7, 4]
  , [4, 5, 6, 7, 8, 9, 0, 3]
  , [3, 2, 0, 1, 9, 0, 1, 2]
  , [0, 1, 3, 2, 9, 8, 0, 1]
  , [1, 0, 4, 5, 6, 7, 3, 2]
  ]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            89010123
            78121874
            87430965
            96549874
            45678903
            32019012
            01329801
            10456732
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 36

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 81
