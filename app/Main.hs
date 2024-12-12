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

import Control.Monad (filterM)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable (traverse_)
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.List qualified as List
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
  solveDay parse1 part2 $ fromGregorian 2024 12 12

type Puzzle = [[Char]]

parse1 :: Parser Puzzle
parse1 = (many letter `endBy1` newline) <* eof

part1 :: Puzzle -> Int
part1 = sum . map price . (regions =<< perimeter) . grid @Int @Int

part2 :: Puzzle -> Int
part2 = sum . map price . (regions =<< sides) . grid @Int @Int

grid :: (Integral x, Integral y) => Puzzle -> Grid x y
grid rows = Map.fromList [((x, y), c) | (y, row) <- zip [0 ..] rows, (x, c) <- zip [0 ..] row]

price :: Region x y -> Int
price = liftA2 (*) Map.size sum

perimeter :: (Integral x, Integral y) => Grid x y -> Count x y
perimeter g p c = length [() | q <- neighbors p, Map.lookup q g /= Just c]

sides :: (Integral x, Integral y) => Grid x y -> Count x y
sides g (x, y) c = length $ filter id [t, l, r, b]
  where
    isDiff q = Map.lookup q g /= Just c
    n = isDiff (x, y - 1)
    nw = isDiff (x - 1, y - 1)
    w = isDiff (x - 1, y)
    sw = isDiff (x - 1, y + 1)
    s = isDiff (x, y + 1)
    se = isDiff (x + 1, y + 1)
    e = isDiff (x + 1, y)
    ne = isDiff (x + 1, y - 1)

    t = n && (w || not nw)
    l = w && (s || not sw)
    b = s && (e || not se)
    r = e && (n || not ne)

neighbors :: (Integral x, Integral y) => (x, y) -> [(x, y)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

type Grid x y = Map (x, y) Char

type Region x y = Map (x, y) Int

type Count x y = (x, y) -> Char -> Int

regions :: (Integral x, Integral y) => Count x y -> Grid x y -> [Region x y]
regions count = Maybe.catMaybes . (`State.evalState` Set.empty) . liftA2 traverse (dfs count) Map.toList

dfs :: (Integral x, Integral y) => Count x y -> Grid x y -> ((x, y), Char) -> State (Set (x, y)) (Maybe (Region x y))
dfs count g (p, c) =
  State.gets (Set.member p) >>= \case
    True -> pure Nothing
    False -> do
      State.modify (Set.insert p)
      Just <$> search count g c Map.empty [p]

search :: (Integral x, Integral y) => Count x y -> Grid x y -> Char -> Region x y -> [(x, y)] -> State (Set (x, y)) (Region x y)
search count g c r = \case
  [] -> pure r
  p : rest -> do
    unseen <- filterM
      do
        \q ->
          if Map.lookup q g == Just c
            then State.gets (not . Set.member q)
            else pure False
      do neighbors p
    traverse_ (State.modify . Set.insert) unseen
    search count g c (Map.insert p (count p c) r) (unseen ++ rest)

firstExample :: Puzzle
firstExample =
  [ ['A', 'A', 'A', 'A']
  , ['B', 'B', 'C', 'D']
  , ['B', 'B', 'C', 'C']
  , ['E', 'E', 'E', 'C']
  ]

secondExample :: Puzzle
secondExample =
  [ ['O', 'O', 'O', 'O', 'O']
  , ['O', 'X', 'O', 'X', 'O']
  , ['O', 'O', 'O', 'O', 'O']
  , ['O', 'X', 'O', 'X', 'O']
  , ['O', 'O', 'O', 'O', 'O']
  ]

thirdExample :: Puzzle
thirdExample =
  [ ['R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'F', 'F']
  , ['R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'C', 'F']
  , ['V', 'V', 'R', 'R', 'R', 'C', 'C', 'F', 'F', 'F']
  , ['V', 'V', 'R', 'C', 'C', 'C', 'J', 'F', 'F', 'F']
  , ['V', 'V', 'V', 'V', 'C', 'J', 'J', 'C', 'F', 'E']
  , ['V', 'V', 'I', 'V', 'C', 'C', 'J', 'J', 'E', 'E']
  , ['V', 'V', 'I', 'I', 'I', 'C', 'J', 'J', 'E', 'E']
  , ['M', 'I', 'I', 'I', 'I', 'I', 'J', 'J', 'E', 'E']
  , ['M', 'I', 'I', 'I', 'S', 'I', 'J', 'E', 'E', 'E']
  , ['M', 'M', 'M', 'I', 'S', 'S', 'J', 'E', 'E', 'E']
  ]

fourthExample :: Puzzle
fourthExample =
  [ ['E', 'E', 'E', 'E', 'E']
  , ['E', 'X', 'X', 'X', 'X']
  , ['E', 'E', 'E', 'E', 'E']
  , ['E', 'X', 'X', 'X', 'X']
  , ['E', 'E', 'E', 'E', 'E']
  ]

fifthExample :: Puzzle
fifthExample =
  [ ['A', 'A', 'A', 'A', 'A', 'A']
  , ['A', 'A', 'A', 'B', 'B', 'A']
  , ['A', 'A', 'A', 'B', 'B', 'A']
  , ['A', 'B', 'B', 'A', 'A', 'A']
  , ['A', 'B', 'B', 'A', 'A', 'A']
  , ['A', 'A', 'A', 'A', 'A', 'A']
  ]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            AAAA
            BBCD
            BBCC
            EEEC
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 140

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 80

  it "parses the second example" do
    let raw =
          [text|
            OOOOO
            OXOXO
            OOOOO
            OXOXO
            OOOOO
          |]
            <> "\n"

    parse parse1 "second example" raw `shouldBe` Right secondExample

  it "solves part one with the second example" do
    part1 secondExample `shouldBe` 772

  it "solves part two with the second example" do
    part2 secondExample `shouldBe` 436

  it "parses the third example" do
    let raw =
          [text|
            RRRRIICCFF
            RRRRIICCCF
            VVRRRCCFFF
            VVRCCCJFFF
            VVVVCJJCFE
            VVIVCCJJEE
            VVIIICJJEE
            MIIIIIJJEE
            MIIISIJEEE
            MMMISSJEEE
          |]
            <> "\n"

    parse parse1 "third example" raw `shouldBe` Right thirdExample

  it "solves part one with the third example" do
    part1 thirdExample `shouldBe` 1930

  it "solves part two with the third example" do
    part2 thirdExample `shouldBe` 1206

  it "parses the fourth example" do
    let raw =
          [text|
            EEEEE
            EXXXX
            EEEEE
            EXXXX
            EEEEE
          |]
            <> "\n"

    parse parse1 "fourth example" raw `shouldBe` Right fourthExample

  it "solves part two with the fourth example" do
    part2 fourthExample `shouldBe` 236

  it "parses the fifth example" do
    let raw =
          [text|
            AAAAAA
            AAABBA
            AAABBA
            ABBAAA
            ABBAAA
            AAAAAA
          |]
            <> "\n"

    parse parse1 "fifth example" raw `shouldBe` Right fifthExample

  it "solves part two with the fifth example" do
    part2 fifthExample `shouldBe` 368
