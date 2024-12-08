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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (fromGregorian)
import Data.List qualified as List
import NeatInterpolation (text)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 08

type Puzzle = [[Tile]]

data Tile = Blank | Antenna Char
  deriving (Show, Eq)

parse1 :: Parser Puzzle
parse1 = (row `endBy1` newline) <* eof where
  row = many tile
  tile = blank <|> antenna
  blank = Blank <$ char '.'
  antenna = Antenna <$> oneOf (['a'..'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

part1 :: Puzzle -> Int
part1 puzzle =
  Set.size .
  Set.fromList .
  filter (inBounds puzzle) .
  concat .
  Map.elems .
  fmap antinodes $
  groupByFrequency puzzle

part2 :: Puzzle -> Int
part2 puzzle =
  Set.size .
  Set.fromList .
  concat .
  Map.elems .
  fmap (harmonicAntinodes puzzle) $
  groupByFrequency puzzle

groupByFrequency :: Puzzle -> Map Char [(Int, Int)]
groupByFrequency rows = Map.fromListWith (<>) do
  (y, columns) <- zip [0..] rows
  (x, Antenna f) <- zip [0..] columns
  pure (f, [(x,y)])

antinodes :: [(Int,Int)] -> [(Int, Int)]
antinodes ps = do
  p : qs <- List.tails ps
  q <- qs
  ((x0,y0),(x1,y1)) <- [(p,q), (q,p)]
  pure (2*x0 - x1, 2*y0 - y1)

harmonicAntinodes :: Puzzle -> [(Int,Int)] -> [(Int, Int)]
harmonicAntinodes puzzle =
  let check = inBounds puzzle
  in
  \ps -> do
    (x0, y0) : qs <- List.tails (List.sort ps)
    (x1, y1) <- qs
    let dx = x1 - x0
        dy = y1 - y0
        m = gcd dx dy
        ix = dx `div` m
        iy = dy `div` m

    concat
      [ takeWhile check [(x0 - n * ix,y0 - n * iy) | n <- [0..]]
      , takeWhile check [(x0 + n * ix,y0 + n * iy) | n <- [1..]]
      ]

inBounds :: Puzzle -> (Int,Int) -> Bool
inBounds puzzle = \(x,y) -> 0 <= x && x < width && 0 <= y && y < height where
  height = length puzzle
  width = case puzzle of [] -> 0 ; row : _ -> length row

firstExample :: Puzzle
firstExample =
  [ [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Antenna '0', Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Antenna '0', Blank, Blank, Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Antenna '0', Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Antenna '0', Blank, Blank, Blank, Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Antenna 'A', Blank, Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Antenna 'A', Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Antenna 'A', Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank ]
  , [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank ]
  ]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            ............
            ........0...
            .....0......
            .......0....
            ....0.......
            ......A.....
            ............
            ............
            ........A...
            .........A..
            ............
            ............
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 14

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 34
