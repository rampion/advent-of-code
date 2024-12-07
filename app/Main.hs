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
import Data.Time (fromGregorian)
import Data.List.NonEmpty (NonEmpty(..))
import NeatInterpolation (text)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 07

type Puzzle = [(Int, NonEmpty Int)]
type Op = Int -> Int -> Int

parse1 :: Parser Puzzle
parse1 = equation `endBy1` newline where
  equation = (,) <$> int <* char ':' <*> nums
  int = read <$> many digit
  nums = (:|) <$> num <*> many num
  num = char ' ' *> int

part1 :: Puzzle -> Int
part1 = solve [(+), (*)]

part2 :: Puzzle -> Int
part2 = solve [(+), (*), concatenation]

concatenation :: Int -> Int -> Int
concatenation n m = read (show n <> show m)

solve :: [Op] -> Puzzle -> Int
solve ops = sum . map fst . filter (uncurry (solvable ops))

solvable :: [Op] -> Int -> NonEmpty Int -> Bool
solvable ops testValue (num :| nums) = any (== testValue) do
  totals ops nums [num]

totals :: [Op] -> [Int] -> [Int] -> [Int]
totals ops = \case
  [] -> id
  m : ms -> \ns -> totals ops ms do
    op <- ops
    n <- ns
    pure (n `op` m)

firstExample :: Puzzle
firstExample =
  [ (190, 10 :| [19])
  , (3267, 81 :| [40, 27])
  , (83, 17 :| [5])
  , (156, 15 :| [6])
  , (7290, 6 :| [8, 6, 15])
  , (161011, 16 :| [10, 13])
  , (192, 17 :| [8, 14])
  , (21037, 9 :| [7, 18, 13])
  , (292, 11 :| [6, 16, 20])
  ]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            190: 10 19
            3267: 81 40 27
            83: 17 5
            156: 15 6
            7290: 6 8 6 15
            161011: 16 10 13
            192: 17 8 14
            21037: 9 7 18 13
            292: 11 6 16 20
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 3749

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 11387
