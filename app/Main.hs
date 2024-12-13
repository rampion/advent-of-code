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

-- import Data.Ratio ((%))
-- import Control.Monad qualified as Monad
-- import Control.Monad.Trans.State.Strict (State)
-- import Control.Monad.Trans.State.Strict qualified as State
-- import Data.Foldable (traverse_)
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
-- import Data.Map (Map)
-- import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
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
  solveDay parse1 part2 $ fromGregorian 2024 12 13

type Puzzle = [Machine]

data Machine = Machine
  { buttonA :: Point
  , buttonB :: Point
  , prize :: Point
  }
  deriving (Show, Eq)

data Point = Point {x :: Int, y :: Int}
  deriving (Show, Eq)

parse1 :: Parser Puzzle
parse1 = (machine `sepBy` newline) <* eof
  where
    machine = Machine <$> button 'A' <*> button 'B' <*> prize
    button c = Point <$ string "Button " <* char c <* string ": X+" <*> int <* string ", Y+" <*> int <* newline
    prize = Point <$ string "Prize: X=" <*> int <* string ", Y=" <*> int <* newline
    int = read <$> many1 digit

part1 :: Puzzle -> Int
part1 = sum . Maybe.mapMaybe cost

part2 :: Puzzle -> Int
part2 = part1 . map \Machine{buttonA,buttonB,prize=Point{x,y}} ->
  Machine{buttonA,buttonB,prize=Point (x+offset) (y+offset)}
  where offset = 10_000_000_000_000

cost :: Machine -> Maybe Int
cost Machine {buttonA, buttonB, prize} = do
  -- prize.x = a * buttonA.x + b * buttonB.x
  -- prize.y = a * buttonA.y + b * buttonB.y

  -- a = (prize.x - b * buttonB.x) / buttonA.x
  -- a = (prize.y - b * buttonB.y) / buttonA.y
  --
  -- (b * buttonB.y - prize.y) / buttonA.y = (b * buttonB.x - prize.x) / buttonA.x
  -- b * buttonB.y * buttonA.x - prize.y * buttonA.x = b * buttonB.x * buttonA.y - prize.x * buttonA.y
  -- b = (prize.y * buttonA.x - prize.x * buttonA.y) / (buttonB.y * buttonA.x - buttonB.x * buttonA.y)
  (b, 0) <- pure $ (prize.y * buttonA.x - prize.x * buttonA.y) `quotRem` (buttonB.y * buttonA.x - buttonB.x * buttonA.y)
  (a, 0) <- pure $ (prize.x - b * buttonB.x) `quotRem` buttonA.x
  pure $ 3*a + b

firstExample :: Puzzle
firstExample =
  [ Machine {buttonA = Point 94 34, buttonB = Point 22 67, prize = Point 8_400 5_400}
  , Machine {buttonA = Point 26 66, buttonB = Point 67 21, prize = Point 12_748 12_176}
  , Machine {buttonA = Point 17 86, buttonB = Point 84 37, prize = Point 7_870 6_450}
  , Machine {buttonA = Point 69 23, buttonB = Point 27 71, prize = Point 18_641 10_279}
  ]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            Button A: X+94, Y+34
            Button B: X+22, Y+67
            Prize: X=8400, Y=5400

            Button A: X+26, Y+66
            Button B: X+67, Y+21
            Prize: X=12748, Y=12176

            Button A: X+17, Y+86
            Button B: X+84, Y+37
            Prize: X=7870, Y=6450

            Button A: X+69, Y+23
            Button B: X+27, Y+71
            Prize: X=18641, Y=10279
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 480

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 875318608908
