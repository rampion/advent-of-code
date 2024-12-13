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

euclid :: Int -> Int -> (Int, Int, Int)
euclid = \a b -> if a < b then loop 1 0 0 1 a b else loop 0 1 1 0 b a where
  loop _ _ !ja !jb 0 d = (d, ja, jb)
  loop !ia !ib !ja !jb c d =
    let (q, r) = d `quotRem` c in
    loop (ja - q*ia) (jb - q*ib) ia ib r c

combos :: Int -> Int -> Int -> Maybe ((Int, Int), (Int,Int))
combos a b p = do
  let (g, ca, _cb) = euclid a b
      b' = b `quot` g
      l = lcm a b
      sa = l `quot` a
      sb = l `quot` b

  (p', 0) <- pure $ p `quotRem` g

  let na = ((ca + b') * p') `rem` b'
      nb = (p - a*na) `quot` b

  pure ((na,nb),(sa,-sb))

cost :: Machine -> Maybe Int
cost Machine {buttonA, buttonB, prize} = do
  ((nax,nbx), (sax,sbx)) <- combos buttonA.x buttonB.x prize.x
  ((nay,nby), (say,sby)) <- combos buttonA.y buttonB.y prize.y

  (tb, 0) <- pure $ ((nay-nax)*sbx*sby + nbx*sax*sby - nby*say*sbx) `quotRem` (sax*sby - say*sbx)

  (q, 0) <- pure $ (tb - nbx) `quotRem` sbx
  let ta = q * sax + nax

  pure $ ta * 3 + tb


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
