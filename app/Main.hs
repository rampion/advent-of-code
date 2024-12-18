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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing -Wno-ambiguous-fields #-}

module Main where

import Control.Monad qualified as Monad
import Data.Bits (shiftR, xor)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.Maybe qualified as Maybe
import Data.Time (fromGregorian)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
-- import Debug.Trace
import NeatInterpolation (text)
import SolveDay (solveDay)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec hiding (State)
import Text.Parsec.Text (Parser)
import Prelude

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 17

data Puzzle = Puzzle
  { registers :: Registers
  , program :: Program
  }
  deriving (Show, Eq)

data Registers = Registers {a :: Int, b :: Int, c :: Int}
  deriving (Show, Eq)

type Program = [Opcode]

--  0     1     2     3     4     5     6     7
data Opcode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
  deriving (Show, Eq, Ord, Enum, Bounded)

parse1 :: Parser Puzzle
parse1 = Puzzle <$> registers <* newline <*> program <* eof
  where
    registers = Registers <$> register 'A' <*> register 'B' <*> register 'C'
    register c = string "Register " *> char c *> string ": " *> int <* newline
    int = read <$> many1 digit
    program = string "Program: " *> sepBy opcode comma <* newline
    opcode = Foldable.asum [op <$ string (show (fromEnum op)) | op <- [ADV .. CDV]]
    comma = char ','

firstExample :: Puzzle
firstExample =
  Puzzle
    { registers = Registers {a = 729, b = 0, c = 0}
    , program = map toEnum [0, 1, 5, 4, 3, 0]
    }

part1 :: Puzzle -> [Int]
part1 Puzzle {program, registers} =
  generateOutput (Vector.fromList program) registers 0

generateOutput :: Vector Opcode -> Registers -> Int -> [Int]
generateOutput program = loop
  where
    loop registers p
      | p + 1 >= Vector.length program = []
      | otherwise =
          let literal = fromEnum $ program Vector.! (p + 1)
              combo = case literal of
                4 -> a registers
                5 -> b registers
                6 -> c registers
                n -> n
              p' = p + 2
              opcode = program Vector.! p
           in -- in traceShow (p, opcode, literal, registers) case opcode of
              case opcode of
                ADV ->
                  -- divide register a / 2 ^ combo and store in a
                  loop registers {a = a registers `shiftR` combo} p'
                BXL ->
                  -- xor register b with literal
                  loop registers {b = b registers `xor` literal} p'
                BST ->
                  -- store combo mod 8 in register b
                  loop registers {b = combo `mod` 8} p'
                JNZ ->
                  -- jump to literal if register a nonzero
                  loop registers if a registers == 0 then p' else literal
                BXC ->
                  -- xor register b with register c and store in b
                  loop registers {b = b registers `xor` c registers} p'
                OUT ->
                  -- output combo mod 8
                  (combo `mod` 8) : loop registers p'
                BDV ->
                  -- divide register a / 2 ^ combo and store in b
                  loop registers {b = a registers `shiftR` combo} p'
                CDV ->
                  -- divide register a / 2 ^ combo and store in c
                  loop registers {c = a registers `shiftR` combo} p'

secondExample :: Puzzle
secondExample =
  Puzzle
    { registers = Registers {a = 2024, b = 0, c = 0}
    , program = map toEnum [0, 3, 5, 4, 3, 0]
    }

day17 :: Puzzle
day17 =
  Puzzle
    { registers = Registers 66171486 0 0
    , program = map toEnum [2, 4, 1, 6, 7, 5, 4, 6, 1, 4, 5, 5, 0, 3, 3, 0]
    }

part2 :: Puzzle -> Maybe Int
part2 puzzle = Maybe.listToMaybe do
  let program = Vector.fromList puzzle.program
      outputs = map fromEnum puzzle.program
  a <- [0 ..]
  let eq = (==) `on` take 2
  Monad.guard do
    generateOutput program puzzle.registers {a} 0 `eq` outputs
  pure a

-- $/> program day17

-- $/> generateOutput (Vector.fromList (program day17)) (registers day17) { a =  233 } 0

-- $> :set -XNamedFieldPuns

-- $> [(a, o) | hi <- [11367361724445,11367361726493], let x = [2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0], lo <- [0..7], let a = 8 * hi + lo, let o = generateOutput (Vector.fromList (program day17)) (registers day17) { a } 0, o == x]

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            Register A: 729
            Register B: 0
            Register C: 0

            Program: 0,1,5,4,3,0
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` [4, 6, 3, 5, 6, 3, 5, 2, 1, 0]

  it "parses the second example" do
    let raw =
          [text|
            Register A: 2024
            Register B: 0
            Register C: 0

            Program: 0,3,5,4,3,0
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right secondExample

  it "solves part two with the second example" do
    part2 secondExample `shouldBe` Just 117_440

{-
Register A: 66171486
Register B: 0
Register C: 0

Program: 2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0

0: bst 4/a
  b <- a & 7
2: bxl 6
  b ^= 6
4: cdv 5/b
  c = a >> b
6: bxc 6
  b ^= c
8: bxl 4
  b ^= 4
a: out 5/b
  out b
c: adv 3
  a >>= 3
e: jnz 0

f a = (a & 7) ^ (a >> ((a & 7) ^ 6)) ^ 2

0 = ( a ^ (a >> (a ^ 6)) ) & 7

2 = f a
4 = f (a >>> 3)
2 = f (a >>> 6)
6 = f (a >>> 9)
7 = f (a >>> 12)
5 = f (a >>> 15)
4 = f (a >>> 18)
6 = f (a >>> 21)
1 = f (a >>> 24)
4 = f (a >>> 27)
5 = f (a >>> 30)
5 = f (a >>> 33)
0 = f (a >>> 36)
3 = f (a >>> 39)
3 = f (a >>> 42)
0 = f (a >>> 45)

a >>> 42 /= 0
a >>> 45 == 0

  4   3   3   3   3   2   2   2   1   1   1   0   0   0   0
  2   9   6   3   0   7   4   1   8   5   2   9   6   3   0
???_???_???_???_???_???_???_???_???_???_???_???_???_???_???
010_
010_001_
010_100_

f a = (a & 7) ^ (a >> ((a & 7) ^ 6)) ^ 2
  f 0 = 0 ^ (0 >> 6) ^ 2 = 2
  f 1 = 1 ^ (1 >> 7) ^ 2 = 3
  f 2 = 2 ^ (2 >> 4) ^ 2 = 0
  f 3 = 3 ^ (3 >> 5) ^ 2 = 1
  f 4 = 4 ^ (4 >> 2) ^ 2 = 7
  f 5 = 5 ^ (5 >> 3) ^ 2 = 7
  f 6 = 6 ^ (6 >> 0) ^ 2 = 2
  f 7 = 7 ^ (7 >> 1) ^ 2 = 6

better way to solve it - work backwards output by output
-}
