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

-- import Control.Monad (guard)
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
-- import Data.Map (Map)
import Data.Map qualified as Map
-- import Data.Maybe (maybeToList)
-- import Data.Set (Set)
-- import Data.Set qualified as Set
import Data.Time (fromGregorian)
import NeatInterpolation (text)
import SolveDay (solveDay)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 11

type Puzzle = [Int]

parse1 :: Parser Puzzle
parse1 = stones <* newline <* eof
  where
    stones = stone `sepBy` char ' '
    stone = read <$> many digit

part1 :: Puzzle -> Int
part1 = countAfter 25

part2 :: Puzzle -> Int
part2 = countAfter 75

countAfter :: Int -> Puzzle -> Int
countAfter n puzz = sum do
  let initial = zip puzz $ repeat 1
  snd <$> iterate blink initial !! n

blink :: (Integral stone, Num count) => [(stone, count)] -> [(stone, count)]
blink = Map.toList . Map.fromListWith (+) . concatMap \(stone, count) -> case stone of
  0 -> pure (1, count)
  (numDigits -> w) | even w -> do
    let (q, r) = stone `quotRem` (10 ^ (w `div` 2))
    [(q, count), (r, count)]
  _ -> pure (stone * 2024, count)

numDigits :: Integral a => a -> a
numDigits = ceiling @Double . logBase 10 . toEnum . fromEnum . succ

firstExample :: Puzzle
firstExample = [125, 17]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            125 17
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 55312
