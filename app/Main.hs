{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing -Wno-ambiguous-fields #-}

module Main where

-- import Control.Monad ((>=>))
-- import Control.Monad qualified as Monad
-- import Control.Monad.State.Strict (MonadState)
-- import Control.Monad.State.Strict qualified as MonadState
-- import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
-- import Control.Monad.Trans.Maybe qualified as MaybeT
-- import Control.Monad.Trans.State.Strict (StateT (runStateT))
-- import Control.Monad.Trans.Writer.Strict (Writer, runWriter)
-- import Control.Monad.Writer.Strict qualified as MonadWriter
-- import Control.Concurrent (threadDelay)
import Data.Bits
-- import Data.Foldable qualified as Foldable
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.Functor.Const (Const (Const, getConst))
-- import Data.Functor.Identity (Identity (Identity, runIdentity))
-- import Data.Functor.Product (Product (Pair))
import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
-- import Data.Maybe qualified as Maybe
-- import Data.Ratio ((%))
-- import Data.Set (Set)
-- import Data.Set qualified as Set
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
  solveDay parse1 part2 $ fromGregorian 2024 12 22

type Puzzle = [SecretValue]

newtype SecretValue = SecretValue {getSecretValue :: Int}
  deriving newtype (Show, Eq, Bits, Read, Num)

parse1 :: Parser Puzzle
parse1 = (secretNumber `endBy` newline) <* eof
  where
    secretNumber = read <$> many1 digit

firstExample :: Puzzle
firstExample = [1, 10, 100, 2024]

secondExample :: Puzzle
secondExample = [1, 2, 3, 2024]

part1 :: Puzzle -> SecretValue
part1 = sum . map step2000

part2 :: Puzzle -> Price
part2 = maximum . Map.unionsWith (+) . map (yields . take 2001 . iterate step)

yields :: [SecretValue] -> Map Pattern Price
yields = Map.fromListWith (const id) . patternYields . map price

price :: SecretValue -> Price
price = Price . flip mod 10 . getSecretValue

-- $> main

patternYields :: [Price] -> [(Pattern, Price)]
patternYields ps = do
  let d :: Price -> Price -> Delta
      d (Price a) (Price b) = Delta (b - a)
  p0 : p1 : p2 : p3 : p4 : _ <- List.tails ps
  pure ((d p0 p1, d p1 p2, d p2 p3, d p3 p4), p4)

newtype Price = Price {getPrice :: Int}
  deriving newtype (Show, Eq, Ord, Num)

newtype Delta = Delta {getDelta :: Int}
  deriving newtype (Show, Eq, Ord, Num)

type Pattern = (Delta, Delta, Delta, Delta)

step2000 :: SecretValue -> SecretValue
step2000 initial = iterate step initial !! 2000

step :: SecretValue -> SecretValue
step = mul2048 . div32 . mul64
  where
    mask = 16_777_216 - 1
    mul64 n = xor n (n `shiftL` 6) .&. mask
    div32 n = xor n (n `shiftR` 5) .&. mask
    mul2048 n = xor n (n `shiftL` 11) .&. mask

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            1
            10
            100
            2024
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "correctly single-steps 123" do
    take 11 (iterate step 123)
      `shouldBe` [123, 15887950, 16495136, 527345, 704524, 1553684, 12683156, 11100544, 12249484, 7753432, 5908254]

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 37327623

  it "computes " do
    map step2000 firstExample `shouldBe` [8685429, 4700978, 15273692, 8667524]

  it "solves part two with the second example" do
    part2 secondExample `shouldBe` 23
