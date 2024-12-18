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
-- import Data.Bits (xor)
-- import Data.Foldable qualified as Foldable
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.Functor.Const (Const (Const, getConst))
-- import Data.Functor.Identity (Identity (Identity, runIdentity))
-- import Data.Functor.Product (Product (Pair))
import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
-- import Data.Map (Map)
import Data.Map qualified as Map
-- import Data.Maybe qualified as Maybe
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

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 18

type Puzzle = [Point]

data Point = Point {x :: X, y :: Y}
  deriving (Eq, Show, Ord)

newtype X = X Int
  deriving newtype (Show, Eq, Ord, Num)

newtype Y = Y Int
  deriving newtype (Show, Eq, Ord, Num)

parse1 :: Parser Puzzle
parse1 = points <* eof
  where
    points = point `endBy` newline
    point = Point <$> x <* char ',' <*> y
    x = X <$> int
    y = Y <$> int
    int = read <$> many1 digit

firstExample :: Puzzle
firstExample =
  [ Point 5 4
  , Point 4 2
  , Point 4 5
  , Point 3 0
  , Point 2 1
  , Point 6 3
  , Point 2 4
  , Point 1 5
  , Point 0 6
  , Point 3 3
  , Point 2 6
  , Point 5 1
  , Point 1 2
  , Point 5 5
  , Point 2 5
  , Point 6 5
  , Point 1 4
  , Point 0 4
  , Point 6 4
  , Point 1 1
  , Point 6 1
  , Point 1 0
  , Point 0 5
  , Point 1 6
  , Point 2 0
  ]

part1 :: Puzzle -> Maybe Int
part1 = part1With 70 70 . take 1024

part1Demo :: Puzzle -> Maybe Int
part1Demo = part1With 6 6 . take 12

part1With :: X -> Y -> Puzzle -> Maybe Int
part1With maxX maxY (Set.fromList -> blocked) = loop (Map.singleton start 0) (Map.singleton 0 (Set.singleton start))
  where
    start = Point 0 0
    end = Point maxX maxY

    loop costs queue = do
      ((cost, ps), queue) <- Map.minViewWithKey queue
      if Set.member end ps
        then pure cost
        else
          let cost' = succ cost
              xs = [(n, mc) | p <- Set.toList ps, n <- neighbors p, let mc = Map.lookup n costs, maybe True (> cost') mc]
              ns = map fst xs
              costs' = Map.fromList [(n, cost') | n <- ns] `Map.union` costs
              queue' = (if null ns then id else Map.insertWith (<>) cost' (Set.fromList ns)) do
                List.foldl' (\m (n, c) -> Map.adjust (Set.delete n) c m) queue [(n, c) | (n, Just c) <- xs]
           in loop costs' queue'

    neighbors p =
      filter (not . (`Set.member` blocked)) $
        concat
          [ [p {x = p.x - 1} | p.x > 0]
          , [p {y = p.y - 1} | p.y > 0]
          , [p {x = p.x + 1} | p.x < maxX]
          , [p {y = p.y + 1} | p.y < maxY]
          ]

part2 :: Puzzle -> Maybe Point
part2 = binarySearch 70 70 =<< length

part2Demo :: Puzzle -> Maybe Point
part2Demo = binarySearch 6 6 =<< length

-- $/> part1With 1 1 [Point 1 1]

-- $> binarySearch 6 6 25 firstExample

binarySearch :: X -> Y -> Int -> Puzzle -> Maybe Point
binarySearch maxX maxY numPoints ps = loop 0 numPoints
  where
    loop lo hi
      | lo < hi =
          let i = (lo + hi) `div` 2
           in if blocked i
                then loop lo (i - 1)
                else loop (i + 1) hi
      | blocked lo = Just (ps !! (lo - 1))
      | otherwise = Nothing

    blocked i = (== Nothing) . part1With maxX maxY $ take i ps

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            5,4
            4,2
            4,5
            3,0
            2,1
            6,3
            2,4
            1,5
            0,6
            3,3
            2,6
            5,1
            1,2
            5,5
            2,5
            6,5
            1,4
            0,4
            6,4
            1,1
            6,1
            1,0
            0,5
            1,6
            2,0
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1Demo firstExample `shouldBe` Just 22

  it "solves part two with the first example" do
    part2Demo firstExample `shouldBe` Just Point {x = 6, y = 1}
