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
import Control.Monad qualified as Monad
-- import Control.Monad.State.Strict (MonadState)
-- import Control.Monad.State.Strict qualified as MonadState
-- import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
-- import Control.Monad.Trans.Maybe qualified as MaybeT
-- import Control.Monad.Trans.State.Strict (StateT (runStateT))
-- import Control.Monad.Trans.Writer.Strict (Writer, runWriter)
-- import Control.Monad.Writer.Strict qualified as MonadWriter
-- import Control.Concurrent (threadDelay)
-- import Data.Bits
-- import Data.Foldable qualified as Foldable
import Data.Function (on)
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
import Data.Set (Set)
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

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 23

type Puzzle = [Connection]

type Connection = (Computer, Computer)

type Computer = (Char, Char)

parse1 :: Parser Puzzle
parse1 = (connection `endBy` newline) <* eof
  where
    connection = (,) <$> computer <* char '-' <*> computer
    computer = (,) <$> lower <*> lower

firstExample :: Puzzle
firstExample =
  [ (('k', 'h'), ('t', 'c'))
  , (('q', 'p'), ('k', 'h'))
  , (('d', 'e'), ('c', 'g'))
  , (('k', 'a'), ('c', 'o'))
  , (('y', 'n'), ('a', 'q'))
  , (('q', 'p'), ('u', 'b'))
  , (('c', 'g'), ('t', 'b'))
  , (('v', 'c'), ('a', 'q'))
  , (('t', 'b'), ('k', 'a'))
  , (('w', 'h'), ('t', 'c'))
  , (('y', 'n'), ('c', 'g'))
  , (('k', 'h'), ('u', 'b'))
  , (('t', 'a'), ('c', 'o'))
  , (('d', 'e'), ('c', 'o'))
  , (('t', 'c'), ('t', 'd'))
  , (('t', 'b'), ('w', 'q'))
  , (('w', 'h'), ('t', 'd'))
  , (('t', 'a'), ('k', 'a'))
  , (('t', 'd'), ('q', 'p'))
  , (('a', 'q'), ('c', 'g'))
  , (('w', 'q'), ('u', 'b'))
  , (('u', 'b'), ('v', 'c'))
  , (('d', 'e'), ('t', 'a'))
  , (('w', 'q'), ('a', 'q'))
  , (('w', 'q'), ('v', 'c'))
  , (('w', 'h'), ('y', 'n'))
  , (('k', 'a'), ('d', 'e'))
  , (('k', 'h'), ('t', 'a'))
  , (('c', 'o'), ('t', 'c'))
  , (('w', 'h'), ('q', 'p'))
  , (('t', 'b'), ('v', 'c'))
  , (('t', 'd'), ('y', 'n'))
  ]

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

part1 :: Puzzle -> Int
part1 = length . tTriangles

tTriangles :: Puzzle -> [(Computer, Computer, Computer)]
tTriangles cs = do
  let sorted = [if (a < b) then (a, b) else (b, a) | (a, b) <- cs]
  let edges = Set.fromList sorted
  let neighbors = Map.fromListWith (<>) do
        (a, b) <- sorted
        pure (a, Set.singleton b)
  (x, ns) <- Map.toList neighbors
  y : rest <- List.tails $ Set.toAscList ns
  z <- rest
  Monad.guard $ Set.member (y, z) edges && any ((== 't') . fst) [x, y, z]
  pure (x, y, z)

maximumOn :: (Ord a) => (b -> a) -> [b] -> b
maximumOn f = List.maximumBy (compare `on` f)

part2 :: Puzzle -> String
part2 cs = format . snd $ maximumOn fst do
  let sorted = [if (a < b) then (a, b) else (b, a) | (a, b) <- cs]
  let edges = Set.fromList sorted
  let neighbors = Map.fromListWith (<>) do
        (a, b) <- sorted
        pure (a, Set.singleton b)
  (x, ns) <- Map.toList neighbors
  let (m, cl) = maximumOn fst $ cliques edges $ Set.toAscList ns
  pure (m + 1, x : cl)

-- $> main

cliques :: (Ord a) => Set (a, a) -> [a] -> [(Int, [a])]
cliques edges = \case
  [] -> pure (0, [])
  a : as -> do
    let bs = filter (\b -> Set.member (a, b) edges) as
    [(m + 1, a : cl) | (m, cl) <- cliques edges bs] <> cliques edges as

format :: [Computer] -> String
format cs = List.intercalate "," [[a, b] | (a, b) <- cs]

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            kh-tc
            qp-kh
            de-cg
            ka-co
            yn-aq
            qp-ub
            cg-tb
            vc-aq
            tb-ka
            wh-tc
            yn-cg
            kh-ub
            ta-co
            de-co
            tc-td
            tb-wq
            wh-td
            ta-ka
            td-qp
            aq-cg
            wq-ub
            ub-vc
            de-ta
            wq-aq
            wq-vc
            wh-yn
            ka-de
            kh-ta
            co-tc
            wh-qp
            tb-vc
            td-yn
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 7

  it "solves part two with the second example" do
    part2 firstExample `shouldBe` format [('c', 'o'), ('d', 'e'), ('k', 'a'), ('t', 'a')]
