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
-- import Data.Bits (xor)
import Data.Foldable qualified as Foldable
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

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 21

type Puzzle = [[Numpad]]

{-
  +---+---+---+
  | 7 | 8 | 9 |
  +---+---+---+
  | 4 | 5 | 6 |
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
      | 0 | A |
      +---+---+
-}
data Numpad = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA
  deriving (Show, Eq, Ord, Enum)

graphNumpad :: [Edge Numpad]
graphNumpad =
  [ Edge N7 N8 R
  , Edge N7 N4 D
  , Edge N8 N7 L
  , Edge N8 N5 D
  , Edge N8 N9 R
  , Edge N9 N8 L
  , Edge N9 N6 D
  , Edge N4 N7 U
  , Edge N4 N5 R
  , Edge N4 N1 D
  , Edge N5 N4 L
  , Edge N5 N8 U
  , Edge N5 N6 R
  , Edge N5 N2 D
  , Edge N6 N5 L
  , Edge N6 N9 U
  , Edge N6 N3 D
  , Edge N1 N4 U
  , Edge N1 N2 R
  , Edge N2 N1 L
  , Edge N2 N5 U
  , Edge N2 N3 R
  , Edge N2 N0 D
  , Edge N3 N2 L
  , Edge N3 N6 U
  , Edge N3 NA D
  , Edge N0 N2 U
  , Edge N0 NA R
  , Edge NA N0 L
  , Edge NA N3 U
  ]

newtype Cost = Cost {numPushes :: Word}
  deriving (Show, Eq, Ord, Num)

moveToAndPress ::
  (Ord inner) =>
  [Edge inner] ->
  (Dirpad -> Dirpad -> Cost) ->
  (inner -> inner -> Cost)
moveToAndPress graph moveToAndPressOuter = \start end -> cached Map.! (start, end)
  where
    neighbors = Map.fromListWith (<>) do
      Edge {start, end, dir} <- graph
      outerStart <- dirpads
      let outerEnd = Dir dir
      pure $
        ( (start, outerStart)
        , Map.singleton (end, outerEnd) (moveToAndPressOuter outerStart outerEnd)
        )

    moveTo = Map.fromList do
      startBoth <- Map.keys neighbors
      pure (startBoth, dijkstra neighbors startBoth)

    cached = Map.fromListWith min do
      ((start, DA), moveFromStart) <- Map.toList moveTo
      ((end, lastPushed), costToMove) <- Map.toList moveFromStart
      let costToPush = moveToAndPressOuter lastPushed DA
      pure ((start, end), costToMove + costToPush)

dijkstra :: forall a. (Ord a) => Map a (Map a Cost) -> a -> Map a Cost
dijkstra neighbors = loop Map.empty . Set.singleton . (,) 0
  where
    loop :: Map a Cost -> Set (Cost, a) -> Map a Cost
    loop ds q = case Set.minView q of
      Nothing -> ds
      Just ((cost, start), q)
        | Map.member start ds -> loop ds q
        | otherwise ->
            loop (Map.insert start cost ds) $
              q `Set.union` Set.fromList do
                (neighbor, cost') <- Map.toList $ neighbors Map.! start
                Monad.guard . not $ Map.member neighbor ds
                pure (cost + cost', neighbor)

{-
      +---+---+
      | ^ | A |
  +---+---+---+
  | < | v | > |
  +---+---+---+
-}
data Dirpad = Dir Dir | DA
  deriving (Show, Eq, Ord)

dirpads :: [Dirpad]
dirpads = [DA, Dir U, Dir D, Dir L, Dir R]

graphDirpad :: [Edge Dirpad]
graphDirpad =
  [ Edge (Dir U) DA R
  , Edge (Dir U) (Dir D) D
  , Edge DA (Dir U) L
  , Edge DA (Dir R) D
  , Edge (Dir L) (Dir D) R
  , Edge (Dir D) (Dir L) L
  , Edge (Dir D) (Dir U) U
  , Edge (Dir D) (Dir R) R
  , Edge (Dir R) (Dir D) L
  , Edge (Dir R) DA U
  ]

data Edge v = Edge
  { start :: v
  , end :: v
  , dir :: Dir
  }
  deriving (Show, Eq)

data Dir = U | D | L | R
  deriving (Show, Eq, Ord)

parse1 :: Parser Puzzle
parse1 = codes <* eof
  where
    codes = many1 numpad `endBy1` newline
    numpad =
      Foldable.asum
        [ N0 <$ char '0'
        , N1 <$ char '1'
        , N2 <$ char '2'
        , N3 <$ char '3'
        , N4 <$ char '4'
        , N5 <$ char '5'
        , N6 <$ char '6'
        , N7 <$ char '7'
        , N8 <$ char '8'
        , N9 <$ char '9'
        , NA <$ char 'A'
        ]

firstExample :: Puzzle
firstExample =
  [ example1a
  , example1b
  , example1c
  , example1d
  , example1e
  ]

example1a, example1b, example1c, example1d, example1e :: [Numpad]
example1a = [N0, N2, N9, NA]
example1b = [N9, N8, N0, NA]
example1c = [N1, N7, N9, NA]
example1d = [N4, N5, N6, NA]
example1e = [N3, N7, N9, NA]

part1 :: Puzzle -> Int
part1 = sum . map (complexity 3)

complexity :: Int -> [Numpad] -> Int
complexity numBots =
  let cached = shortestSequenceLength numBots
   in \code -> cached code * numericValue code

shortestSequenceLength :: Int -> [Numpad] -> Int
shortestSequenceLength 0 = length
shortestSequenceLength numBots = fromEnum . numPushes . sum . (zipWith transitionLength =<< (NA :))
  where
    transitionLength :: Numpad -> Numpad -> Cost
    transitionLength = moveToAndPress graphNumpad do
      iterate (moveToAndPress graphDirpad) human !! (numBots - 1)

    human :: a -> a -> Cost
    human _ _ = 1

numericValue :: [Numpad] -> Int
numericValue = List.foldl' (\t d -> t * 10 + fromEnum d) 0 . takeWhile (/= NA)

part2 :: Puzzle -> Int
part2 = sum . map (complexity 26)

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            029A
            980A
            179A
            456A
            379A
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 126_384

  let cached3 = shortestSequenceLength 3

  it "finds an optimal sequence for the first example code" do
    cached3 example1a
      `shouldBe` length
        ( concat
            [ [Dir L, Dir D, DA]
            , [Dir L, DA]
            , [DA]
            , [Dir R, Dir R, Dir U, DA]
            , [Dir D, DA]
            , [DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [Dir D, DA]
            , [Dir U, DA]
            , [Dir L, Dir D, DA]
            , [Dir R, Dir U, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, Dir U, DA]
            , [Dir R, DA]
            , [DA]
            , [Dir D, DA]
            , [Dir U, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, DA]
            , [Dir R, Dir U, DA]
            , [DA]
            , [DA]
            , [Dir D, DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            ]
        )

  it "finds an optimal sequence for the second example code" do
    cached3 example1b
      `shouldBe` length
        ( concat
            [ [Dir L, Dir D, Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [DA]
            , [DA]
            , [Dir D, DA]
            , [Dir U, DA]
            , [Dir L, Dir D, DA]
            , [Dir L, DA]
            , [DA]
            , [Dir R, Dir R, Dir U, DA]
            , [Dir D, DA]
            , [DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, DA]
            , [Dir R, Dir U, DA]
            , [DA]
            , [DA]
            , [Dir D, DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, DA]
            , [Dir R, Dir U, DA]
            , [Dir L, DA]
            , [Dir R, DA]
            ]
        )

  it "finds an optimal sequence for the third example code" do
    cached3 example1c
      `shouldBe` length
        ( concat
            [ [Dir L, Dir D, Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [Dir L, Dir D, DA]
            , [Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [DA]
            , [Dir D, DA]
            , [DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [DA]
            , [Dir D, DA]
            , [Dir U, DA]
            , [Dir L, Dir D, DA]
            , [Dir R, Dir U, DA]
            , [DA]
            , [Dir L, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, DA]
            , [Dir R, Dir U, DA]
            , [DA]
            , [DA]
            , [Dir D, DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            ]
        )

  it "finds an optimal sequence for the fourth example code" do
    cached3 example1d
      `shouldBe` length
        ( concat
            [ [Dir L, Dir D, Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [DA]
            , [Dir L, Dir D, DA]
            , [Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [DA]
            , [Dir D, DA]
            , [DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, DA]
            , [Dir R, Dir U, DA]
            , [Dir L, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, DA]
            , [Dir R, Dir U, DA]
            , [Dir L, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, DA]
            , [Dir R, Dir U, DA]
            , [DA]
            , [Dir D, DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            ]
        )

  it "finds an optimal sequence for the fifth example code" do
    cached3 example1e
      `shouldBe` length
        ( concat
            [ [Dir L, Dir D, Dir L, DA]
            , [Dir R, Dir R, Dir U, DA]
            , [Dir D, DA]
            , [Dir U, DA]
            , [Dir L, Dir D, DA]
            , [Dir L, DA]
            , [DA]
            , [Dir R, Dir R, Dir U, DA]
            , [DA]
            , [Dir D, DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            , [DA]
            , [Dir D, DA]
            , [Dir U, DA]
            , [Dir L, Dir D, DA]
            , [Dir R, Dir U, DA]
            , [DA]
            , [Dir L, DA]
            , [Dir R, DA]
            , [Dir L, Dir D, Dir L, DA]
            , [Dir R, DA]
            , [Dir R, Dir U, DA]
            , [DA]
            , [DA]
            , [Dir D, DA]
            , [Dir L, Dir U, DA]
            , [Dir R, DA]
            ]
        )

{-
it "solves part two with the first example" do
  part2 firstExample `shouldBe` 0
-}
