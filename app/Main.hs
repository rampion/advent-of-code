{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad ((<=<))
-- import Control.Monad qualified as Monad
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
-- import Data.Function (on)
import Data.Functor ((<&>))
-- import Data.Functor.Const (Const (Const, getConst))
-- import Data.Functor.Identity (Identity (Identity, runIdentity))
-- import Data.Functor.Product (Product (Pair))
import Data.Either qualified as Either
-- import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
-- import Data.Map (Map)
-- import Data.Map qualified as Map
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
  solveDay parse1 part1 $ fromGregorian 2024 12 25

parse1 :: Parser Puzzle
parse1 = (schematic `sepBy` newline) <* eof where
  schematic :: Parser Schematic
  schematic = lock <|> key

  lock = Lock <$ allFilled <*> diagram one zero <* allEmpty
  key = Key <$ allEmpty <*> diagram zero one <* allFilled

  one :: Parser Word
  one = 1 <$ char '#'

  zero :: Parser Word
  zero = 0 <$ char '.'

  allFilled :: Parser String
  allFilled = string "#####\n"

  allEmpty :: Parser String
  allEmpty = string ".....\n"

  diagram :: Parser Word -> Parser Word -> Parser Diagram
  diagram start end = fmap Diagram . line <=< line <=< line <=< line <=< line $ replicate 5 (level start end)

  level :: Parser Word -> Parser Word -> Parser (Parser (Parser (Parser (Parser Word))))
  level start end
    = (\(a,b) -> a 0 <|> b 0)
    . cell start end
    . cell start end
    . cell start end
    $ cell start end (\n -> (n+) <$> start, \n -> (n+) <$> end)

  line :: [Parser a] -> Parser [a]
  line cells = sequenceA cells <* newline
  
  cell :: Num a => Parser a -> Parser a -> (a -> Parser b, a -> Parser b) -> (a -> Parser (Parser b), a -> Parser (Parser b))
  cell start end (cont, quit) = 
    ( \(!n) -> start <&> \m -> cont (n + m) <|> quit (n + m)
    , \(!n) -> end <&> \m -> quit (n + m)
    )
  
newtype Diagram = Diagram { getDiagram :: [Word] }
  deriving (Show, Eq)

opposite :: Diagram -> Diagram
opposite = Diagram . map (5 -) . getDiagram

compareDiagram :: Diagram -> Diagram -> Maybe Ordering 
compareDiagram = \(Diagram lhs) (Diagram rhs) -> loop EQ lhs rhs where
  loop = \cases
    re [] [] -> Just re
    EQ (a:as) (b:bs) -> loop (compare a b) as bs
    LT (a:as) (b:bs) | a <= b -> loop LT as bs
    GT (a:as) (b:bs) | a >= b -> loop GT as bs
    _ _ _ -> Nothing

data Schematic = Lock Diagram | Key Diagram
  deriving (Show, Eq)

type Puzzle = [Schematic]

firstExample :: Puzzle
firstExample =
  [ Lock (Diagram [ 0, 5, 3, 4, 3])
  , Lock (Diagram [ 1, 2, 0, 5, 3])
  , Key (Diagram [ 5, 0, 2, 1, 3])
  , Key (Diagram [ 4, 3, 4, 0, 2])
  , Key (Diagram [ 3, 0, 2, 0, 1])
  ]

part1 :: Puzzle -> Int
part1 = uncurry countPairs . Either.partitionEithers . map \case
  Lock d -> Left d
  Key d -> Right d

countPairs :: [Diagram] -> [Diagram] -> Int
countPairs locks keys = length [ () | Diagram as <- locks, Diagram bs <- keys, all (<= 5) $ zipWith (+) as bs]

part2 :: Puzzle -> ()
part2 _ = ()

-- $> main

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            #####
            .####
            .####
            .####
            .#.#.
            .#...
            .....
            
            #####
            ##.##
            .#.##
            ...##
            ...#.
            ...#.
            .....
            
            .....
            #....
            #....
            #...#
            #.#.#
            #.###
            #####
            
            .....
            .....
            #.#..
            ###..
            ###.#
            ###.#
            #####
            
            .....
            .....
            .....
            #....
            #.#..
            #.#.#
            #####
        |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 3

  it "solves part two with the second example" do
    part2 firstExample `shouldBe` ()
