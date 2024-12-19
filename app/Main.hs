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
  solveDay parse1 part2 $ fromGregorian 2024 12 19

data Puzzle = Puzzle
  { towels :: [String]
  , designs :: [String]
  }
  deriving (Show, Eq)

parse1 :: Parser Puzzle
parse1 = Puzzle <$> towels <* newline <* newline <*> designs <* eof
  where
    towels = pattern `sepBy1` string ", "
    designs = pattern `endBy1` newline
    pattern = many1 lower

firstExample :: Puzzle
firstExample =
  Puzzle
    { towels = ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
    , designs = ["brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb"]
    }

part1 :: Puzzle -> Int
part1 Puzzle {towels, designs} = length . filter (> 0) $ map (combos towels) designs

combos :: [String] -> String -> Int
combos pats design = cache Map.! 0
  where
    cache :: Map Int Int
    cache = Map.fromList do
      (off, suf) <- zip [0 ..] $ List.tails design
      pure . (,) off . sum $
        if null suf
          then do
            pure 1
          else do
            (len, pat) <- lenpats
            Monad.guard $ pat `List.isPrefixOf` suf
            pure $ cache Map.! (off + len)

    lenpats = [(length pat, pat) | pat <- pats]

part2 :: Puzzle -> Int
part2 Puzzle {towels, designs} = sum $ combos towels <$> designs

-- $> main

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            r, wr, b, g, bwu, rb, gb, br

            brwrr
            bggr
            gbbr
            rrbgbr
            ubwu
            bwurrg
            brgr
            bbrgwb
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 6

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 16
