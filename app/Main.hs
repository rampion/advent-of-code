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
import Data.Bits (xor, (.&.), (.|.))
-- import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
-- import Data.Functor.Const (Const (Const, getConst))
-- import Data.Functor.Identity (Identity (Identity, runIdentity))
-- import Data.Functor.Product (Product (Pair))
import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
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
  solveDay parse1 part2 $ fromGregorian 2024 12 24

data Puzzle = Puzzle
  { initialWireValues :: Map Wire Value
  , gateConnections :: Map Wire Gate
  }
  deriving (Show, Eq)

type Wire = String

type Value = Word

data Gate = AND Wire Wire | OR Wire Wire | XOR Wire Wire
  deriving (Show, Eq)

parse1 :: Parser Puzzle
parse1 = Puzzle <$> initialWireValues <* newline <*> gateConnections <* eof
  where
    initialWireValues = Map.fromList <$> (initialWireValue `endBy` newline)
    initialWireValue = (,) <$> wire <* string ": " <*> bit
    wire = Monad.replicateM 3 wireChar
    wireChar = lower <|> digit
    bit = (0 <$ char '0') <|> (1 <$ char '1')
    gateConnections = Map.fromList <$> (gateConnection `endBy` newline)
    gateConnection = flip (,) <$> gate <* string " -> " <*> wire
    gate = (&) <$> wire <* char ' ' <*> op <* char ' ' <*> wire
    op = (AND <$ string "AND") <|> (OR <$ string "OR") <|> (XOR <$ string "XOR")

firstExample :: Puzzle
firstExample =
  Puzzle
    { initialWireValues =
        Map.fromList
          [ ("x00", 1)
          , ("x01", 0)
          , ("x02", 1)
          , ("x03", 1)
          , ("x04", 0)
          , ("y00", 1)
          , ("y01", 1)
          , ("y02", 1)
          , ("y03", 1)
          , ("y04", 1)
          ]
    , gateConnections =
        Map.fromList
          [ ("mjb", "ntg" `XOR` "fgs")
          , ("tnw", "y02" `OR` "x01")
          , ("z05", "kwq" `OR` "kpj")
          , ("fst", "x00" `OR` "x03")
          , ("z01", "tgd" `XOR` "rvg")
          , ("bfw", "vdt" `OR` "tnw")
          , ("z10", "bfw" `AND` "frj")
          , ("bqk", "ffh" `OR` "nrd")
          , ("djm", "y00" `AND` "y03")
          , ("psh", "y03" `OR` "y00")
          , ("z08", "bqk" `OR` "frj")
          , ("frj", "tnw" `OR` "fst")
          , ("z11", "gnj" `AND` "tgd")
          , ("z00", "bfw" `XOR` "mjb")
          , ("vdt", "x03" `OR` "x00")
          , ("z02", "gnj" `AND` "wpb")
          , ("kjc", "x04" `AND` "y00")
          , ("qhw", "djm" `OR` "pbm")
          , ("hwm", "nrd" `AND` "vdt")
          , ("rvg", "kjc" `AND` "fst")
          , ("fgs", "y04" `OR` "y02")
          , ("pbm", "y01" `AND` "x02")
          , ("kwq", "ntg" `OR` "kjc")
          , ("tgd", "psh" `XOR` "fgs")
          , ("z09", "qhw" `XOR` "tgd")
          , ("kpj", "pbm" `OR` "djm")
          , ("ffh", "x03" `XOR` "y03")
          , ("ntg", "x00" `XOR` "y04")
          , ("z06", "bfw" `OR` "bqk")
          , ("wpb", "nrd" `XOR` "fgs")
          , ("z04", "frj" `XOR` "qhw")
          , ("z07", "bqk" `OR` "frj")
          , ("nrd", "y03" `OR` "x01")
          , ("z03", "hwm" `AND` "bqk")
          , ("z12", "tgd" `XOR` "rvg")
          , ("gnj", "tnw" `OR` "pbm")
          ]
    }

part1 :: Puzzle -> Word
part1 = zNumber . finalWireValues

part2 :: Puzzle -> Maybe String
part2 = fmap format . correct 4

format :: [(Wire, Wire)] -> String
format = List.intercalate "," . List.sort . concatMap \(a, b) -> [a, b]

correct :: Word -> Puzzle -> Maybe [(Wire, Wire)]
correct n Puzzle {initialWireValues, gateConnections} = Maybe.listToMaybe correctSwaps
  where
    x = getNumber 'x' initialWireValues
    y = getNumber 'y' initialWireValues
    z = x + y
    correctSwaps =
      [ swaps
      | swaps <- pairs n (Map.keys gateConnections)
      , let gateConnections' = swap swaps gateConnections
      , acyclic gateConnections'
      , part1 Puzzle {initialWireValues, gateConnections = gateConnections'} == z
      ]

acyclic :: Map Wire Gate -> Bool
acyclic _ = False

swap :: [(Wire, Wire)] -> Map Wire Gate -> Map Wire Gate
swap ps gs = (`Map.union` gs) $ Map.fromList do
  (a, b) <- ps
  [(a, gs Map.! b), (b, gs Map.! a)]

pairs :: Word -> [a] -> [[(a, a)]]
pairs 0 _ = pure []
pairs n as = do
  (pre, a : bs) <- List.inits as `zip` List.tails as
  (inf, b : suf) <- List.inits bs `zip` List.tails bs
  ((a, b) :) <$> pairs (n - 1) (pre <> inf <> suf)

finalWireValues :: Puzzle -> Map Wire Value
finalWireValues Puzzle {initialWireValues, gateConnections} = m
  where
    m =
      initialWireValues `Map.union` Map.fromList do
        Map.toList gateConnections <&> fmap \case
          XOR a b -> (m Map.! a) `xor` (m Map.! b)
          AND a b -> (m Map.! a) .&. (m Map.! b)
          OR a b -> (m Map.! a) .|. (m Map.! b)

zNumber :: Map Wire Value -> Word
zNumber = getNumber 'z'

getNumber :: Char -> Map Wire Value -> Word
getNumber c = foldl' (\n b -> 2 * n + b) 0 . map snd . Map.toDescList . Map.filterWithKey \k _ -> [c] `List.isPrefixOf` k

-- $> main

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            x00: 1
            x01: 0
            x02: 1
            x03: 1
            x04: 0
            y00: 1
            y01: 1
            y02: 1
            y03: 1
            y04: 1

            ntg XOR fgs -> mjb
            y02 OR x01 -> tnw
            kwq OR kpj -> z05
            x00 OR x03 -> fst
            tgd XOR rvg -> z01
            vdt OR tnw -> bfw
            bfw AND frj -> z10
            ffh OR nrd -> bqk
            y00 AND y03 -> djm
            y03 OR y00 -> psh
            bqk OR frj -> z08
            tnw OR fst -> frj
            gnj AND tgd -> z11
            bfw XOR mjb -> z00
            x03 OR x00 -> vdt
            gnj AND wpb -> z02
            x04 AND y00 -> kjc
            djm OR pbm -> qhw
            nrd AND vdt -> hwm
            kjc AND fst -> rvg
            y04 OR y02 -> fgs
            y01 AND x02 -> pbm
            ntg OR kjc -> kwq
            psh XOR fgs -> tgd
            qhw XOR tgd -> z09
            pbm OR djm -> kpj
            x03 XOR y03 -> ffh
            x00 XOR y04 -> ntg
            bfw OR bqk -> z06
            nrd XOR fgs -> wpb
            frj XOR qhw -> z04
            bqk OR frj -> z07
            y03 OR x01 -> nrd
            hwm AND bqk -> z03
            tgd XOR rvg -> z12
            tnw OR pbm -> gnj
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 2024
