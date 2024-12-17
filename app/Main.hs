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

import Control.Monad ((>=>))
import Control.Monad qualified as Monad
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as MonadState
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Maybe qualified as MaybeT
import Control.Monad.Trans.State.Strict (StateT (runStateT))
import Control.Monad.Trans.Writer.Strict (Writer, runWriter)
import Control.Monad.Writer.Strict qualified as MonadWriter
-- import Control.Concurrent (threadDelay)
import Data.Bits (xor)
import Data.Foldable qualified as Foldable
-- import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Functor.Product (Product (Pair))
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
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import NeatInterpolation (text)
import SolveDay (solveDay)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec hiding (State)
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part1 $ fromGregorian 2024 12 17

data Puzzle = Puzzle
  { registers :: Registers
  , program :: Program
  }
  deriving (Show, Eq)

data Registers = Registers {a :: Int, b :: Int, c :: Int}
  deriving (Show, Eq)

type Program = [Opcode]

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
part1 Puzzle {program, registers} = snd do
  programLoop (Vector.fromList program) `runProcess` Computer {registers, instructionPointer = 0}

data Computer = Computer
  { registers :: Registers
  , instructionPointer :: Int
  }
  deriving (Show, Eq)

runProcess :: Process a -> Computer -> ((Maybe a, Computer), [Int])
runProcess = flip \initial -> runWriter . (`runStateT` initial) . runMaybeT

type Process = MaybeT (StateT Computer (Writer [Int]))

programLoop :: Vector Opcode -> Process ()
programLoop program = do
  opcode <- lookupOpcode program =<< postincrementInstructionPointer
  operand <- lookupOpcode program =<< postincrementInstructionPointer
  dispatch opcode (fromEnum operand)
  programLoop program

lookupOpcode :: Vector Opcode -> Int -> Process Opcode
lookupOpcode program = MaybeT.hoistMaybe . (program Vector.!?)

blankComputer :: Computer
blankComputer = Computer {instructionPointer = 0, registers = Registers 0 0 0}

postincrementInstructionPointer :: Process Int
postincrementInstructionPointer =
  MonadState.state (fromPair . _instructionPointer \i -> Pair (Const i) (Identity (succ i)))
  where
    fromPair (Pair (Const a) (Identity b)) = (a, b)

dispatch :: Opcode -> Int -> Process ()
dispatch = \case
  ADV -> adv -- divide register a / 2 ^ combo
  BXL -> bxl -- xor register b with literal
  BST -> bst -- store combo mod 8 in register b
  JNZ -> jnz -- jump to literal if register a nonzero
  BXC -> bxc -- xor register b with register c and store in b
  OUT -> out -- output combo mod 8
  BDV -> bdv -- divide register / 2 ^ combo and store in b
  CDV -> cdv -- divide register / 2 ^ combo and store in c

adv, bxl, bst, jnz, bxc, out, bdv, cdv :: Int -> Process ()
adv = rdv _a
bdv = rdv _b
cdv = rdv _c

rdv :: Setter' Computer Int -> Int -> Process ()
rdv reg =
  combo >=> \denom -> do
    numer <- use _a
    reg .= (numer `div` 2 ^ denom)

bxl n = _b %= (`xor` n)

bxc _ = bxl =<< use _c

bst = combo >=> \n -> _b .= (n `mod` 8)

jnz n = do
  a <- use _a
  Monad.when (a /= 0) do
    _instructionPointer .= n

out = combo >=> \n -> MonadWriter.tell [n `mod` 8]

combo :: Int -> Process Int
combo = \case
  4 -> use _a
  5 -> use _b
  6 -> use _c
  n -> pure n

_a, _b, _c, _instructionPointer :: Lens' Computer Int
_a f computer = f computer.registers.a <&> \a -> (computer :: Computer) {registers = computer.registers {a}}
_b f computer = f computer.registers.b <&> \b -> (computer :: Computer) {registers = computer.registers {b}}
_c f computer = f computer.registers.c <&> \c -> (computer :: Computer) {registers = computer.registers {c}}
_instructionPointer f computer =
  f computer.instructionPointer <&> \instructionPointer ->
    computer {instructionPointer}

type Optic' f s a = (a -> f a) -> (s -> f s)

type Lens' s a = forall f. (Functor f) => Optic' f s a

type Setter' s a = Optic' Identity s a

type Getter' s a = Optic' (Const a) s a

(%=) :: (MonadState s m) => Setter' s a -> (a -> a) -> m ()
l %= f = MonadState.modify (runIdentity . l (Identity . f))

(.=) :: (MonadState s m) => Setter' s a -> a -> m ()
l .= a = l %= const a

use :: (MonadState s m) => Getter' s a -> m a
use l = MonadState.gets (getConst . l Const)

part2 :: Puzzle -> Int
part2 _ = 0

-- $> runTests

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

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 0
