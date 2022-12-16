{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoFieldSelectors #-}

module AdventOfCode.Solver where

import Data.Kind
import Data.Text (Text)
import Test.Hspec hiding (Example)
import Text.Parsec
import Prelude

type Parser :: Type -> Type
type Parser = Parsec Text ()

type Checkable :: Type -> Type -> Type -> Constraint
type Checkable input part1output part2output =
  ( Show input
  , Eq input
  , Show part1output
  , Eq part1output
  , Show part2output
  , Eq part2output
  )

type Solver :: Type
data Solver where
  Solver ::
    forall input part1output part2output.
    Checkable input part1output part2output =>
    { parser :: Parser input
    , part1 :: input -> part1output
    , part2 :: input -> part2output
    , spec :: Spec
    } ->
    Solver

type Example :: Type -> Type -> Type -> Type
data Example input part1output part2output = Example
  { raw :: Text
  , parsed :: input
  , part1output :: part1output
  , part2output :: part2output
  }

xcheck ::
  Checkable input part1output part2output =>
  Parser input ->
  (input -> part1output) ->
  (input -> part2output) ->
  Example input part1output part2output ->
  Spec
xcheck _parser _part1 _part2 _example = pure ()

fcheck ::
  Checkable input part1output part2output =>
  Parser input ->
  (input -> part1output) ->
  (input -> part2output) ->
  Example input part1output part2output ->
  Spec
fcheck parser part1 part2 = focus . check parser part1 part2

check ::
  Checkable input part1output part2output =>
  Parser input ->
  (input -> part1output) ->
  (input -> part2output) ->
  Example input part1output part2output ->
  Spec
check parser part1 part2 Example {raw, parsed, part1output, part2output} = do
  it "can parse the example" do
    parse parser "example" raw `shouldBe` Right parsed

  it "reports the correct output for part 1 of the example" do
    part1 parsed `shouldBe` part1output

  it "reports the correct output for part 2 of the example" do
    part2 parsed `shouldBe` part2output
