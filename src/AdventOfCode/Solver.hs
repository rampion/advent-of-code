{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoFieldSelectors #-}

module AdventOfCode.Solver where

import AdventOfCode.Spec
import Data.Kind
import Data.Text (Text)
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
    , spec :: forall m. SpecMonoid m => SpecWriter m
    } ->
    Solver

type Example :: Type -> Type -> Type -> Type
data Example input part1output part2output = Example
  { raw :: Text
  , parsed :: input
  , part1output :: part1output
  , part2output :: part2output
  }

check ::
  ( SpecMonoid m
  , Checkable input part1output part2output
  ) =>
  Parser input ->
  (input -> part1output) ->
  (input -> part2output) ->
  Example input part1output part2output ->
  SpecWriter m
check parser part1 part2 = tellSpec . runCheck parser part1 part2

runCheck ::
  Checkable input part1output part2output =>
  Parser input ->
  (input -> part1output) ->
  (input -> part2output) ->
  Example input part1output part2output ->
  Spec
runCheck parser part1 part2 Example {raw, parsed, part1output, part2output} = do
  it "can parse the example" do
    parse parser "example" raw `shouldBe` Right parsed

  it "reports the correct output for part 1 of the example" do
    part1 parsed `shouldBe` part1output

  it "reports the correct output for part 2 of the example" do
    part2 parsed `shouldBe` part2output
