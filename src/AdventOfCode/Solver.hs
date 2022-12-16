{-# LANGUAGE NoFieldSelectors #-}
module AdventOfCode.Solver where

import AdventOfCode.Spec
import Prelude

type Parser = Parsec String ()

data Solver where
  Solver :: 
    forall input part1output part2output.
    ( Show part1output, Eq part1output
    , Show part2output, Eq part2output
    ) => 
    { parser :: Parser input
    , part1 :: input -> part1output
    , part2 :: input -> part2output
    , spec :: SpecWriter
    } -> Solver

data Example input part1output part2output = Example
  { raw :: String
  , parsed :: input
  , part1output :: part1output
  , part2output :: part2output
  }

check :: Solver -> Example -> SpecWriter
check solver example = tellSpec do
  runCheck solver example

runCheck :: Solver -> Example -> Spec
runCheck Solver{parser,part1,part2}
      Example{raw,parsed,input,part1output,part2output} = do
  it "can parse the example" do
    actual <- liftIO do parse parser "example" raw
    actual `shouldBe` Right parsed

  it "reports the correct output for part 1 of the example" do
    part1 input `shouldBe` part1output

  it "reports the correct output for part 2 of the example" do
    part2 input `shouldBe` part2output
