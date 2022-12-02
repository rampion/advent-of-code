module Main where

import Prelude
import Test.Hspec
import AdventOfCode
import Text.Parsec.String (parseFromFile)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspec do
  describe "day1" do
    let exampleInput =
          [ [1000, 2000, 3000]
          , [4000]
          , [5000, 6000]
          , [7000, 8000, 9000]
          , [10000]
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day1parser "test/input/day1"
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day1part1 exampleInput `shouldBe` 24000

    it "reports the correct output for part 2 of the example" do
      day1part2 exampleInput `shouldBe` 45000
