module Main where

import AdventOfCode
import Control.Monad.IO.Class (liftIO)
import Prelude
import Test.Hspec
import Text.Parsec.String (parseFromFile)

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

  describe "day2" do
    let exampleInput =
          [ (Rock, Y)
          , (Paper, X)
          , (Scissors, Z)
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day2parser "test/input/day2"
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day2part1 exampleInput `shouldBe` 15

    it "reports the correct output for part 2 of the example" do
      day2part2 exampleInput `shouldBe` 12

  describe "day3" do
    let exampleInput =
          [ zip [22, 36, 18, 23, 16, 49, 20, 23, 36, 7, 49, 18] [8, 3, 19, 32, 39, 39, 6, 32, 32, 8, 32, 16]
          , zip [10, 17, 34, 44, 40, 17, 44, 10, 17, 26, 10, 33, 30, 38, 33, 38] [18, 19, 32, 39, 6, 32, 52, 45, 18, 38, 18, 32, 52, 19, 45, 38]
          , zip [42, 13, 13, 4, 26, 17, 42, 18, 48] [22, 42, 23, 23, 46, 49, 28, 23, 7]
          , zip [23, 39, 17, 22, 38, 39, 52, 34, 8, 34, 39, 22, 23, 38, 34] [10, 2, 22, 3, 10, 14, 14, 45, 28, 14, 22, 46, 43, 32, 14]
          , zip [20, 20, 7, 36, 20, 44, 33, 36] [43, 3, 20, 46, 52, 20, 52, 46]
          , zip [29, 18, 52, 19, 36, 19, 42, 42, 52, 19, 33, 26] [23, 23, 19, 38, 23, 38, 13, 16, 23, 39, 30, 23]
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day3parser "test/input/day3"
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day3part1 exampleInput `shouldBe` 157

    it "reports the correct output for part 2 of the example" do
      day3part2 exampleInput `shouldBe` 70

  describe "day4" do
    let exampleInput = 
          [ ( (2,4),(6,8) )
          , ( (2,3),(4,5) )
          , ( (5,7),(7,9) )
          , ( (2,8),(3,7) )
          , ( (6,6),(4,6) )
          , ( (2,6),(4,8) )
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day4parser "test/input/day4"
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day4part1 exampleInput `shouldBe` 2

    it "reports the correct output for part 2 of the example" do
      day4part2 exampleInput `shouldBe` 4
