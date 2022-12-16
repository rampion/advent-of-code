{-# LANGUAGE OverloadedLists #-}
module Main where

import AdventOfCode
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (WriterT (..), tell)
import Data.IntMap qualified as IntMap
import Data.Monoid (Dual (..), Endo (..))
import System.Directory (doesFileExist)
import Test.Hspec
import Text.Parsec.String (parseFromFile)
import Text.Parsec (runParser)
import Prelude
import Data.Maybe (catMaybes)
import Data.Vector qualified as Vector
import Data.Map qualified as Map

main :: IO ()
main = runSpecs @LastSpec do
  describeIfAvailable "day1" \day1Input -> do
    let exampleInput =
          [ [1000, 2000, 3000]
          , [4000]
          , [5000, 6000]
          , [7000, 8000, 9000]
          , [10000]
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day1parser day1Input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day1part1 exampleInput `shouldBe` 24000

    it "reports the correct output for part 2 of the example" do
      day1part2 exampleInput `shouldBe` 45000

  describeIfAvailable "day2" \day2Input -> do
    let exampleInput =
          [ (Rock, Y)
          , (Paper, X)
          , (Scissors, Z)
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day2parser day2Input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day2part1 exampleInput `shouldBe` 15

    it "reports the correct output for part 2 of the example" do
      day2part2 exampleInput `shouldBe` 12

  describeIfAvailable "day3" \day3Input -> do
    let exampleInput =
          [ zip [22, 36, 18, 23, 16, 49, 20, 23, 36, 7, 49, 18] [8, 3, 19, 32, 39, 39, 6, 32, 32, 8, 32, 16]
          , zip [10, 17, 34, 44, 40, 17, 44, 10, 17, 26, 10, 33, 30, 38, 33, 38] [18, 19, 32, 39, 6, 32, 52, 45, 18, 38, 18, 32, 52, 19, 45, 38]
          , zip [42, 13, 13, 4, 26, 17, 42, 18, 48] [22, 42, 23, 23, 46, 49, 28, 23, 7]
          , zip [23, 39, 17, 22, 38, 39, 52, 34, 8, 34, 39, 22, 23, 38, 34] [10, 2, 22, 3, 10, 14, 14, 45, 28, 14, 22, 46, 43, 32, 14]
          , zip [20, 20, 7, 36, 20, 44, 33, 36] [43, 3, 20, 46, 52, 20, 52, 46]
          , zip [29, 18, 52, 19, 36, 19, 42, 42, 52, 19, 33, 26] [23, 23, 19, 38, 23, 38, 13, 16, 23, 39, 30, 23]
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day3parser day3Input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day3part1 exampleInput `shouldBe` 157

    it "reports the correct output for part 2 of the example" do
      day3part2 exampleInput `shouldBe` 70

  describeIfAvailable "day4" \day4Input -> do
    let exampleInput =
          [ ((2, 4), (6, 8))
          , ((2, 3), (4, 5))
          , ((5, 7), (7, 9))
          , ((2, 8), (3, 7))
          , ((6, 6), (4, 6))
          , ((2, 6), (4, 8))
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day4parser day4Input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day4part1 exampleInput `shouldBe` 2

    it "reports the correct output for part 2 of the example" do
      day4part2 exampleInput `shouldBe` 4

  describeIfAvailable "day5" \day5input -> do
    let exampleInput =
          Day5Input
            { crateStacks = IntMap.fromList [(1, "NZ"), (2, "DCM"), (3, "P")]
            , procedure =
                [ Step {move = 1, from = 2, to = 1}
                , Step {move = 3, from = 1, to = 3}
                , Step {move = 2, from = 2, to = 1}
                , Step {move = 1, from = 1, to = 2}
                ]
            }

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day5parser day5input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day5part1 exampleInput `shouldBe` "CMZ"

    it "reports the correct output for part 2 of the example" do
      day5part2 exampleInput `shouldBe` "MCD"

  describeTell "day6" do
    let examples = zip [0 :: Int ..]
          [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19)
          , ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23)
          , ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23)
          , ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29)
          , ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
          ]

    forM_ examples \(i, (exampleInput, _, _)) -> do
      let name = "example input " ++ show i
      
      it ("can parse " ++ name) do
        runParser day6parser () name exampleInput `shouldBe` Right exampleInput

    describe "part 1" do
      forM_ examples \(i, (exampleInput, exampleOutput1, _)) -> do
        let name = "example input " ++ show i
        it ("reports the correct output for part 1 with " ++ name) do
          day6part1 exampleInput `shouldBe` exampleOutput1

    describe "part 2" do
      forM_ examples \(i, (exampleInput, _, exampleOutput2)) -> do
        let name = "example input " ++ show i
        it ("reports the correct output for part 2 with " ++ name) do
          day6part2 exampleInput `shouldBe` exampleOutput2

  describeIfAvailable "day7" \day7input -> do
    let exampleInput = 
          [ CD Outermost
          , LS 
            [ Dir "a"
            , File 14848514 "b.txt"
            , File 8504156 "c.dat"
            , Dir "d"
            ]
          , CD (In "a")
          , LS
            [ Dir "e"
            , File 29116 "f"
            , File 2557 "g"
            , File 62596 "h.lst"
            ]
          , CD (In "e")
          , LS
            [ File 584 "i"
            ]
          , CD Out
          , CD Out
          , CD (In "d")
          , LS
            [ File 4060174 "j"
            , File 8033020 "d.log"
            , File 5626152 "d.ext"
            , File 7214296 "k"
            ]
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day7parser day7input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day7part1 exampleInput `shouldBe` 95437

    it "reports the correct output for part 2 of the example" do
      day7part2 exampleInput `shouldBe` 24933642

  describeIfAvailable "day8" \day8input -> do
    let exampleInput = toGrid rawInput
        rawInput =
          [ [ 3, 0, 3, 7, 3 ]
          , [ 2, 5, 5, 1, 2 ]
          , [ 6, 5, 3, 3, 2 ]
          , [ 3, 3, 5, 4, 9 ]
          , [ 3, 5, 3, 9, 0 ]
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile (fromGrid <$> day8parser) day8input
      parsedInput `shouldBe` Right rawInput

    it "reports the correct output for part 1 of the example" do
      day8part1 exampleInput `shouldBe` 21

    describe "visible" do
      it "reports the correct total visibilities" do
        fromGrid (visible exampleInput) `shouldBe`
            [ [  True,  True,  True,  True,  True ]
            , [  True,  True,  True, False,  True ]
            , [  True,  True, False,  True,  True ]
            , [  True, False,  True, False,  True ]
            , [  True,  True,  True,  True,  True ]
            ]

    describe "visibilities" do
      it "reports the correct partial visibilities" do
        let t = True
            f = False
        fromGrid (visibilities exampleInput) `shouldBe`
            [ [ [t,f,t,f], [f,f,t,f], [f,f,t,f], [t,t,t,f], [f,t,t,f] ]
            , [ [t,f,f,f], [t,f,t,f], [f,t,t,f], [f,f,f,f], [f,t,f,f] ]
            , [ [t,t,t,t], [f,t,f,f], [f,f,f,f], [f,t,f,f], [f,t,f,f] ]
            , [ [t,f,f,f], [f,f,f,f], [t,f,f,t], [f,f,f,f], [t,t,t,t] ]
            , [ [t,f,f,t], [t,f,f,t], [f,f,f,t], [t,t,t,t], [f,t,f,t] ]
            ]

    describe "leftToRight" do
      it "reports the correct left-to-right visibilities" do
        let t = True
            f = False
        fromGrid (leftToRight exampleInput) `shouldBe`
            [ [ t, f, f, t, f ]
            , [ t, t, f, f, f ]
            , [ t, f, f, f, f ]
            , [ t, f, t, f, t ]
            , [ t, t, f, t, f ]
            ]

    describe "isLeftVisible" do
      it "reports the correct left-to-right visibilities" do
        isLeftVisible [ 3, 0, 3, 7, 3] `shouldBe` [True, False, False, True, False]

      it "uses the correct scan" do
        scanl max (-1) [3, 0, 3, 7, 3] `shouldBe` [-1,3,3,3,7,7 :: Int]

    describe "scenicScores" do
      it "assigns the correct scores to each location" do
        fromGrid (scenicScores exampleInput) `shouldBe`
            [ [ 0, 0, 0, 0, 0 ]
            , [ 0, 1, 4, 1, 0 ]
            , [ 0, 6, 1, 2, 0 ]
            , [ 0, 1, 8, 3, 0 ]
            , [ 0, 0, 0, 0, 0 ]
            ]

    describe "leftScores" do
      it "assigns the correct scores to each location" do
        leftScore [3,0,3,7,3] `shouldBe` [0,1,2,3,1]
        

    it "reports the correct output for part 2 of the example" do
      day8part2 exampleInput `shouldBe` 8

  describeIfAvailable "day9" \day9input -> do
    let exampleInput = 
          [ (R, 4)
          , (U, 4)
          , (L, 3)
          , (D, 1)
          , (R, 4)
          , (D, 1)
          , (L, 5)
          , (R, 2)
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day9parser day9input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day9part1 exampleInput `shouldBe` 13

    it "reports the correct output for part 2 of the example" do
      day9part2 exampleInput `shouldBe` 1

    let exampleInput2 = 
          [ (R, 5)
          , (U, 8)
          , (L, 8)
          , (D, 3)
          , (R, 17)
          , (D, 10)
          , (L, 25)
          , (U, 20)
          ]

    it "reports the correct output for part 2 of the larger example" do
      day9part2 exampleInput2 `shouldBe` 36

  describeIfAvailable "day10" \day10input -> do
    let examplePairs =
          [ (Nothing,               RegisterState {cycle = 1, value = 1})
          , (Nothing,               RegisterState {cycle = 2, value = 1})
          , (Just (Addx 15),        RegisterState {cycle = 3, value = 16})
          , (Nothing,               RegisterState {cycle = 4, value = 16})
          , (Just (Addx (-11)),     RegisterState {cycle = 5, value = 5})
          , (Nothing,               RegisterState {cycle = 6, value = 5})
          , (Just (Addx 6),         RegisterState {cycle = 7, value = 11})
          , (Nothing,               RegisterState {cycle = 8, value = 11})
          , (Just (Addx (-3)),      RegisterState {cycle = 9, value = 8})
          , (Nothing,               RegisterState {cycle = 10, value = 8})
          , (Just (Addx 5),         RegisterState {cycle = 11, value = 13})
          , (Nothing,               RegisterState {cycle = 12, value = 13})
          , (Just (Addx (-1)),      RegisterState {cycle = 13, value = 12})
          , (Nothing,               RegisterState {cycle = 14, value = 12})
          , (Just (Addx (-8)),      RegisterState {cycle = 15, value = 4})
          , (Nothing,               RegisterState {cycle = 16, value = 4})
          , (Just (Addx 13),        RegisterState {cycle = 17, value = 17})
          , (Nothing,               RegisterState {cycle = 18, value = 17})
          , (Just (Addx 4),         RegisterState {cycle = 19, value = 21})
          , (Just Noop,             RegisterState {cycle = 20, value = 21})
          , (Nothing,               RegisterState {cycle = 21, value = 21})
          , (Just (Addx (-1)),      RegisterState {cycle = 22, value = 20})
          , (Nothing,               RegisterState {cycle = 23, value = 20})
          , (Just (Addx 5),         RegisterState {cycle = 24, value = 25})
          , (Nothing,               RegisterState {cycle = 25, value = 25})
          , (Just (Addx (-1)),      RegisterState {cycle = 26, value = 24})
          , (Nothing,               RegisterState {cycle = 27, value = 24})
          , (Just (Addx 5),         RegisterState {cycle = 28, value = 29})
          , (Nothing,               RegisterState {cycle = 29, value = 29})
          , (Just (Addx (-1)),      RegisterState {cycle = 30, value = 28})
          , (Nothing,               RegisterState {cycle = 31, value = 28})
          , (Just (Addx 5),         RegisterState {cycle = 32, value = 33})
          , (Nothing,               RegisterState {cycle = 33, value = 33})
          , (Just (Addx (-1)),      RegisterState {cycle = 34, value = 32})
          , (Nothing,               RegisterState {cycle = 35, value = 32})
          , (Just (Addx 5),         RegisterState {cycle = 36, value = 37})
          , (Nothing,               RegisterState {cycle = 37, value = 37})
          , (Just (Addx (-1)),      RegisterState {cycle = 38, value = 36})
          , (Nothing,               RegisterState {cycle = 39, value = 36})
          , (Just (Addx (-35)),     RegisterState {cycle = 40, value = 1})
          , (Nothing,               RegisterState {cycle = 41, value = 1})
          , (Just (Addx 1),         RegisterState {cycle = 42, value = 2})
          , (Nothing,               RegisterState {cycle = 43, value = 2})
          , (Just (Addx 24),        RegisterState {cycle = 44, value = 26})
          , (Nothing,               RegisterState {cycle = 45, value = 26})
          , (Just (Addx (-19)),     RegisterState {cycle = 46, value = 7})
          , (Nothing,               RegisterState {cycle = 47, value = 7})
          , (Just (Addx 1),         RegisterState {cycle = 48, value = 8})
          , (Nothing,               RegisterState {cycle = 49, value = 8})
          , (Just (Addx 16),        RegisterState {cycle = 50, value = 24})
          , (Nothing,               RegisterState {cycle = 51, value = 24})
          , (Just (Addx (-11)),     RegisterState {cycle = 52, value = 13})
          , (Just Noop,             RegisterState {cycle = 53, value = 13})
          , (Just Noop,             RegisterState {cycle = 54, value = 13})
          , (Nothing,               RegisterState {cycle = 55, value = 13})
          , (Just (Addx 21),        RegisterState {cycle = 56, value = 34})
          , (Nothing,               RegisterState {cycle = 57, value = 34})
          , (Just (Addx (-15)),     RegisterState {cycle = 58, value = 19})
          , (Just Noop,             RegisterState {cycle = 59, value = 19})
          , (Just Noop,             RegisterState {cycle = 60, value = 19})
          , (Nothing,               RegisterState {cycle = 61, value = 19})
          , (Just (Addx (-3)),      RegisterState {cycle = 62, value = 16})
          , (Nothing,               RegisterState {cycle = 63, value = 16})
          , (Just (Addx 9),         RegisterState {cycle = 64, value = 25})
          , (Nothing,               RegisterState {cycle = 65, value = 25})
          , (Just (Addx 1),         RegisterState {cycle = 66, value = 26})
          , (Nothing,               RegisterState {cycle = 67, value = 26})
          , (Just (Addx (-3)),      RegisterState {cycle = 68, value = 23})
          , (Nothing,               RegisterState {cycle = 69, value = 23})
          , (Just (Addx 8),         RegisterState {cycle = 70, value = 31})
          , (Nothing,               RegisterState {cycle = 71, value = 31})
          , (Just (Addx 1),         RegisterState {cycle = 72, value = 32})
          , (Nothing,               RegisterState {cycle = 73, value = 32})
          , (Just (Addx 5),         RegisterState {cycle = 74, value = 37})
          , (Just Noop,             RegisterState {cycle = 75, value = 37})
          , (Just Noop,             RegisterState {cycle = 76, value = 37})
          , (Just Noop,             RegisterState {cycle = 77, value = 37})
          , (Just Noop,             RegisterState {cycle = 78, value = 37})
          , (Just Noop,             RegisterState {cycle = 79, value = 37})
          , (Nothing,               RegisterState {cycle = 80, value = 37})
          , (Just (Addx (-36)),     RegisterState {cycle = 81, value = 1})
          , (Just Noop,             RegisterState {cycle = 82, value = 1})
          , (Nothing,               RegisterState {cycle = 83, value = 1})
          , (Just (Addx 1),         RegisterState {cycle = 84, value = 2})
          , (Nothing,               RegisterState {cycle = 85, value = 2})
          , (Just (Addx 7),         RegisterState {cycle = 86, value = 9})
          , (Just Noop,             RegisterState {cycle = 87, value = 9})
          , (Just Noop,             RegisterState {cycle = 88, value = 9})
          , (Just Noop,             RegisterState {cycle = 89, value = 9})
          , (Nothing,               RegisterState {cycle = 90, value = 9})
          , (Just (Addx 2),         RegisterState {cycle = 91, value = 11})
          , (Nothing,               RegisterState {cycle = 92, value = 11})
          , (Just (Addx 6),         RegisterState {cycle = 93, value = 17})
          , (Just Noop,             RegisterState {cycle = 94, value = 17})
          , (Just Noop,             RegisterState {cycle = 95, value = 17})
          , (Just Noop,             RegisterState {cycle = 96, value = 17})
          , (Just Noop,             RegisterState {cycle = 97, value = 17})
          , (Just Noop,             RegisterState {cycle = 98, value = 17})
          , (Nothing,               RegisterState {cycle = 99, value = 17})
          , (Just (Addx 1),         RegisterState {cycle = 100, value = 18})
          , (Just Noop,             RegisterState {cycle = 101, value = 18})
          , (Just Noop,             RegisterState {cycle = 102, value = 18})
          , (Nothing,               RegisterState {cycle = 103, value = 18})
          , (Just (Addx 7),         RegisterState {cycle = 104, value = 25})
          , (Nothing,               RegisterState {cycle = 105, value = 25})
          , (Just (Addx 1),         RegisterState {cycle = 106, value = 26})
          , (Just Noop,             RegisterState {cycle = 107, value = 26})
          , (Nothing,               RegisterState {cycle = 108, value = 26})
          , (Just (Addx (-13)),     RegisterState {cycle = 109, value = 13})
          , (Nothing,               RegisterState {cycle = 110, value = 13})
          , (Just (Addx 13),        RegisterState {cycle = 111, value = 26})
          , (Nothing,               RegisterState {cycle = 112, value = 26})
          , (Just (Addx 7),         RegisterState {cycle = 113, value = 33})
          , (Just Noop,             RegisterState {cycle = 114, value = 33})
          , (Nothing,               RegisterState {cycle = 115, value = 33})
          , (Just (Addx 1),         RegisterState {cycle = 116, value = 34})
          , (Nothing,               RegisterState {cycle = 117, value = 34})
          , (Just (Addx (-33)),     RegisterState {cycle = 118, value = 1})
          , (Just Noop,             RegisterState {cycle = 119, value = 1})
          , (Just Noop,             RegisterState {cycle = 120, value = 1})
          , (Just Noop,             RegisterState {cycle = 121, value = 1})
          , (Nothing,               RegisterState {cycle = 122, value = 1})
          , (Just (Addx 2),         RegisterState {cycle = 123, value = 3})
          , (Just Noop,             RegisterState {cycle = 124, value = 3})
          , (Just Noop,             RegisterState {cycle = 125, value = 3})
          , (Just Noop,             RegisterState {cycle = 126, value = 3})
          , (Nothing,               RegisterState {cycle = 127, value = 3})
          , (Just (Addx 8),         RegisterState {cycle = 128, value = 11})
          , (Just Noop,             RegisterState {cycle = 129, value = 11})
          , (Nothing,               RegisterState {cycle = 130, value = 11})
          , (Just (Addx (-1)),      RegisterState {cycle = 131, value = 10})
          , (Nothing,               RegisterState {cycle = 132, value = 10})
          , (Just (Addx 2),         RegisterState {cycle = 133, value = 12})
          , (Nothing,               RegisterState {cycle = 134, value = 12})
          , (Just (Addx 1),         RegisterState {cycle = 135, value = 13})
          , (Just Noop,             RegisterState {cycle = 136, value = 13})
          , (Nothing,               RegisterState {cycle = 137, value = 13})
          , (Just (Addx 17),        RegisterState {cycle = 138, value = 30})
          , (Nothing,               RegisterState {cycle = 139, value = 30})
          , (Just (Addx (-9)),      RegisterState {cycle = 140, value = 21})
          , (Nothing,               RegisterState {cycle = 141, value = 21})
          , (Just (Addx 1),         RegisterState {cycle = 142, value = 22})
          , (Nothing,               RegisterState {cycle = 143, value = 22})
          , (Just (Addx 1),         RegisterState {cycle = 144, value = 23})
          , (Nothing,               RegisterState {cycle = 145, value = 23})
          , (Just (Addx (-3)),      RegisterState {cycle = 146, value = 20})
          , (Nothing,               RegisterState {cycle = 147, value = 20})
          , (Just (Addx 11),        RegisterState {cycle = 148, value = 31})
          , (Just Noop,             RegisterState {cycle = 149, value = 31})
          , (Just Noop,             RegisterState {cycle = 150, value = 31})
          , (Nothing,               RegisterState {cycle = 151, value = 31})
          , (Just (Addx 1),         RegisterState {cycle = 152, value = 32})
          , (Just Noop,             RegisterState {cycle = 153, value = 32})
          , (Nothing,               RegisterState {cycle = 154, value = 32})
          , (Just (Addx 1),         RegisterState {cycle = 155, value = 33})
          , (Just Noop,             RegisterState {cycle = 156, value = 33})
          , (Just Noop,             RegisterState {cycle = 157, value = 33})
          , (Nothing,               RegisterState {cycle = 158, value = 33})
          , (Just (Addx (-13)),     RegisterState {cycle = 159, value = 20})
          , (Nothing,               RegisterState {cycle = 160, value = 20})
          , (Just (Addx (-19)),     RegisterState {cycle = 161, value = 1})
          , (Nothing,               RegisterState {cycle = 162, value = 1})
          , (Just (Addx 1),         RegisterState {cycle = 163, value = 2})
          , (Nothing,               RegisterState {cycle = 164, value = 2})
          , (Just (Addx 3),         RegisterState {cycle = 165, value = 5})
          , (Nothing,               RegisterState {cycle = 166, value = 5})
          , (Just (Addx 26),        RegisterState {cycle = 167, value = 31})
          , (Nothing,               RegisterState {cycle = 168, value = 31})
          , (Just (Addx (-30)),     RegisterState {cycle = 169, value = 1})
          , (Nothing,               RegisterState {cycle = 170, value = 1})
          , (Just (Addx 12),        RegisterState {cycle = 171, value = 13})
          , (Nothing,               RegisterState {cycle = 172, value = 13})
          , (Just (Addx (-1)),      RegisterState {cycle = 173, value = 12})
          , (Nothing,               RegisterState {cycle = 174, value = 12})
          , (Just (Addx 3),         RegisterState {cycle = 175, value = 15})
          , (Nothing,               RegisterState {cycle = 176, value = 15})
          , (Just (Addx 1),         RegisterState {cycle = 177, value = 16})
          , (Just Noop,             RegisterState {cycle = 178, value = 16})
          , (Just Noop,             RegisterState {cycle = 179, value = 16})
          , (Just Noop,             RegisterState {cycle = 180, value = 16})
          , (Nothing,               RegisterState {cycle = 181, value = 16})
          , (Just (Addx (-9)),      RegisterState {cycle = 182, value = 7})
          , (Nothing,               RegisterState {cycle = 183, value = 7})
          , (Just (Addx 18),        RegisterState {cycle = 184, value = 25})
          , (Nothing,               RegisterState {cycle = 185, value = 25})
          , (Just (Addx 1),         RegisterState {cycle = 186, value = 26})
          , (Nothing,               RegisterState {cycle = 187, value = 26})
          , (Just (Addx 2),         RegisterState {cycle = 188, value = 28})
          , (Just Noop,             RegisterState {cycle = 189, value = 28})
          , (Just Noop,             RegisterState {cycle = 190, value = 28})
          , (Nothing,               RegisterState {cycle = 191, value = 28})
          , (Just (Addx 9),         RegisterState {cycle = 192, value = 37})
          , (Just Noop,             RegisterState {cycle = 193, value = 37})
          , (Just Noop,             RegisterState {cycle = 194, value = 37})
          , (Just Noop,             RegisterState {cycle = 195, value = 37})
          , (Nothing,               RegisterState {cycle = 196, value = 37})
          , (Just (Addx (-1)),      RegisterState {cycle = 197, value = 36})
          , (Nothing,               RegisterState {cycle = 198, value = 36})
          , (Just (Addx 2),         RegisterState {cycle = 199, value = 38})
          , (Nothing,               RegisterState {cycle = 200, value = 38})
          , (Just (Addx (-37)),     RegisterState {cycle = 201, value = 1})
          , (Nothing,               RegisterState {cycle = 202, value = 1})
          , (Just (Addx 1),         RegisterState {cycle = 203, value = 2})
          , (Nothing,               RegisterState {cycle = 204, value = 2})
          , (Just (Addx 3),         RegisterState {cycle = 205, value = 5})
          , (Just Noop,             RegisterState {cycle = 206, value = 5})
          , (Nothing,               RegisterState {cycle = 207, value = 5})
          , (Just (Addx 15),        RegisterState {cycle = 208, value = 20})
          , (Nothing,               RegisterState {cycle = 209, value = 20})
          , (Just (Addx (-21)),     RegisterState {cycle = 210, value = -1})
          , (Nothing,               RegisterState {cycle = 211, value = -1})
          , (Just (Addx 22),        RegisterState {cycle = 212, value = 21})
          , (Nothing,               RegisterState {cycle = 213, value = 21})
          , (Just (Addx (-6)),      RegisterState {cycle = 214, value = 15})
          , (Nothing,               RegisterState {cycle = 215, value = 15})
          , (Just (Addx 1),         RegisterState {cycle = 216, value = 16})
          , (Just Noop,             RegisterState {cycle = 217, value = 16})
          , (Nothing,               RegisterState {cycle = 218, value = 16})
          , (Just (Addx 2),         RegisterState {cycle = 219, value = 18})
          , (Nothing,               RegisterState {cycle = 220, value = 18})
          , (Just (Addx 1),         RegisterState {cycle = 221, value = 19})
          , (Just Noop,             RegisterState {cycle = 222, value = 19})
          , (Nothing,               RegisterState {cycle = 223, value = 19})
          , (Just (Addx (-10)),     RegisterState {cycle = 224, value = 9})
          , (Just Noop,             RegisterState {cycle = 225, value = 9})
          , (Just Noop,             RegisterState {cycle = 226, value = 9})
          , (Nothing,               RegisterState {cycle = 227, value = 9})
          , (Just (Addx 20),        RegisterState {cycle = 228, value = 29})
          , (Nothing,               RegisterState {cycle = 229, value = 29})
          , (Just (Addx 1),         RegisterState {cycle = 230, value = 30})
          , (Nothing,               RegisterState {cycle = 231, value = 30})
          , (Just (Addx 2),         RegisterState {cycle = 232, value = 32})
          , (Nothing,               RegisterState {cycle = 233, value = 32})
          , (Just (Addx 2),         RegisterState {cycle = 234, value = 34})
          , (Nothing,               RegisterState {cycle = 235, value = 34})
          , (Just (Addx (-6)),      RegisterState {cycle = 236, value = 28})
          , (Nothing,               RegisterState {cycle = 237, value = 28})
          , (Just (Addx (-11)),     RegisterState {cycle = 238, value = 17})
          , (Just Noop,             RegisterState {cycle = 239, value = 17})
          , (Just Noop,             RegisterState {cycle = 240, value = 17})
          , (Just Noop,             RegisterState {cycle = 241, value = 17})
          ]
        (catMaybes -> exampleInstructions, exampleStates) = unzip examplePairs

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day10parser day10input
      parsedInput `shouldBe` Right exampleInstructions

    it "reports the correct output for part 1 of the example" do
      day10part1 exampleInstructions `shouldBe` 13140

    describe "toRegisterStates" do
      it "reports the correct toRegisterStates for the small example input" do
        toRegisterStates [Noop, Addx 3, Addx (-5)] `shouldBe`
          [ RegisterState 1 1
          , RegisterState 2 1
          , RegisterState 3 1
          , RegisterState 4 4
          , RegisterState 5 4
          , RegisterState 6 (-1)
          ]

      it "reports the correct toRegisterStates for the example input" do
        toRegisterStates exampleInstructions `shouldBe` exampleStates

    describe "sampleRegisterStates" do
      it "reports the correct samples for the example input" do
        sampleRegisterStates [20,60..220] (toRegisterStates exampleInstructions) `shouldBe`
          [ RegisterState  20 21
          , RegisterState  60 19
          , RegisterState 100 18
          , RegisterState 140 21
          , RegisterState 180 16
          , RegisterState 220 18
          ]

    it "reports the correct output for part 2 of the example" do
      day10part2 exampleInstructions `shouldBe` Image
        "##..##..##..##..##..##..##..##..##..##..\
        \###...###...###...###...###...###...###.\
        \####....####....####....####....####....\
        \#####.....#####.....#####.....#####.....\
        \######......######......######......####\
        \#######.......#######.......#######....."

  describeIfAvailable "day11" \day11input -> do
    let exampleInput =
          [ Note
            { items = [79,98]
            , monkey = Monkey
              { operation = Times 19
              , divisibleBy = 23
              , throwTrueTo = 2
              , throwFalseTo = 3
              }
            }
          , Note
            { items = [54,65,75,74]
            , monkey = Monkey
              { operation = Plus 6
              , divisibleBy = 19
              , throwTrueTo = 2
              , throwFalseTo = 0
              }
            }
          , Note
            { items = [79,60,97]
            , monkey = Monkey
              { operation = Square
              , divisibleBy = 13
              , throwTrueTo = 1
              , throwFalseTo = 3
              }
            }
          , Note
            { items = [74]
            , monkey = Monkey
              { operation = Plus 3
              , divisibleBy = 17
              , throwTrueTo = 0
              , throwFalseTo = 1
              }
            }
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day11parser day11input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day11part1 exampleInput `shouldBe` 10605

    it "reports the correct first round behaviour" do
      let firstRound = rounds (Just (`quot` 3)) exampleInput !! 0
      zip [0 :: Int ..] firstRound `shouldBe` 
        [ (0, [79, 98])
        , (1, [54, 65, 75, 74])
        , (2, [79, 60, 97])
        , (3, [74, 500, 620, 1200, 3136])
        ]

    it "reports the correct early round behaviour for part 2" do
      inspectionsIn Nothing 1 exampleInput `shouldBe` [2, 4, 3, 6]
      inspectionsIn Nothing 20 exampleInput `shouldBe` [99, 97, 8, 103]
      inspectionsIn Nothing 1000 exampleInput `shouldBe` [5204,4792,199,5192]

    it "reports the correct output for part 2 of the example" do
      day11part2 exampleInput `shouldBe` 2_713_310_158

  describeIfAvailable "day12" \day12input -> do
    let exampleInput = HeightMap
          { currentPosition = Position { column = 0, row = 0 }
          , bestSignal = Position { column = 5, row = 2 }
          , elevations = ElevationGrid do 
              Vector.fromList
                [  Vector.fromList [0,  0,  1, 16, 15, 14, 13, 12]
                ,  Vector.fromList [0,  1,  2, 17, 24, 23, 23, 11]
                ,  Vector.fromList [0,  2,  2, 18, 25, 25, 23, 10]
                ,  Vector.fromList [0,  2,  2, 19, 20, 21, 22,  9]
                ,  Vector.fromList [0,  1,  3,  4,  5,  6,  7,  8]
                ]
          }

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day12parser day12input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day12part1 exampleInput `shouldBe` Just 31

    it "searches the grid correctly" do
      let n = Nothing
      let HeightMap{currentPosition,elevations} = exampleInput
      let grids = search currentPosition elevations
      (grids !! 0) `shouldBe` ElevationGrid do 
        Vector.fromList
          [  Vector.fromList [Just 0,n,n,n,n,n,n,n]
          ,  Vector.fromList [n,n,n,n,n,n,n,n]
          ,  Vector.fromList [n,n,n,n,n,n,n,n]
          ,  Vector.fromList [n,n,n,n,n,n,n,n]
          ,  Vector.fromList [n,n,n,n,n,n,n,n]
          ]

      (grids !! 1) `shouldBe` ElevationGrid do 
        Vector.fromList
          [  Vector.fromList [Just 0,Just 1,n,n,n,n,n,n]
          ,  Vector.fromList [Just 1,n,n,n,n,n,n,n]
          ,  Vector.fromList [n,n,n,n,n,n,n,n]
          ,  Vector.fromList [n,n,n,n,n,n,n,n]
          ,  Vector.fromList [n,n,n,n,n,n,n,n]
          ]
    it "reports the correct output for part 2 of the example" do
      day12part2 exampleInput `shouldBe` 29

  describeIfAvailable "day13" \day13input -> do
    let exampleInput =
          [ ( [1,1,3,1,1]
            , [1,1,5,1,1]
            )

          , ( [[1],[2,3,4]]
            , [[1],4]
            )

          , ( [9]
            , [[8,7,6]]
            )

          , ( [[4,4],4,4]
            , [[4,4],4,4,4]
            )

          , ( [7,7,7,7]
            , [7,7,7]
            )

          , ( []
            , [3]
            )

          , ( [[[]]]
            , [[]]
            )

          , ( [1,[2,[3,[4,[5,6,7]]]],8,9]
            , [1,[2,[3,[4,[5,6,0]]]],8,9]
            )
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day13parser day13input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day13part1 exampleInput `shouldBe` 13

    it "orders the packets correctly" do
      order exampleInput `shouldBe`
        [ []
        , [[]]
        , [[[]]]
        , [1,1,3,1,1]
        , [1,1,5,1,1]
        , [[1],[2,3,4]]
        , [1,[2,[3,[4,[5,6,0]]]],8,9]
        , [1,[2,[3,[4,[5,6,7]]]],8,9]
        , [[1],4]
        , [[2]]
        , [3]
        , [[4,4],4,4]
        , [[4,4],4,4,4]
        , [[6]]
        , [7,7,7]
        , [7,7,7,7]
        , [[8,7,6]]
        , [9]
        ]


    it "reports the correct output for part 2 of the example" do
      day13part2 exampleInput `shouldBe` 140

  describeIfAvailable "day14" \day14input -> do
    -- The sand is pouring into the cave from point 500,0.
    let exampleInput = 
          [ RockShape [ Point{right=498,down=4}, Point{right=498,down=6}, Point{right=496,down=6} ]
          , RockShape [ Point{right=503,down=4}, Point{right=502,down=4}, Point{right=502,down=9}, Point{right=494,down=9} ]
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day14parser day14input
      parsedInput `shouldBe` Right exampleInput

    it "can load the empty cave" do
      makeCave exampleInput `shouldBe` Cave
        { leftEdge = 494
        , rightEdge = 503
        , bottomEdge = 9
        , caveContents = Map.fromList
            [ ( Point 498 4, RockFilled )
            , ( Point 498 5, RockFilled )
            , ( Point 498 6, RockFilled )
            , ( Point 497 6, RockFilled )
            , ( Point 496 6, RockFilled )
            , ( Point 503 4, RockFilled )
            , ( Point 502 4, RockFilled )
            , ( Point 502 5, RockFilled )
            , ( Point 502 6, RockFilled )
            , ( Point 502 7, RockFilled )
            , ( Point 502 8, RockFilled )
            , ( Point 502 9, RockFilled )
            , ( Point 501 9, RockFilled )
            , ( Point 500 9, RockFilled )
            , ( Point 499 9, RockFilled )
            , ( Point 498 9, RockFilled )
            , ( Point 497 9, RockFilled )
            , ( Point 496 9, RockFilled )
            , ( Point 495 9, RockFilled )
            , ( Point 494 9, RockFilled )
            ]
        }

    it "reports the correct output for part 1 of the example" do
      day14part1 exampleInput `shouldBe` 24

    it "reports the correct output for part 2 of the example" do
      day14part2 exampleInput `shouldBe` 93

  describeIfAvailable "day15" \day15input -> do
    let exampleInput = 
          [ Report {sensor=XY{x=2, y=18}, beacon=XY{x= -2, y=15}}
          , Report {sensor=XY{x=9, y=16}, beacon=XY{x=10, y=16}}
          , Report {sensor=XY{x=13, y=2}, beacon=XY{x=15, y=3}}
          , Report {sensor=XY{x=12, y=14}, beacon=XY{x=10, y=16}}
          , Report {sensor=XY{x=10, y=20}, beacon=XY{x=10, y=16}}
          , Report {sensor=XY{x=14, y=17}, beacon=XY{x=10, y=16}}
          , Report {sensor=XY{x=8, y=7}, beacon=XY{x=2, y=10}}
          , Report {sensor=XY{x=2, y=0}, beacon=XY{x=2, y=10}}
          , Report {sensor=XY{x=0, y=11}, beacon=XY{x=2, y=10}}
          , Report {sensor=XY{x=20, y=14}, beacon=XY{x=25, y=17}}
          , Report {sensor=XY{x=17, y=20}, beacon=XY{x=21, y=22}}
          , Report {sensor=XY{x=16, y=7}, beacon=XY{x=15, y=3}}
          , Report {sensor=XY{x=14, y=3}, beacon=XY{x=15, y=3}}
          , Report {sensor=XY{x=20, y=1}, beacon=XY{x=15, y=3}}
          ]

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day15parser day15input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day15part1y 10 exampleInput `shouldBe` 26

    it "reports the correct ineligble segments for part 1 of the example" do
      findSegments 10 exampleInput `shouldBe` [(-2,1), (3,24)]

    it "detects correct radiuses for part 1 of the example" do
      map radius exampleInput `shouldBe`
          [ 7  -- Report {sensor=XY{x=2, y=18}, beacon=XY{x= -2, y=15}}
          , 1  -- Report {sensor=XY{x=9, y=16}, beacon=XY{x=10, y=16}}
          , 3  -- Report {sensor=XY{x=13, y=2}, beacon=XY{x=15, y=3}}
          , 4  -- Report {sensor=XY{x=12, y=14}, beacon=XY{x=10, y=16}}
          , 4  -- Report {sensor=XY{x=10, y=20}, beacon=XY{x=10, y=16}}
          , 5  -- Report {sensor=XY{x=14, y=17}, beacon=XY{x=10, y=16}}
          , 9  -- Report {sensor=XY{x=8, y=7}, beacon=XY{x=2, y=10}}
          , 10 -- Report {sensor=XY{x=2, y=0}, beacon=XY{x=2, y=10}}
          , 3  -- Report {sensor=XY{x=0, y=11}, beacon=XY{x=2, y=10}}
          , 8  -- Report {sensor=XY{x=20, y=14}, beacon=XY{x=25, y=17}}
          , 6  -- Report {sensor=XY{x=17, y=20}, beacon=XY{x=21, y=22}}
          , 5  -- Report {sensor=XY{x=16, y=7}, beacon=XY{x=15, y=3}}
          , 1  -- Report {sensor=XY{x=14, y=3}, beacon=XY{x=15, y=3}}
          , 7  -- Report {sensor=XY{x=20, y=1}, beacon=XY{x=15, y=3}}
          ]

    it "detects correct overlaps for part 1 of the example" do
      map (overlapY 10) exampleInput `shouldBe`
          [ Nothing       -- Report {sensor=XY{x=2, y=18}, beacon=XY{x= -2, y=15}}
          , Nothing       -- Report {sensor=XY{x=9, y=16}, beacon=XY{x=10, y=16}}
          , Nothing       -- Report {sensor=XY{x=13, y=2}, beacon=XY{x=15, y=3}}
          , Just (12,12)  -- Report {sensor=XY{x=12, y=14}, beacon=XY{x=10, y=16}}
          , Nothing       -- Report {sensor=XY{x=10, y=20}, beacon=XY{x=10, y=16}}
          , Nothing       -- Report {sensor=XY{x=14, y=17}, beacon=XY{x=10, y=16}}
          , Just (3,14)   -- Report {sensor=XY{x=8, y=7}, beacon=XY{x=2, y=10}}
          , Nothing       -- Report {sensor=XY{x=2, y=0}, beacon=XY{x=2, y=10}}
          , Just (-2,1)   -- Report {sensor=XY{x=0, y=11}, beacon=XY{x=2, y=10}}
          , Just (16,24)  -- Report {sensor=XY{x=20, y=14}, beacon=XY{x=25, y=17}}
          , Nothing       -- Report {sensor=XY{x=17, y=20}, beacon=XY{x=21, y=22}}
          , Just (14,18)  -- Report {sensor=XY{x=16, y=7}, beacon=XY{x=15, y=3}}
          , Nothing       -- Report {sensor=XY{x=14, y=3}, beacon=XY{x=15, y=3}}
          , Nothing       -- Report {sensor=XY{x=20, y=1}, beacon=XY{x=15, y=3}}
          ]

    it "reports the correct output for part 2 of the example" do
      day15part2s 20 exampleInput `shouldBe` [56_000_011]

    it "reports the correct coordinates for part 2 of the example" do
      locate 20 exampleInput `shouldBe` [XY 14 11]

  describeIfAvailable "day16" \day16input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day16parser day16input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day16part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day16part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day17" \day17input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day17parser day17input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day17part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day17part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day18" \day18input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day18parser day18input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day18part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day18part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day19" \day19input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day19parser day19input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day19part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day19part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day20" \day20input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day20parser day20input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day20part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day20part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day21" \day21input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day21parser day21input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day21part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day21part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day22" \day22input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day22parser day22input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day22part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day22part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day23" \day23input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day23parser day23input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day23part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day23part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day24" \day24input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day24parser day24input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day24part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day24part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day25" \day25input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day25parser day25input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day25part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day25part2 exampleInput `shouldBe` error "unknown"

runSpecs :: SpecMonoid m => WriterT m IO () -> IO ()
runSpecs w = do
  ((), spec) <- runWriterT w
  hspec (fromSpecMonoid spec)

describeTell :: SpecMonoid m => String -> SpecWith () -> WriterT m IO ()
describeTell name = tell . toSpecMonoid . Test.Hspec.describe name

describeIfAvailable :: SpecMonoid m => String -> (String -> SpecWith ()) -> WriterT m IO ()
describeIfAvailable day test = do
  let testInput = "test/input/" <> day
  inputExists <- liftIO do doesFileExist testInput
  when inputExists do
    describeTell day (test testInput)

class Monoid m => SpecMonoid m where
  toSpecMonoid :: Spec -> m
  fromSpecMonoid :: m -> Spec

newtype AllSpecs = AllSpecs (Spec -> Spec)
  deriving (Semigroup, Monoid) via Endo Spec

instance SpecMonoid AllSpecs where
  toSpecMonoid spec = AllSpecs (*> spec)
  fromSpecMonoid (AllSpecs m) = m do pure ()

newtype LastSpec = LastSpec (Spec -> Spec)
  deriving (Semigroup, Monoid) via Dual (Endo Spec)

instance SpecMonoid LastSpec where
  toSpecMonoid spec = LastSpec (const spec)
  fromSpecMonoid (LastSpec m) = m do pure ()
