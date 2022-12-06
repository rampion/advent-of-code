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
          [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, error "unknown")
          , ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, error "unknown")
          , ("nppdvjthqldpwncqszvftbrmjlhg", 6, error "unknown")
          , ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, error "unknown")
          , ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, error "unknown")
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
        xit ("reports the correct output for part 2 with " ++ name) do
          day6part2 exampleInput `shouldBe` exampleOutput2

  describeIfAvailable "day7" \day7input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day7parser day7input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day7part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day7part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day8" \day8input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day8parser day8input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day8part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day8part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day9" \day9input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day9parser day9input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day9part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day9part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day10" \day10input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day10parser day10input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day10part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day10part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day11" \day11input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day11parser day11input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day11part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day11part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day12" \day12input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day12parser day12input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day12part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day12part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day13" \day13input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day13parser day13input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day13part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day13part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day14" \day14input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day14parser day14input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day14part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day14part2 exampleInput `shouldBe` error "unknown"

  describeIfAvailable "day15" \day15input -> do
    let exampleInput = error "unknown"

    it "can parse the example input" do
      parsedInput <- liftIO do parseFromFile day15parser day15input
      parsedInput `shouldBe` Right exampleInput

    it "reports the correct output for part 1 of the example" do
      day15part1 exampleInput `shouldBe` error "unknown"

    it "reports the correct output for part 2 of the example" do
      day15part2 exampleInput `shouldBe` error "unknown"

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
