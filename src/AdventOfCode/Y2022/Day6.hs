module AdventOfCode.Y2022.Day6 where

import AdventOfCode.Y2022.Prelude
import Data.Map.Strict qualified as Map
import Control.Monad (forM_)
import Data.Text qualified as Text

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = tellSpec do
        let examples =
              zip
                [0 :: Int ..]
                [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19)
                , ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23)
                , ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23)
                , ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29)
                , ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
                ]

        forM_ examples \(i, (exampleInput, _, _)) -> do
          let name = "example input " ++ show i

          it ("can parse " ++ name) do
            runParser parser () name (Text.pack exampleInput) `shouldBe` Right exampleInput

        describe "part 1" do
          forM_ examples \(i, (exampleInput, exampleOutput1, _)) -> do
            let name = "example input " ++ show i
            it ("reports the correct output for part 1 with " ++ name) do
              part1 exampleInput `shouldBe` exampleOutput1

        describe "part 2" do
          forM_ examples \(i, (exampleInput, _, exampleOutput2)) -> do
            let name = "example input " ++ show i
            it ("reports the correct output for part 2 with " ++ name) do
              part2 exampleInput `shouldBe` exampleOutput2
    }

parser :: Parser String
parser = many (oneOf ['a' .. 'z'])

part1 :: String -> Int
part1 = findUnique 4

findUnique :: Int -> String -> Int
findUnique width = \input ->
  let (chunk, later) = splitAt width input
      counts = Map.fromListWith (+) do zip chunk (repeat 1)
   in loop counts (zip3 [width ..] input later)
  where
    loop :: Map.Map Char Int -> [(Int, Char, Char)] -> Int
    loop counts ((pos, old, new) : ts)
      | Map.size counts == width = pos
      | otherwise = loop (Map.insertWith (+) new 1 do Map.update dec old counts) ts
    loop _ [] = -1

    dec 1 = Nothing
    dec n = Just (n - 1)

part2 :: String -> Int
part2 = findUnique 14
