{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day11 where

import AdventOfCode.Y2023.Prelude
import Data.List (tails, transpose)
import Data.Bool (bool)

solver :: Solver
solver = Solver
  { parser
  , part1 = part2 2
  , part2 = part2 1_000_000
  , spec = do
      let parsed = 
            [ [ F, F, F, T, F, F, F, F, F, F ]
            , [ F, F, F, F, F, F, F, T, F, F ]
            , [ T, F, F, F, F, F, F, F, F, F ]
            , [ F, F, F, F, F, F, F, F, F, F ]
            , [ F, F, F, F, F, F, T, F, F, F ]
            , [ F, T, F, F, F, F, F, F, F, F ]
            , [ F, F, F, F, F, F, F, F, F, T ]
            , [ F, F, F, F, F, F, F, F, F, F ]
            , [ F, F, F, F, F, F, F, T, F, F ]
            , [ T, F, F, F, T, F, F, F, F, F ]
            ]
      check parser (part2 2) (const ()) Example
        { raw = (<> "\n") [text|
            ...#......
            .......#..
            #.........
            ..........
            ......#...
            .#........
            .........#
            ..........
            .......#..
            #...#.....
          |]
        , parsed
        , part1output = 374
        , part2output = ()
        }

      it "gets the right answer for part two with a factor of 10" do
        part2 10 parsed `shouldBe` 1030

      it "gets the right answer for part two with a factor of 10" do
        part2 100 parsed `shouldBe` 8410
  }

pattern T :: Bool
pattern T = True

pattern F :: Bool
pattern F = False

type Input :: Type
type Input = [[Bool]]

parser :: Parser Input
parser = some tile `endBy` newline where
  tile = asum
    [ char '.' $> F
    , char '#' $> T
    ]

distances :: Num a => [(a,a)] -> [a]
distances pairs = do
  (y₀,x₀):ps <- tails pairs
  (y₁,x₁) <- ps
  pure do
    abs (y₀ - y₁) + abs (x₀ - x₁)

part2 :: Integer -> Input -> Integer
part2 n = sum . distances . expandedPoints (n - 1)

expandedPoints :: Integer -> Input -> [(Integer,Integer)]
expandedPoints factor input = do
  let ys = zipWith (+) [0..] . tail . scanl (+) 0 $ map (bool 0 factor . all not) input
      xs = zipWith (+) [0..] . tail . scanl (+) 0 . map (bool 0 factor . all not) $ transpose input
  (y, row) <- zip ys input
  (x, T) <- zip xs row
  pure (y,x)
