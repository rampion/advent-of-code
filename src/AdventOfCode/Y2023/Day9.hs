{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day9 where

import AdventOfCode.Y2023.Prelude

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      let firstExample = 
            [ [0, 3, 6, 9, 12, 15] 
            , [1, 3, 6, 10, 15, 21]
            , [10, 13, 16, 21, 30, 45] 
            ]

      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            0 3 6 9 12 15
            1 3 6 10 15 21
            10 13 16 21 30 45
          |]
        , parsed = firstExample
        , part1output = 114
        , part2output = 2
        }
  }

type Input :: Type
type Input = [[Int]]

parser :: Parser Input
parser = (int `sepBy` space) `endBy` newline where
  int = (negate <$ char '-' <*> nat) <|> nat
  nat = read <$> some digit
  space = char ' '

part1 :: Input -> Int
part1 = sum . map (extrapolate . reverse)

extrapolate :: [Int] -> Int
extrapolate [] = 0
extrapolate as | all (== 0) as = 0
extrapolate as@(a:at) = a + extrapolate (zipWith (-) as at)

part2 :: Input -> Int
part2 = sum . map extrapolate
