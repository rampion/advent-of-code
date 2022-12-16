{-# OPTIONS_GHC -Wno-name-shadowing #-}
module AdventOfCode.Y2022.Day3 where

import AdventOfCode.Y2022.Prelude
import Control.Arrow ((***))
import Data.Functor ((<&>))
import Data.Set qualified as Set

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = check parser part1 part2 Example
    { raw = (<> "\n") [text|
        vJrwpWtwJgWrhcsFMMfFFhFp
        jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        PmmdzqPrVvPwwTWBwg
        wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        ttgJtRGJQctTZtZT
        CrZsJsPPZsGzwwsLwLmpwMDw
      |]
    , parsed = 
        [ zip [22, 36, 18, 23, 16, 49, 20, 23, 36, 7, 49, 18] [8, 3, 19, 32, 39, 39, 6, 32, 32, 8, 32, 16]
        , zip [10, 17, 34, 44, 40, 17, 44, 10, 17, 26, 10, 33, 30, 38, 33, 38] [18, 19, 32, 39, 6, 32, 52, 45, 18, 38, 18, 32, 52, 19, 45, 38]
        , zip [42, 13, 13, 4, 26, 17, 42, 18, 48] [22, 42, 23, 23, 46, 49, 28, 23, 7]
        , zip [23, 39, 17, 22, 38, 39, 52, 34, 8, 34, 39, 22, 23, 38, 34] [10, 2, 22, 3, 10, 14, 14, 45, 28, 14, 22, 46, 43, 32, 14]
        , zip [20, 20, 7, 36, 20, 44, 33, 36] [43, 3, 20, 46, 52, 20, 52, 46]
        , zip [29, 18, 52, 19, 36, 19, 42, 42, 52, 19, 33, 26] [23, 23, 19, 38, 23, 38, 13, 16, 23, 39, 30, 23]
        ]
    , part1output = 157
    , part2output = 70
    }
  }

parser :: Parser [[(Int,Int)]]
parser = rucksack `endBy` newline where
  lower = oneOf ['a'..'z'] <&> \c -> fromEnum c - fromEnum 'a' + 1
  upper = oneOf ['A'..'Z'] <&> \c -> fromEnum c - fromEnum 'A' + 27
  item = lower <|> upper
  rucksack = some item <&> \items -> zip items (drop (length items `div` 2) items)

part1 :: [[(Int,Int)]] -> Int
part1 = sum . map inCommon where
  inCommon = head . Set.toList . uncurry Set.intersection . (Set.fromList *** Set.fromList) . unzip

part2 :: [[(Int,Int)]] -> Int
part2 = sum . triples findBadge . map toSet where
  toSet = Set.fromList . uncurry (++) . unzip
  findBadge a b c = head do Set.toList do a `Set.intersection` b `Set.intersection` c
  triples f (a0:a1:a2:as) = f a0 a1 a2 : triples f as
  triples _ _ = []
