{-# OPTIONS_GHC -Wno-name-shadowing #-}
module AdventOfCode.Day3 where

import AdventOfCode.Solver
import Control.Applicative (some, (<|>))
import Control.Arrow ((***))
import Data.Functor ((<&>))
import Data.Set qualified as Set
import Prelude
import Text.Parsec hiding ((<|>))

day3parser :: Parser [[(Int,Int)]]
day3parser = rucksack `endBy` newline where
  lower = oneOf ['a'..'z'] <&> \c -> fromEnum c - fromEnum 'a' + 1
  upper = oneOf ['A'..'Z'] <&> \c -> fromEnum c - fromEnum 'A' + 27
  item = lower <|> upper
  rucksack = some item <&> \items -> zip items (drop (length items `div` 2) items)

day3part1 :: [[(Int,Int)]] -> Int
day3part1 = sum . map inCommon where
  inCommon = head . Set.toList . uncurry Set.intersection . (Set.fromList *** Set.fromList) . unzip

day3part2 :: [[(Int,Int)]] -> Int
day3part2 = sum . triples findBadge . map toSet where
  toSet = Set.fromList . uncurry (++) . unzip
  findBadge a b c = head do Set.toList do a `Set.intersection` b `Set.intersection` c
  triples f (a0:a1:a2:as) = f a0 a1 a2 : triples f as
  triples _ _ = []
