{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedRecordDot #-}
module AdventOfCode.Y2023.Day4 where

import Data.Foldable (fold)
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.List (foldl')
import AdventOfCode.Y2023.Prelude
import Data.Monoid (Sum(..))
import Data.Set qualified as Set

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      describe "winnings" do
        it "should match the example" do
          winnings parsed `shouldBe`
            IntMap.fromList
              [ (1, 1)
              , (2, 2)
              , (3, 4)
              , (4, 8)
              , (5, 14)
              , (6, 1)
              ]
        
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
            Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
            Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
            Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
            Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
            Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
          |]
        , parsed
        , part1output = 13
        , part2output = 30
        }
  }

parsed :: Input
parsed =
  [ ( 1, Card { winners = [41, 48, 83, 86, 17], numbers = [83, 86, 6, 31, 17, 9, 48, 53] })
  , ( 2, Card { winners = [13, 32, 20, 16, 61], numbers = [61, 30, 68, 82, 17, 32, 24, 19] })
  , ( 3, Card { winners = [ 1, 21, 53, 59, 44], numbers = [69, 82, 63, 72, 16, 21, 14, 1] })
  , ( 4, Card { winners = [41, 92, 73, 84, 69], numbers = [59, 84, 76, 51, 58, 5, 54, 83] })
  , ( 5, Card { winners = [87, 83, 26, 28, 32], numbers = [88, 30, 70, 12, 93, 22, 82, 36] })
  , ( 6, Card { winners = [31, 18, 13, 56, 72], numbers = [74, 77, 10, 23, 35, 67, 36, 11] })
  ]

type Input :: Type
type Input = [(Int, Card)]

type Card :: Type
data Card = Card
  { winners :: [Int]
  , numbers :: [Int]
  }
  deriving stock (Show, Eq)

parser :: Parser Input
parser = line `endBy1` newline where
  line = (,) <$> index <* many1 space <*> card
  index = string "Card" *> many1 space *> int <* char ':'
  card = Card <$>  many1 (int <* many1 space) <* string "|" <*> many1 (many1 space *> int)
  int = read <$> many1 digit
  space = char ' '

score :: Card -> Int
score card = Set.size do
  Set.intersection (Set.fromList card.winners) (Set.fromList card.numbers)

adjust :: Int -> Int
adjust n = case n of
  0 -> 0
  n -> 2 ^ (n - 1)

part1 :: Input -> Sum Int
part1 = foldMap (Sum . adjust . score . snd)

winnings :: Input -> IntMap (Sum Int)
winnings = foldl' step =<< initial where
  initial = IntMap.fromList . fmap (const 1 <$>)

  step count (i, card) = IntMap.unionWith (<>) count do
    IntMap.fromList [ (j, m) | let m = count IntMap.! i, j <- [ i + 1 .. i + score card ]]

part2 :: Input -> Sum Int
part2 = fold . winnings
