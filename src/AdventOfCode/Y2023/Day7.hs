{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day7 where

import Control.Monad (replicateM)
import AdventOfCode.Y2023.Prelude
import Data.List (sortBy, sort, foldl')
import Control.Arrow (first, (&&&))
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            32T3K 765
            T55J5 684
            KK677 28
            KTJJT 220
            QQQJA 483
          |]
        , parsed
        , part1output = 6440
        , part2output = 5905
        }
  }

type Input :: Type
type Input = [(Hand, Bid)]

type Hand :: Type
type Hand = [Card]

type Bid :: Type
type Bid = Integer

type Card :: Type
data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving stock (Show, Eq, Ord)

type Rank :: Type
data Rank
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving stock (Show, Eq, Ord)

parsed :: Input
parsed = 
  [ ([Three,Two,Ten,Three,King], 765)
  , ([Ten,Five,Five,Jack,Five], 684)
  , ([King,King,Six,Seven,Seven], 28)
  , ([King,Ten,Jack,Jack,Ten], 220)
  , ([Queen,Queen,Queen,Jack,Ace], 483)
  ]

parser :: Parser Input
parser = line `endBy` newline where
  line = (,) <$> replicateM 5 card <* space <*> integer
  card = asum
    [ char '2' $> Two
    , char '3' $> Three
    , char '4' $> Four
    , char '5' $> Five
    , char '6' $> Six
    , char '7' $> Seven
    , char '8' $> Eight
    , char '9' $> Nine
    , char 'T' $> Ten
    , char 'J' $> Jack
    , char 'Q' $> Queen
    , char 'K' $> King
    , char 'A' $> Ace
    ]
  space = char ' '
  integer = read <$> some digit

part1 :: Input -> Integer
part1
  = sum
  . zipWith (*) [1..]
  . map snd
  . sortBy (comparing fst)
  . map (first (rank &&& id))

rank :: [Card] -> Rank
rank = final . foldl' step Map.empty where
  step m c = Map.insertWith (+) c (1 :: Integer) m
  final = 
    categorize
    . sort 
    . Map.elems
    . Map.delete Joker

  categorize = \case
    [] -> FiveOfAKind
    [_] -> FiveOfAKind
    [1,_] -> FourOfAKind
    [_,_] -> FullHouse
    [_,1,_] -> ThreeOfAKind
    [_,_,_] -> TwoPair
    [_,_,_,_] -> OnePair
    _ -> HighCard

part2 :: Input -> Integer
part2 = part1 . map (first (map jokerize))

jokerize :: Card -> Card
jokerize = \case
  Jack -> Joker
  c -> c
