{-# LANGUAGE TupleSections #-}
module AdventOfCode.Day11 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec
import Control.Applicative (some)
import Data.Foldable (asum)
import Data.IntMap
import Data.Ord (Down(..))
import Data.List (sort, foldl', transpose, unfoldr)
import Data.Coerce (coerce)
import Control.Arrow (first, second)
import Control.Monad (join)
import Data.Maybe (fromMaybe)

type MonkeyId = Int
type WorryLevel = Int
type Throw = (MonkeyId, WorryLevel)
type Turn = [WorryLevel]
type Round = [Turn]
type MonkeyMap = IntMap

data Note = Note
  { items :: [WorryLevel]
  , monkey :: Monkey
  }
  deriving stock (Show, Eq)

data Monkey = Monkey
  { operation :: Operation
  , divisibleBy :: WorryLevel
  , throwTrueTo :: MonkeyId
  , throwFalseTo :: MonkeyId
  }
  deriving stock (Show, Eq)

data Operation
  = Plus WorryLevel
  | Times WorryLevel
  | Square
  deriving stock (Show, Eq)

day11parser :: Parser [Note]
day11parser = note `sepBy` newline where
  note = Note <$ header <*> items <*> monkey
  header = string "Monkey " <* int <* char ':' <* newline
  items = string "  Starting items: " *> (worryLevel `sepBy` string ", ") <* newline
  monkey = Monkey <$> operation <*> test <*> ifTrue <*> ifFalse
  operation = string "  Operation: new = old " *> asum
    [ Plus <$ string "+ " <*> worryLevel
    , Square <$ try (string "* old")
    , Times <$ string "* " <*> worryLevel
    ] <* newline
  test = string "  Test: divisible by " *> worryLevel <* newline
  ifTrue = string "    If true: throw to monkey " *> int <* newline
  ifFalse = string "    If false: throw to monkey " *> int <* newline
  int = read <$> some digit
  worryLevel = read <$> some digit

day11part1 :: [Note] -> Int
day11part1 = monkeyBusiness . inspectionsIn (Just (`quot` 3)) 20

monkeyBusiness :: [Int] -> Int
monkeyBusiness = product . take 2 . coerce (sort @(Down Int))

inspectionsIn :: Maybe (WorryLevel -> WorryLevel) -> Int -> [Note] -> [Int]
inspectionsIn moderate numRounds = fmap countInspections . turnsIn moderate numRounds

turnsIn :: Maybe (WorryLevel -> WorryLevel) -> Int -> [Note] -> [[Turn]]
turnsIn moderate numRounds = transpose . take numRounds . rounds moderate

countInspections :: [Turn] -> Int
countInspections = sum . fmap length

rounds :: Maybe (WorryLevel -> WorryLevel) -> [Note] -> [Round]
rounds moderate (fromList . zip [0..] -> notes) = chunksOf (size notes) do 
  unfoldr 
    do nextTurn (mapWithKey (\k -> behaviour m k . monkey) notes) 
    do (empty, fmap (reverse . items) notes)
  where
    m = fromMaybe (`rem` modulus) moderate
    modulus = product (divisibleBy . monkey <$> notes)

behaviour :: (WorryLevel -> WorryLevel) -> MonkeyId -> Monkey -> TurnState -> WorryLevel -> TurnState
behaviour moderate monkeyId Monkey
  { operation
  , divisibleBy
  , throwTrueTo
  , throwFalseTo
  } = flip inspectAndThrow
  where
    inspect = moderate . case operation of
      Plus n -> (+n)
      Times n -> (*n)
      Square -> join (*)

    nextMonkey newLevel
      | newLevel `rem` divisibleBy == 0 = throwTrueTo
      | otherwise = throwFalseTo

    switch nextMonkeyId
      | nextMonkeyId <= monkeyId = first
      | otherwise                = second 

    inspectAndThrow oldLevel =
      let newLevel = inspect oldLevel
          nextMonkeyId = nextMonkey newLevel
      in switch nextMonkeyId (adjust (newLevel:) nextMonkeyId)

type TurnState = (MonkeyMap [WorryLevel], MonkeyMap [WorryLevel])

nextTurn :: MonkeyMap (TurnState -> WorryLevel -> TurnState)  -> TurnState -> Maybe (Turn, TurnState)
nextTurn inspectAndThrow = go where
  go (lt, gte) = case minViewWithKey gte of
    Nothing -> go (empty, lt)
    Just ((monkeyId, reverse -> items), gt) -> Just
      ( items
      , Data.List.foldl'
          do inspectAndThrow ! monkeyId 
          do (insert monkeyId [] lt, gt)
          do items
      )

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr \case
  [] -> Nothing
  as -> Just do splitAt n as

day11part2 :: [Note] -> Int
day11part2 = monkeyBusiness . inspectionsIn Nothing 10_000
