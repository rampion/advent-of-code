{-# LANGUAGE TupleSections #-}
module AdventOfCode.Y2022.Day11 where

import AdventOfCode.Y2022.Prelude
import Data.IntMap qualified as IntMap
import Data.Ord (Down(..))
import Data.List (sort, foldl', transpose, unfoldr)
import Data.Coerce (coerce)
import Control.Arrow (first, second)
import Control.Monad (join)
import Data.Maybe (fromMaybe)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
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

      check parser part1 part2 Example
        { raw = [text|
            Monkey 0:
              Starting items: 79, 98
              Operation: new = old * 19
              Test: divisible by 23
                If true: throw to monkey 2
                If false: throw to monkey 3

            Monkey 1:
              Starting items: 54, 65, 75, 74
              Operation: new = old + 6
              Test: divisible by 19
                If true: throw to monkey 2
                If false: throw to monkey 0

            Monkey 2:
              Starting items: 79, 60, 97
              Operation: new = old * old
              Test: divisible by 13
                If true: throw to monkey 1
                If false: throw to monkey 3

            Monkey 3:
              Starting items: 74
              Operation: new = old + 3
              Test: divisible by 17
                If true: throw to monkey 0
                If false: throw to monkey 1
          |] <> "\n"
        , parsed = exampleInput
        , part1output = 10605
        , part2output = 2_713_310_158
        }

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
  }

type MonkeyId :: Type
type MonkeyId = Int

type WorryLevel :: Type
type WorryLevel = Int

type Throw :: Type
type Throw = (MonkeyId, WorryLevel)

type Turn :: Type
type Turn = [WorryLevel]

type Round :: Type
type Round = [Turn]

type MonkeyMap :: Type -> Type
type MonkeyMap = IntMap.IntMap

type Note :: Type
data Note = Note
  { items :: [WorryLevel]
  , monkey :: Monkey
  }
  deriving stock (Show, Eq)

type Monkey :: Type
data Monkey = Monkey
  { operation :: Operation
  , divisibleBy :: WorryLevel
  , throwTrueTo :: MonkeyId
  , throwFalseTo :: MonkeyId
  }
  deriving stock (Show, Eq)

type Operation :: Type
data Operation
  = Plus WorryLevel
  | Times WorryLevel
  | Square
  deriving stock (Show, Eq)

parser :: Parser [Note]
parser = note `sepBy` newline where
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

part1 :: [Note] -> Int
part1 = monkeyBusiness . inspectionsIn (Just (`quot` 3)) 20

monkeyBusiness :: [Int] -> Int
monkeyBusiness = product . take 2 . coerce (sort @(Down Int))

inspectionsIn :: Maybe (WorryLevel -> WorryLevel) -> Int -> [Note] -> [Int]
inspectionsIn moderate numRounds = fmap countInspections . turnsIn moderate numRounds

turnsIn :: Maybe (WorryLevel -> WorryLevel) -> Int -> [Note] -> [[Turn]]
turnsIn moderate numRounds = transpose . take numRounds . rounds moderate

countInspections :: [Turn] -> Int
countInspections = sum . fmap length

rounds :: Maybe (WorryLevel -> WorryLevel) -> [Note] -> [Round]
rounds moderate (IntMap.fromList . zip [0..] -> notes) = chunksOf (IntMap.size notes) do 
  unfoldr 
    do nextTurn (IntMap.mapWithKey (\k -> behaviour m k . monkey) notes) 
    do (IntMap.empty, fmap (reverse . items) notes)
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
      Plus n -> (+ n)
      Times n -> (* n)
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
      in switch nextMonkeyId (IntMap.adjust (newLevel:) nextMonkeyId)

type TurnState :: Type
type TurnState = (MonkeyMap [WorryLevel], MonkeyMap [WorryLevel])

nextTurn :: MonkeyMap (TurnState -> WorryLevel -> TurnState)  -> TurnState -> Maybe (Turn, TurnState)
nextTurn inspectAndThrow = go where
  go (lt, gte) = case IntMap.minViewWithKey gte of
    Nothing -> go (IntMap.empty, lt)
    Just ((monkeyId, reverse -> items), gt) -> Just
      ( items
      , Data.List.foldl'
          do inspectAndThrow IntMap.! monkeyId 
          do (IntMap.insert monkeyId [] lt, gt)
          do items
      )

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr \case
  [] -> Nothing
  as -> Just do splitAt n as

part2 :: [Note] -> Int
part2 = monkeyBusiness . inspectionsIn Nothing 10_000
