{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day12 where

import AdventOfCode.Y2023.Prelude
import Control.Monad (guard)
import Control.Monad.Trans.State (evalState, evalStateT, get, modify, state)
import Control.Monad.Trans.Writer (execWriter)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (unpack)
import Data.Traversable (for)

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = do
        let example0parsed =
              [ Row
                  { conditions = [Unknown, Unknown, Unknown, Working, Damaged, Damaged, Damaged]
                  , damagedRunLengths = [1, 1, 3]
                  }
              , Row
                  { conditions = [Working, Unknown, Unknown, Working, Working, Unknown, Unknown, Working, Working, Working, Unknown, Damaged, Damaged, Working]
                  , damagedRunLengths = [1, 1, 3]
                  }
              , Row
                  { conditions = [Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown]
                  , damagedRunLengths = [1, 3, 1, 6]
                  }
              , Row
                  { conditions = [Unknown, Unknown, Unknown, Unknown, Working, Damaged, Working, Working, Working, Damaged, Working, Working, Working]
                  , damagedRunLengths = [4, 1, 1]
                  }
              , Row
                  { conditions = [Unknown, Unknown, Unknown, Unknown, Working, Damaged, Damaged, Damaged, Damaged, Damaged, Damaged, Working, Working, Damaged, Damaged, Damaged, Damaged, Damaged, Working]
                  , damagedRunLengths = [1, 6, 5]
                  }
              , Row
                  { conditions = [Unknown, Damaged, Damaged, Damaged, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown]
                  , damagedRunLengths = [3, 2, 1]
                  }
              ]
        let example0raw =
              [text|
                ???.### 1,1,3
                .??..??...?##. 1,1,3
                ?#?#?#?#?#?#?#? 1,3,1,6
                ????.#...#... 4,1,1
                ????.######..#####. 1,6,5
                ?###???????? 3,2,1
              |]

        check
          parser
          part1
          part2
          Example
            { raw = example0raw <> "\n"
            , parsed = example0parsed
            , part1output = Just 21
            , part2output = ()
            }

        let example0counts =
              [ 1
              , 4
              , 1
              , 1
              , 4
              , 10
              ]

        let example0 =
              zip3 (lines (unpack example0raw)) example0parsed example0counts

        for_ example0 \(raw, row, count) -> do
          it ("produces the proper count for " <> raw) do
            numArrangements row `shouldBe` Just count

        let (example0raw1, example0row1, _example0count1) = example0 !! 1
        describe ("Example " <> example0raw1) do
          it "counts the damaged run lengths correctly" do
            numDamagedRunLengths (arrangementCounts example0row1)
              `shouldBe` 3

          it "computes the conditionRuns correctly" do
            conditionRuns (arrangementCounts example0row1)
              `shouldBe` [ Run Working 1 0
                         , Run Unknown 2 1
                         , Run Working 2 3
                         , Run Unknown 2 5
                         , Run Working 3 7
                         , Run Unknown 1 10
                         , Run Damaged 2 11
                         , Run Working 1 13
                         ]
          it "computes the startingPositionsForLength correctly" do
            startingPositionsForLength (arrangementCounts example0row1)
              `shouldBe` Map.fromList
                [ (1, Set.fromList [1, 2, 5, 6])
                , (2, Set.fromList [1, 5, 11])
                , (3, Set.fromList [10])
                ]

          it "computes the numKnownDamagedBefore correctly" do
            numKnownDamagedBefore (arrangementCounts example0row1)
              `shouldBe` Map.fromList
                [ (0, 0)
                , (1, 0)
                , (2, 0)
                , (3, 0)
                , (4, 0)
                , (5, 0)
                , (6, 0)
                , (7, 0)
                , (8, 0)
                , (9, 0)
                , (10, 0)
                , (11, 0)
                , (12, 1)
                , (13, 1)
                ]

          it "computes the startingPositionsForDamagedRun correctly" do
            startingPositionsForDamagedRun (arrangementCounts example0row1)
              `shouldBe` [ [(1, Just 0), (2, Just 0), (5, Just 0), (6, Just 0)]
                         , [(2, Just 0), (5, Just 0), (6, Just 0)]
                         , [(10, Just 0)]
                         ]

          it "computes the numArrangementsAfter correctly" do
            numArrangementsAfter (arrangementCounts example0row1)
              `shouldBe` Map.fromList
                [ ((0, 1), 4)
                , ((0, 2), 2)
                , ((0, 5), 0)
                , ((0, 6), 0)
                , ((1, 2), 3)
                , ((1, 5), 2)
                , ((1, 6), 1)
                , ((2, 10), 1)
                ]
    }

type Input :: Type
type Input = [Row]

type Row :: Type
data Row = Row
  { conditions :: [Condition]
  , damagedRunLengths :: [Int]
  }
  deriving stock (Show, Eq)

type Condition :: Type
data Condition
  = Working
  | Unknown
  | Damaged
  deriving stock (Show, Eq)

parser :: Parser Input
parser = row `endBy` newline
  where
    row = Row <$> many condition <* char ' ' <*> (int `sepBy` char ',')
    condition =
      asum
        [ char '.' $> Working
        , char '?' $> Unknown
        , char '#' $> Damaged
        ]
    int = read <$> many digit

part1 :: Input -> Maybe Int
part1 = fmap sum . traverse numArrangements

type Run :: Type
data Run = Run
  { condition :: Condition
  , size :: Int
  , column :: Int
  }
  deriving stock (Show, Eq)

type ArrangementCounts :: Type
data ArrangementCounts = ArrangementCounts
  { total :: Maybe Int
  , numDamagedRunLengths :: Int
  , numColumns :: Int
  , conditionRuns :: [Run]
  , startingPositionsForLength :: Map.Map Int (Set.Set Int)
  , numKnownDamagedBefore :: Map.Map Int Int
  , startingPositionsForDamagedRun :: [[(Int, Maybe Int)]]
  , numArrangementsAfter :: Map.Map (Int, Int) Int
  }

numArrangements :: Row -> Maybe Int
numArrangements = total . arrangementCounts

arrangementCounts :: Row -> ArrangementCounts
arrangementCounts Row {..} = ArrangementCounts {..}
  where
    total = getNumArrangementsAfter 0 0

    getNumArrangementsAfter run startCol =
      listToMaybe
        [ num
        | col <- [startCol .. numColumns - 1]
        , num <- maybeToList do
            Map.lookup (run, col) numArrangementsAfter
        ]

    numDamagedRunLengths = length damagedRunLengths

    numColumns = length conditions

    conditionRuns = flip evalState 0 do
      for (NonEmpty.group conditions) \g -> do
        let condition = NonEmpty.head g
        let size = length g
        column <- state \c -> (c, c + size)
        pure Run {..}

    startingPositionsForLength = Map.fromListWith (<>) . execWriter . (`evalStateT` []) $ do
      for_ conditionRuns \Run {..} -> case condition of
        Working -> do
          starts <- state (,[])
          tell [(column - start, Set.singleton start) | start <- starts]
        Damaged -> do
          modify (column :)
        Unknown -> do
          let last = column + size - 1
          modify ([column .. last] <>)
          starts <- get
          tell [(stop - start, Set.singleton start) | start <- starts, stop <- [max (start + 1) column .. last]]

      starts <- state (,[])
      tell [(numColumns - start, Set.singleton start) | start <- starts]

    numKnownDamagedBefore =
      Map.fromList . zip [0 .. numColumns - 1] . scanl (+) 0 $
        zipWith
          do \curr prev -> if curr == Damaged && prev /= Damaged then 1 else 0
          do conditions
          do Working : conditions

    startingPositionsForDamagedRun = do
      (len, minCol, maxCol) <- zip3
        do damagedRunLengths
        do scanl (\col len -> col + len + 1) 0 damagedRunLengths
        do scanr (\len col -> col - len - 1) (numColumns + 1) damagedRunLengths

      pure do
        startsForLen <- maybeToList do
          Map.lookup len startingPositionsForLength

        col <- [minCol .. maxCol]

        guard do
          Set.member col startsForLen

        pure (col, Map.lookup col numKnownDamagedBefore)

    numArrangementsAfter = Map.fromList do
      (run, len, starts) <- zip3 [0 ..] damagedRunLengths startingPositionsForDamagedRun

      ((start, knownHere), continue) <- zip starts do
        fmap Just (tail starts) <> [Nothing]

      let numHere
            | run + 1 < numDamagedRunLengths = fromMaybe 0 do
                getNumArrangementsAfter (run + 1) (start + len + 1)
            | otherwise = 1

      let numLater = fromMaybe 0 do
            (nextStart, knownThere) <- continue
            guard do knownHere == knownThere
            Map.lookup (run, nextStart) numArrangementsAfter

      pure ((run, start), numHere + numLater)

part2 :: Input -> ()
part2 _ = ()
