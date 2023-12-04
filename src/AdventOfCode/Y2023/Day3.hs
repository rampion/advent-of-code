{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedRecordDot #-}
module AdventOfCode.Y2023.Day3 where

import AdventOfCode.Y2023.Prelude
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Functor ((<&>))
import Data.Either (partitionEithers)
import Data.Foldable (fold)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            467..114..
            ...*......
            ..35..633.
            ......#...
            617*......
            .....+.58.
            ..592.....
            ......755.
            ...$.*....
            .664.598..
          |]
        , parsed = 
            ( [ (At 0 0, 467, 3)
              , (At 0 5, 114, 3)
              , (At 2 2, 35, 2)
              , (At 2 6, 633, 3)
              , (At 4 0, 617, 3)
              , (At 5 7, 58, 2)
              , (At 6 2, 592, 3)
              , (At 7 6, 755, 3)
              , (At 9 1, 664, 3)
              , (At 9 5, 598, 3)
              ]
            , [ (At 1 3, '*')
              , (At 3 6, '#')
              , (At 4 3, '*')
              , (At 5 5, '+')
              , (At 8 3, '$')
              , (At 8 5, '*')
              ]
            )
            
        , part1output = 4361
        , part2output = 467835
        }
  }

type Position :: Type
data Position = At { row :: Int, column :: Int }
  deriving stock (Eq, Ord, Show)

type Input :: Type
type Input = ([Number], [Symbol])

type Number :: Type
type Number = (Position, Integer, Int)

type Symbol :: Type
type Symbol = (Position, Char)

parser :: Parser Input
parser = partitionEithers . concat <$> (line `endBy1` newline) where
  line = many period *> many (tag (number <|> symbol) <* many period)
  period = char '.'
  number = many1 digit <&> \ds pos -> Left (pos, read ds, length ds)
  symbol = oneOf "!@#$%^&*()-+={}[]\\|;;\"'<>,?/" <&> \c pos -> Right (pos, c)
  tag = (<**>) position
  position = liftA2 At (pred . sourceLine) (pred . sourceColumn) <$> getPosition

part1 :: Input -> Integer
part1 (numbers, symbols) = total do
  let cached = adjacencies numbers

  (pos, _) <- symbols

  maybeToList do Map.lookup pos cached

  where
    total = sum . fmap (\(_,n,_) -> n) . Set.toList . fold

part2 :: Input -> Integer
part2 (numbers, symbols) = sum do
  let cached = adjacencies numbers

  (pos, '*') <- symbols

  [(_,a,_),(_,b,_)]  <- maybeToList do Set.toList <$> Map.lookup pos cached

  pure (a * b)

adjacencies :: [Number] -> Map.Map Position (Set.Set Number)
adjacencies numbers = Map.fromListWith (<>) do
    (pos, n, width) <- numbers
    row <- [pos.row - 1 .. pos.row + 1]
    column <- [pos.column - 1 .. pos.column + width]
    pure (At{row,column}, Set.singleton (pos, n, width))
