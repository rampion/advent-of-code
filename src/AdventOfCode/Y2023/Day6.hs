{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day6 where

import AdventOfCode.Y2023.Prelude
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Functor ((<&>))

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            Time:      7  15   30
            Distance:  9  40  200
          |]
        , parsed
        , part1output = 288
        , part2output = 71503
        }
  }

parsed :: Input
parsed =
  [ Race 7 9
  , Race 15 40
  , Race 30 200
  ]

type Input :: Type
type Input = [Race]

type Race :: Type
data Race = Race
  { milliseconds :: Integer
  , millimeters :: Integer
  }
  deriving stock (Show, Eq)

parser :: Parser Input
parser = liftA2 
    do zipWith Race
    do string "Time:" *> values
    do string "Distance:" *> values
  where
    values = some (some space *> int) <* newline
    space = char ' '
    int = read <$> some digit

part1 :: Input -> Integer
part1 = product . fmap recordBeatingCount

recordBeatingCount :: Race -> Integer
-- recordBeatingCount (Race t d) = sum [ 1 | s <- [0..t], s * (t - s) > d ]
recordBeatingCount (Race t d) = fromMaybe 0 do
  find (\s -> s * (t -s) > d) [0..t] <&> \s -> t - 2*s + 1
    

part2 :: Input -> Integer
part2 = recordBeatingCount . merge

merge :: [Race] -> Race
merge = liftA2 Race 
  do convert milliseconds
  do convert millimeters

  where
    convert f = read . concatMap (show . f)
