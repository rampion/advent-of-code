module AdventOfCode.Day2 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec hiding ((<|>))
import Control.Applicative
import Control.Comonad

data RockPaperScissors = Rock | Paper | Scissors
  deriving stock (Show, Eq, Enum)

data LoseDrawWin = Lose | Draw | Win
  deriving stock (Show, Eq)

data XYZ = X | Y | Z
  deriving stock (Show, Eq)

day2parser :: Parser [(RockPaperScissors, XYZ)]
day2parser = line `endBy` newline where
  line = (,) <$> abc <* space <*> xyz
  abc = Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C'
  xyz = X <$ char 'X' <|> Y <$ char 'Y' <|> Z <$ char 'Z'

day2part1 :: [(RockPaperScissors, XYZ)] -> Int
day2part1 = totalScore . fmap (fmap toABC)
  where toABC X = Rock
        toABC Y = Paper
        toABC Z = Scissors

day2part2 :: [(RockPaperScissors, XYZ)] -> Int
day2part2 = totalScore . fmap ((=>> toABC) . fmap toLoseDrawWin)
  where toABC = uncurry reply

        toLoseDrawWin X = Lose
        toLoseDrawWin Y = Draw
        toLoseDrawWin Z = Win

reply :: RockPaperScissors -> LoseDrawWin -> RockPaperScissors
reply opp goal = head [ you | you <- [Rock, Paper, Scissors], play opp you == goal ]
  {-
reply opp Lose = toEnum do (fromEnum opp + 2) `rem` 3
reply opp Draw = opp
reply opp Win  = toEnum do (fromEnum opp + 1) `rem` 3
-}

totalScore :: [(RockPaperScissors,RockPaperScissors)] -> Int
totalScore = sum . map (uncurry roundScore)

roundScore :: RockPaperScissors -> RockPaperScissors -> Int
roundScore opp you = outcomeScore opp you + shapeScore you

play :: RockPaperScissors -> RockPaperScissors -> LoseDrawWin
play Rock Scissors = Lose
play Scissors Paper = Lose
play Paper Rock = Lose
play opp you
  | opp == you = Draw
  | otherwise = Win

outcomeScore :: RockPaperScissors -> RockPaperScissors -> Int
outcomeScore opp you = case play opp you of
  Lose -> 0
  Draw -> 3
  Win -> 6

shapeScore :: RockPaperScissors -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3
