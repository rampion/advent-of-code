module AdventOfCode.Y2022.Day2 where

import AdventOfCode.Y2022.Prelude
import Control.Comonad

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = check parser part1 part2 Example
        { raw =
            [text|
              A Y
              B X
              C Z
            |] <> "\n"
        , parsed =
            [ (Rock, Y)
            , (Paper, X)
            , (Scissors, Z)
            ]
        , part1output = 15
        , part2output = 12
        }
    }

type RockPaperScissors :: Type
data RockPaperScissors = Rock | Paper | Scissors
  deriving stock (Show, Eq, Enum)

type LoseDrawWin :: Type
data LoseDrawWin = Lose | Draw | Win
  deriving stock (Show, Eq)

type XYZ :: Type
data XYZ = X | Y | Z
  deriving stock (Show, Eq)

parser :: Parser [(RockPaperScissors, XYZ)]
parser = line `endBy` newline
  where
    line = (,) <$> abc <* space <*> xyz
    abc = Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C'
    xyz = X <$ char 'X' <|> Y <$ char 'Y' <|> Z <$ char 'Z'

part1 :: [(RockPaperScissors, XYZ)] -> Int
part1 = totalScore . fmap (fmap toABC)
  where
    toABC X = Rock
    toABC Y = Paper
    toABC Z = Scissors

part2 :: [(RockPaperScissors, XYZ)] -> Int
part2 = totalScore . fmap ((=>> toABC) . fmap toLoseDrawWin)
  where
    toABC = uncurry reply

    toLoseDrawWin X = Lose
    toLoseDrawWin Y = Draw
    toLoseDrawWin Z = Win

reply :: RockPaperScissors -> LoseDrawWin -> RockPaperScissors
reply opp goal = head [you | you <- [Rock, Paper, Scissors], play opp you == goal]

{-
reply opp Lose = toEnum do (fromEnum opp + 2) `rem` 3
reply opp Draw = opp
reply opp Win  = toEnum do (fromEnum opp + 1) `rem` 3
-}

totalScore :: [(RockPaperScissors, RockPaperScissors)] -> Int
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
