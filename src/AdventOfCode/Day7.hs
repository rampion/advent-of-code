module AdventOfCode.Day7 where

import Prelude
import AdventOfCode.Solver
import Data.Foldable (asum)
import Control.Applicative (some)

import Data.String (IsString)
import Text.Parsec
import Data.Map.Lazy qualified as Map
import Data.List qualified as List
import Data.Functor ((<&>))

data CommandLog = CD Path | LS [Listing]
  deriving stock (Show, Eq)

data Path = Outermost | In DirName | Out
  deriving stock (Show, Eq)

data Listing = Dir DirName | File Int FileName
  deriving stock (Show, Eq)

newtype DirName = DirName String
  deriving newtype (Show, Eq, Ord, IsString)

newtype FileName = FileName String
  deriving newtype (Show, Eq, Ord, IsString)

type DirPath = [DirName]

day7parser :: Parser [CommandLog]
day7parser = many commandLog where
  commandLog = string "$ " *> (cd <|> ls)
  cd = CD <$ string "cd " <*> path <* newline
  path = asum
    [ Outermost <$ char '/'
    , Out <$ string ".."
    , In <$> dirName
    ]
  ls = LS <$ string "ls" <* newline <*> (listing `endBy` newline)
  listing = dir <|> file
  dir = Dir <$ string "dir " <*> dirName
  file = File <$> int <* char ' ' <*> fileName
  dirName = DirName <$> some (noneOf "\n")
  fileName = FileName <$> some (noneOf "\n")
  int = read <$> some digit

dirSizes :: [CommandLog] -> Map.Map DirPath Int
dirSizes
  = loeb
  . Map.mapWithKey
      do \cwd ls m -> sum
            do ls <&> \case
                  Dir d -> m Map.! (d : cwd)
                  File n _ -> n
  . snd
  . List.foldl'
      do \(cwd, tree) -> \case
            CD Outermost -> ([], tree)
            CD (In name) -> (name : cwd, tree)
            CD Out -> (tail cwd, tree)
            LS ls -> (cwd, Map.insert cwd ls tree)
      do (error "unknown", Map.empty)

loeb :: Functor f => f (f a -> a) -> f a
loeb formulae = values where
  values = fmap ($values) formulae

day7part1 :: [CommandLog] -> Int
day7part1
  = sum
  . Map.filter (<= 100000)
  . dirSizes

day7part2 :: [CommandLog] -> Int
day7part2 input = freedSize where
  sizes = dirSizes input
  totalSize = 70_000_000
  availSize = 30_000_000
  maxSize = totalSize - availSize
  currSize = sizes Map.! []
  excess = currSize - maxSize
  freedSize = minimum do Map.filter (>= excess) sizes
