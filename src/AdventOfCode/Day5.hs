{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Day5 where

import AdventOfCode.Solver
import Control.Applicative (some)
import Data.IntMap qualified as IntMap
import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import Text.Parsec
import Prelude

data Day5Input = Day5Input
  { crateStacks :: IntMap.IntMap String
  , procedure :: [Step]
  }
  deriving stock (Eq, Show)

data Step = Step
  { move :: Int
  , from :: Int
  , to :: Int
  }
  deriving stock (Eq, Show)

day5parser :: Parser Day5Input
day5parser = Day5Input <$> crateStacks <* newline <*> procedure
  where
    crateStacks = toMap <$> stacks <*> labels
    toMap stacks labels = IntMap.fromList (zip labels stacks)

    stacks = fmap catMaybes . transpose <$> row `endBy` newline
    row = crateOrBlank `sepBy` space
    crateOrBlank = Just <$> crate <|> Nothing <$ blank
    crate = char '[' *> oneOf ['A' .. 'Z'] <* char ']'
    blank = try (string "   ")

    labels = (space *> int <* space) `sepBy` space <* newline

    procedure = step `endBy` newline
    step = Step <$ string "move " <*> int <* string " from " <*> int <* string " to " <*> int

    int = read <$> some digit
    space = char ' '

day5part1 :: Day5Input -> String
day5part1 = \Day5Input {crateStacks, procedure} -> map head do IntMap.elems do foldl' step crateStacks procedure
  where
    step crateStacks Step {move, from, to} =
      let (moved, remains) = splitAt move (crateStacks IntMap.! from)
       in IntMap.adjust (reverse moved <>) to do IntMap.insert from remains crateStacks

day5part2 :: Day5Input -> String
day5part2 = \Day5Input {crateStacks, procedure} -> map head do IntMap.elems do foldl' step crateStacks procedure
  where
    step crateStacks Step {move, from, to} =
      let (moved, remains) = splitAt move (crateStacks IntMap.! from)
       in IntMap.adjust (moved <>) to do IntMap.insert from remains crateStacks
