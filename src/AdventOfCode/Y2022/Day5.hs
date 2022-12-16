{-# OPTIONS_GHC -Wno-name-shadowing #-}
module AdventOfCode.Y2022.Day5 where

import AdventOfCode.Y2022.Prelude
import Data.IntMap qualified as IntMap
import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Function ((&))

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = check parser part1 part2 Example
    { raw = [text|
      |     [D]    
      | [N] [C]    
      | [Z] [M] [P]
      |  1   2   3 
      |
      | move 1 from 2 to 1
      | move 3 from 1 to 3
      | move 2 from 2 to 1
      | move 1 from 1 to 2
      |] & Text.unlines . map (Text.drop 2) . Text.lines
    , parsed = Day5Input
        { crateStacks = IntMap.fromList [(1, "NZ"), (2, "DCM"), (3, "P")]
        , procedure =
            [ Step {move = 1, from = 2, to = 1}
            , Step {move = 3, from = 1, to = 3}
            , Step {move = 2, from = 2, to = 1}
            , Step {move = 1, from = 1, to = 2}
            ]
        }
    , part1output = "CMZ"
    , part2output = "MCD"
    }
  }

type Day5Input :: Type
data Day5Input = Day5Input
  { crateStacks :: IntMap.IntMap String
  , procedure :: [Step]
  }
  deriving stock (Eq, Show)

type Step :: Type
data Step = Step
  { move :: Int
  , from :: Int
  , to :: Int
  }
  deriving stock (Eq, Show)

parser :: Parser Day5Input
parser = Day5Input <$> crateStacks <* newline <*> procedure
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

part1 :: Day5Input -> String
part1 = \Day5Input {crateStacks, procedure} -> map head do IntMap.elems do foldl' step crateStacks procedure
  where
    step crateStacks Step {move, from, to} =
      let (moved, remains) = splitAt move (crateStacks IntMap.! from)
       in IntMap.adjust (reverse moved <>) to do IntMap.insert from remains crateStacks

part2 :: Day5Input -> String
part2 = \Day5Input {crateStacks, procedure} -> map head do IntMap.elems do foldl' step crateStacks procedure
  where
    step crateStacks Step {move, from, to} =
      let (moved, remains) = splitAt move (crateStacks IntMap.! from)
       in IntMap.adjust (moved <>) to do IntMap.insert from remains crateStacks
