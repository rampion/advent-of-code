{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day15 where

import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import AdventOfCode.Y2023.Prelude
import Data.Map.Strict qualified as Map

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
          |]
        , parsed =
            [ Step {raw="rn=1", label="rn", box=0, action=FocalLength 1}
            , Step {raw="cm-", label="cm", box=0, action=Remove}
            , Step {raw="qp=3", label="qp", box=1, action=FocalLength 3}
            , Step {raw="cm=2", label="cm", box=0, action=FocalLength 2}
            , Step {raw="qp-", label="qp", box=1, action=Remove}
            , Step {raw="pc=4", label="pc", box=3, action=FocalLength 4}
            , Step {raw="ot=9", label="ot", box=3, action=FocalLength 9}
            , Step {raw="ab=5", label="ab", box=3, action=FocalLength 5}
            , Step {raw="pc-", label="pc", box=3, action=Remove}
            , Step {raw="pc=6", label="pc", box=3,  action=FocalLength 6}
            , Step {raw="ot=7", label="ot", box=3, action=FocalLength 7}
            ]
        , part1output = 1320
        , part2output = 145
        }
  }

type Input :: Type
type Input = [Step]

parser :: Parser Input
parser = (step `sepBy` comma) <* newline where
  step = do
    label <- some (oneOf ['a' .. 'z'])
    let box = hash label
    (action, raw) <- (<|>)
      do 
          c <- char '='
          ds <- some digit
          pure (FocalLength (read ds), label <> (c:ds))
      do
          c <- char '-'
          pure (Remove, label <> [c])
    pure Step{..}
  comma = char ','

type Step :: Type
data Step = Step
  { raw :: String
  , label :: String
  , box :: Int
  , action :: Action
  }
  deriving stock (Eq, Show)

type Action :: Type
data Action
  = FocalLength Int
  | Remove
  deriving stock (Eq, Show)

part1 :: Input -> Int
part1 = sum . map (hash . raw)

hash :: String -> Int
hash = foldl' (\h c -> (17 * (h + fromEnum c)) `rem` 256) 0

part2 :: Input -> Int
part2 = cleanup . foldl' follow Map.empty . zip @Int [0..] where
  follow m (ord, Step{..}) = case action of
    Remove -> Map.adjust (Map.delete label) box m
    FocalLength len -> Map.insertWith (Map.unionWith unify)  box (Map.singleton label (ord, len)) m

  unify (_newOrd,newLen) (oldOrd,_oldLen) = (oldOrd,newLen)

  cleanup m = sum do
    (box, lenses) <- Map.toList m
    (slot, len) <- zip [1..] . map snd . sortBy (comparing fst) $ Map.elems lenses
    pure do (box + 1) * slot * len
