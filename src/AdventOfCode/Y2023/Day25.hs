{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day25 where

import AdventOfCode.Y2023.Prelude

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
          |]
        , parsed
        , part1output = ()
        , part2output = ()
        }
  }

parsed :: Input
parsed = Input

type Input :: Type
data Input = Input
  deriving stock (Show, Eq)

parser :: Parser Input
parser = pure Input

part1 :: Input -> ()
part1 _ = ()

part2 :: Input -> ()
part2 _ = ()
