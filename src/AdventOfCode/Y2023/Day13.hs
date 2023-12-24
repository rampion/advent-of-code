{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day13 where

import AdventOfCode.Y2023.Prelude
import Data.List (transpose, foldl')
import Data.Bits (xor, popCount)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            #.##..##.
            ..#.##.#.
            ##......#
            ##......#
            ..#.##.#.
            ..##..##.
            #.#.##.#.
            
            #...##..#
            #....#..#
            ..##..###
            #####.##.
            #####.##.
            ..##..###
            #....#..#
          |]
        , parsed =
            [ [ [ Rock, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Ash ]
              , [ Ash, Ash, Rock, Ash, Rock, Rock, Ash, Rock, Ash ]
              , [ Rock, Rock, Ash, Ash, Ash, Ash, Ash, Ash, Rock ]
              , [ Rock, Rock, Ash, Ash, Ash, Ash, Ash, Ash, Rock ]
              , [ Ash, Ash, Rock, Ash, Rock, Rock, Ash, Rock, Ash ]
              , [ Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Ash ]
              , [ Rock, Ash, Rock, Ash, Rock, Rock, Ash, Rock, Ash ]
              ]
            , [ [ Rock, Ash, Ash, Ash, Rock, Rock, Ash, Ash, Rock ]
              , [ Rock, Ash, Ash, Ash, Ash, Rock, Ash, Ash, Rock ]
              , [ Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Rock ]
              , [ Rock, Rock, Rock, Rock, Rock, Ash, Rock, Rock, Ash ]
              , [ Rock, Rock, Rock, Rock, Rock, Ash, Rock, Rock, Ash ]
              , [ Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Rock ]
              , [ Rock, Ash, Ash, Ash, Ash, Rock, Ash, Ash, Rock ]
              ]
            ]
        , part1output = Just 405
        , part2output = Just 400
        }
  }

type Input :: Type
type Input = [Pattern]

type Pattern :: Type
type Pattern = [[Tile]]

type Tile :: Type
data Tile = Ash | Rock
  deriving stock (Show, Eq)

parser :: Parser Input
parser = pattern_ `sepBy` newline where
  pattern_ = some tile `endBy` newline
  tile = asum
    [ char '.' $> Ash
    , char '#' $> Rock
    ]

type Reflection :: Type
data Reflection = BeforeColumn Int | BeforeRow Int

findReflection :: Pattern -> [(Int, Reflection)]
findReflection = \pat -> asum 
  [ reflect BeforeRow pat
  , reflect BeforeColumn (transpose pat)
  ]
  where
    reflect tag = loop tag 0 [] . map pack

    pack :: [Tile] -> Int
    pack = foldl' (\n t -> 2*n + case t of Ash -> 0; Rock -> 1) 0

    loop _ _ _ [] = []
    loop tag !c [] (a:as) = loop tag (c + 1) [a] as
    loop tag c bs as@(a:at) = 
      (dist bs as, tag c) : loop tag (c + 1) (a:bs) at

    dist bs as = sum (zipWith (\a -> popCount . xor a) as bs)

part1 :: Input -> Maybe Int
part1 = fmap sum . traverse (fmap score . lookup 0 . findReflection)

score :: Reflection -> Int
score = \case
  BeforeColumn col -> col
  BeforeRow row -> 100 * row

part2 :: Input -> Maybe Int
part2 = fmap sum . traverse (fmap score . lookup 1 . findReflection)
