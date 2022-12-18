{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module AdventOfCode.Y2022.Day18 where

import AdventOfCode.Y2022.Prelude
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Function ((&))

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = check parser part1 part2 Example
    { raw = [text|
        2,2,2
        1,2,2
        3,2,2
        2,1,2
        2,3,2
        2,2,1
        2,2,3
        2,2,4
        2,2,6
        1,2,5
        3,2,5
        2,1,5
        2,3,5
      |] <> "\n"
    , parsed =
        [ Cube 2 2 2
        , Cube 1 2 2
        , Cube 3 2 2
        , Cube 2 1 2
        , Cube 2 3 2
        , Cube 2 2 1
        , Cube 2 2 3
        , Cube 2 2 4
        , Cube 2 2 6
        , Cube 1 2 5
        , Cube 3 2 5
        , Cube 2 1 5
        , Cube 2 3 5
        ]
    , part1output = 64
    , part2output = 58
    }
  }

type Cube :: Type
data Cube = Cube { x :: Int, y :: Int, z :: Int }
  deriving stock (Eq, Show, Ord)

parser :: Parser [Cube]
parser = cube `endBy` newline where
  cube = Cube <$> int <* comma <*> int <* comma <*> int
  int = read <$> some digit
  comma = char ','

type Side :: Type
data Side = X | Y | Z
  deriving stock (Eq, Show, Ord)

part1 :: [Cube] -> Int
part1 = Map.size . Map.filter (== 1) . Map.fromListWith (+) . (`zip` repeat (1 :: Int)) . concatMap sides

sides :: Cube -> [(Side,Cube)]
sides cube@Cube{x,y,z} =
  [ (X, cube)
  , (X, cube { x = x + 1 })
  , (Y, cube)
  , (Y, cube { y = y + 1 })
  , (Z, cube)
  , (Z, cube { z = z + 1 })
  ]

part2 :: [Cube] -> Int
part2 = length . search . Set.fromList

search :: Set.Set Cube -> [(Side, Cube)]
search cubes = concat found where
  ~(found, queue) = loop Set.empty 1 (Cube minX minY minZ:concat queue)

  minX = minimum (Set.map x cubes) - 1
  minY = minimum (Set.map y cubes) - 1
  minZ = minimum (Set.map z cubes) - 1

  maxX = maximum (Set.map x cubes) + 1
  maxY = maximum (Set.map y cubes) + 1
  maxZ = maximum (Set.map z cubes) + 1

  loop :: Set.Set Cube -> Int -> [Cube] -> ([[(Side, Cube)]], [[Cube]])
  loop _searched 0 _queue = ([], [])
  loop searched n ~(cube@Cube{x,y,z}:next) = 
    let newSides = concat
          [ [ (X, cube') | let cube' = cube { x = x - 1 }, cube' `Set.member` cubes ]
          , [ (X, cube)  | let cube' = cube { x = x + 1 }, cube' `Set.member` cubes ]
          , [ (Y, cube') | let cube' = cube { y = y - 1 }, cube' `Set.member` cubes ]
          , [ (Y, cube)  | let cube' = cube { y = y + 1 }, cube' `Set.member` cubes ]
          , [ (Z, cube') | let cube' = cube { z = z - 1 }, cube' `Set.member` cubes ]
          , [ (Z, cube)  | let cube' = cube { z = z + 1 }, cube' `Set.member` cubes ]
          ]
        adjacent = concat
          [ [ cube' | let cube' = cube { x = x - 1 }, minX < x ]
          , [ cube' | let cube' = cube { x = x + 1 }, x + 1 <= maxX ]
          , [ cube' | let cube' = cube { y = y - 1 }, minY <= y ]
          , [ cube' | let cube' = cube { y = y + 1 }, y + 1 <= maxY ]
          , [ cube' | let cube' = cube { z = z - 1 }, minZ < z ]
          , [ cube' | let cube' = cube { z = z + 1 }, z + 1 <= maxZ ]
          ]
        unsearched = adjacent & filter \cube' ->
          not (Set.member cube' cubes) && not (Set.member cube' searched)

        ~(moreSides, later) = loop (Set.fromList unsearched `Set.union` searched) (n - 1 + length unsearched) next
    in 
    (newSides : moreSides, unsearched : later)
