{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
module AdventOfCode.Day12 where

import Prelude
import AdventOfCode.Solver
import Data.Vector qualified as Vector
import Text.Parsec hiding ((<|>))
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.List (foldl')
import Control.Monad (guard)

data HeightMap = HeightMap
  { currentPosition :: Position
  , bestSignal :: Position
  , elevations :: ElevationGrid Int
  }
  deriving stock (Show, Eq)

newtype ElevationGrid a = ElevationGrid { fromElevationGrid :: Vector.Vector (Vector.Vector a) }
  deriving newtype (Show, Eq)
  deriving stock (Functor, Foldable)

data Position = Position
  { column :: Int
  , row :: Int
  }
  deriving stock (Show, Eq)

day12parser :: Parser HeightMap
day12parser = do
  let normalize = \case
        'S' -> 0
        'E' -> 25
        c -> fromEnum c - fromEnum 'a'

  rows <- many1 (oneOf ('S':'E':['a'..'z'])) `endBy` newline

  let tagged = concat do 
        zipWith zip rows [[Position{column,row} | column <- [0..]] | row <- [0..]] 

  currentPosition <- lookup 'S' tagged & maybe
    do fail "missing start position"
    do pure

  bestSignal <- lookup 'E' tagged & maybe
    do fail "missing end position"
    do pure

  let elevations = ElevationGrid do
        Vector.fromList do fmap (Vector.fromList . fmap normalize) rows

  pure HeightMap{currentPosition,bestSignal,elevations}

(!) :: ElevationGrid a -> Position -> a
(!) (ElevationGrid v) Position{column,row} = v Vector.! row Vector.! column

(//) :: ElevationGrid a -> [(Position,a)] -> ElevationGrid a
(//) = \(ElevationGrid rows) -> ElevationGrid . foldl' update rows where
  update rows (Position{column,row},new) = 
    rows Vector.// [(row, (rows Vector.! row) Vector.// [(column, new)])]

width :: ElevationGrid a -> Int
width = Vector.length . Vector.head . fromElevationGrid

height :: ElevationGrid a -> Int
height = Vector.length . fromElevationGrid

day12part1 :: HeightMap -> Maybe Int
day12part1 HeightMap{currentPosition,elevations,bestSignal} = distances currentPosition elevations ! bestSignal

distances :: Position -> ElevationGrid Int -> ElevationGrid (Maybe Int)
distances start = last . search start

search :: Position -> ElevationGrid Int -> [ElevationGrid (Maybe Int)]
search start elevations = grids where
  maxColumn = width elevations - 1
  maxRow = height elevations - 1
  initial = (Nothing <$ elevations) // [(start, Just 0)]

  ~(queue, grids) = loop 1 (start:queue) initial

  loop 0 _ distance = ([], [distance])
  loop n (~(p:ps)) distance =
    let adj = concat
          [ [p { column = column'} | let column' = column p + 1, column' <= maxColumn]
          , [p { column = column'} | let column' = column p - 1, column' >= 0]
          , [p { row = row'} | let row' = row p + 1, row' <= maxRow]
          , [p { row = row'} | let row' = row p - 1, row' >= 0]
          ]
        maxElevation = 1 + elevations ! p
        new = [q | q <- adj, elevations ! q <= maxElevation, isNothing (distance ! q)]
    in 
    case distance ! p of
      Nothing -> error do "unknown distance for " ++ show p
      Just d -> bilazily (new ++) (distance:) do
                  loop (n - 1 + length new) ps (distance // map (, Just (d + 1)) new)

bilazily :: (x -> y) -> (a -> b) -> (x, a) -> (y, b)
bilazily f g ~(x,a) = (f x, g a)

day12part2 :: HeightMap -> Int
day12part2 HeightMap{elevations,bestSignal} = minimum do
  let ElevationGrid rows = distances bestSignal (fmap (25-) elevations)
  (row, columns) <- zip [0..] do Vector.toList rows
  (column, Just d) <- zip [0..] do Vector.toList columns
  guard do elevations ! Position{row,column} == 0
  pure d
