module AdventOfCode.Day14 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec hiding ((<|>), between)
import Control.Applicative
import Data.Set qualified as Set
import Data.Map qualified as Map

newtype RockShape = RockShape [Point]
  deriving stock (Show, Eq)

data Point = Point { right :: Int, down :: Int } 
  deriving stock (Show, Eq, Ord)

{-
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
 -}
day14parser :: Parser [RockShape]
day14parser = rockShape `endBy` newline where
  rockShape = RockShape <$> point `sepBy` string " -> "
  point = Point <$> int <* char ',' <*> int
  int = read <$> some digit

data Cave = Cave
  { leftEdge :: Int
  , rightEdge :: Int
  , bottomEdge :: Int
  , caveContents :: Map.Map Point FillMaterial
  }
  deriving stock Eq

instance Show Cave where
  show Cave{leftEdge,rightEdge,bottomEdge,caveContents} = unlines do
    down <- [down source .. bottomEdge]
    pure do
      right <- [min leftEdge (right source) .. max rightEdge (right source)]
      let p = Point{down,right}
      pure case Map.lookup p caveContents of
        _ | p == source -> '+'
        Just RockFilled -> '#'
        Just SandFilled -> 'o'
        _ -> '.'

makeCave :: [RockShape] -> Cave
makeCave rockShapes = Cave
  { leftEdge = minimum do Set.map right rocks
  , rightEdge = maximum do Set.map right rocks
  , bottomEdge = maximum do Set.map down rocks
  , caveContents = Map.fromSet (const RockFilled) rocks
  }
  where
    rocks = Set.fromList do
      RockShape points <- rockShapes
      (start,end) <- zip points (tail points)
      if right start == right end
        then [ start { down } | down <- between (down start) (down end) ]
        else [ start { right } | right <- between (right start) (right end) ]

between :: Int -> Int -> [Int]
between a b = [min a b .. max a b]

data FillMaterial = AirFilled | RockFilled | SandFilled
  deriving stock (Eq, Show)

fillCave :: Cave -> Cave
fillCave cave@Cave{leftEdge,rightEdge,bottomEdge,caveContents} = cave { caveContents = snd (loop source caveContents) } where
  loop p@Point{down,right} state  = case check p state of
    Just m -> (m, state)
    Nothing -> fall p [p{down=down+1},p{down=down+1,right=right-1},p{down=down+1,right=right+1}] state

  check p state
    | outOfBounds p = Just AirFilled
    | otherwise = Map.lookup p state

  fall q (p:ps) (loop p -> (m,state)) = case m of
    AirFilled -> (AirFilled, Map.insert q AirFilled state)
    _   -> fall q ps state
  fall q [] state = (SandFilled, Map.insert q SandFilled state)

  outOfBounds Point{down,right} = right < leftEdge || right > rightEdge || down > bottomEdge

source :: Point
source = Point{right=500,down=0}

addFloor :: Cave -> Cave
addFloor Cave{leftEdge,rightEdge,bottomEdge,caveContents} = newCave where
  newCave = Cave
    { bottomEdge = newBottomEdge
    , leftEdge = newLeftEdge
    , rightEdge = newRightEdge
    , caveContents = newCaveContents
    }

  newBottomEdge = bottomEdge + 2
  newLeftEdge = min leftEdge (right source - newBottomEdge)
  newRightEdge = max rightEdge (right source + newBottomEdge)
  newCaveContents = caveContents `Map.union` Map.fromList
    [ (Point{right,down}, RockFilled) | let down = newBottomEdge, right <- [newLeftEdge .. newRightEdge] ]

day14part1 :: [RockShape] -> Int
day14part1 = Map.size . Map.filter (==SandFilled) . caveContents . fillCave . makeCave

day14part2 :: [RockShape] -> Int
day14part2 = Map.size . Map.filter (==SandFilled) . caveContents . fillCave . addFloor . makeCave

{-
day14part2 :: [RockShape] -> Cave
day14part2 = fillCave . addFloor . makeCave
-}
