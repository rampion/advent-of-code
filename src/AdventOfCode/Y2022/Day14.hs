module AdventOfCode.Y2022.Day14 where

import AdventOfCode.Y2022.Prelude hiding (between)
import Data.Set qualified as Set
import Data.Map qualified as Map

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = tellSpec do
      let exampleInput = 
            [ RockShape [ Point{right=498,down=4}, Point{right=498,down=6}, Point{right=496,down=6} ]
            , RockShape [ Point{right=503,down=4}, Point{right=502,down=4}, Point{right=502,down=9}, Point{right=494,down=9} ]
            ]

      runCheck parser part1 part2 Example
        { raw = [text|
            498,4 -> 498,6 -> 496,6
            503,4 -> 502,4 -> 502,9 -> 494,9
          |]
        , parsed = exampleInput
        , part1output = 24
        , part2output = 93
        }

      it "can load the empty cave" do
        makeCave exampleInput `shouldBe` Cave
          { leftEdge = 494
          , rightEdge = 503
          , bottomEdge = 9
          , caveContents = Map.fromList
              [ ( Point 498 4, RockFilled )
              , ( Point 498 5, RockFilled )
              , ( Point 498 6, RockFilled )
              , ( Point 497 6, RockFilled )
              , ( Point 496 6, RockFilled )
              , ( Point 503 4, RockFilled )
              , ( Point 502 4, RockFilled )
              , ( Point 502 5, RockFilled )
              , ( Point 502 6, RockFilled )
              , ( Point 502 7, RockFilled )
              , ( Point 502 8, RockFilled )
              , ( Point 502 9, RockFilled )
              , ( Point 501 9, RockFilled )
              , ( Point 500 9, RockFilled )
              , ( Point 499 9, RockFilled )
              , ( Point 498 9, RockFilled )
              , ( Point 497 9, RockFilled )
              , ( Point 496 9, RockFilled )
              , ( Point 495 9, RockFilled )
              , ( Point 494 9, RockFilled )
              ]
          }
  }

type RockShape :: Type
newtype RockShape = RockShape [Point]
  deriving stock (Show, Eq)

type Point :: Type
data Point = Point { right :: Int, down :: Int } 
  deriving stock (Show, Eq, Ord)

{-
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
 -}
parser :: Parser [RockShape]
parser = rockShape `endBy` newline where
  rockShape = RockShape <$> point `sepBy` string " -> "
  point = Point <$> int <* char ',' <*> int
  int = read <$> some digit

type Cave :: Type
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

type FillMaterial :: Type
data FillMaterial = AirFilled | RockFilled | SandFilled
  deriving stock (Eq, Show)

fillCave :: Cave -> Cave
fillCave cave@Cave{leftEdge,rightEdge,bottomEdge,caveContents} = cave { caveContents = snd (loop source caveContents) } where
  loop p@Point{down,right} state  = case fill p state of
    Just m -> (m, state)
    Nothing -> fall p [p{down=down + 1},p{down=down + 1,right=right - 1},p{down=down + 1,right=right + 1}] state

  fill p state
    | outOfBounds p = Just AirFilled
    | otherwise = Map.lookup p state

  fall q (p:ps) (loop p -> (m,state)) = case m of
    AirFilled -> (AirFilled, Map.insert q AirFilled state)
    _   -> fall q ps state
  fall q [] state = (SandFilled, Map.insert q SandFilled state)

  outOfBounds Point{down,right} = right < leftEdge || right > rightEdge || down > bottomEdge

-- The sand is pouring into the cave from point 500,0.
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

part1 :: [RockShape] -> Int
part1 = Map.size . Map.filter (== SandFilled) . caveContents . fillCave . makeCave

part2 :: [RockShape] -> Int
part2 = Map.size . Map.filter (== SandFilled) . caveContents . fillCave . addFloor . makeCave

{-
part2 :: [RockShape] -> Cave
part2 = fillCave . addFloor . makeCave
-}
