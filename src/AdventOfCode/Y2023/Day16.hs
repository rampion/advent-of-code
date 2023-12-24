{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-uni-patterns #-}

module AdventOfCode.Y2023.Day16 where

import Control.Arrow (first, second, (***))
import Control.Monad.Trans.State (execState, gets, state, modify, type State)
import Data.Maybe (listToMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set
import AdventOfCode.Y2023.Prelude hiding (pattern Empty, type State)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            .|...\....
            |.-.\.....
            .....|-...
            ........|.
            ..........
            .........\
            ..../.\\..
            .-.-/..|..
            .|....-|.\
            ..//.|....
          |]
        , parsed =
            [ [ Empty, VerticalSplitter, Empty, Empty, Empty, BackwardMirror, Empty, Empty, Empty, Empty ]
            , [ VerticalSplitter, Empty, HorizontalSplitter, Empty, BackwardMirror, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, VerticalSplitter, HorizontalSplitter, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, VerticalSplitter, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, BackwardMirror ]
            , [ Empty, Empty, Empty, Empty, ForwardMirror, Empty, BackwardMirror, BackwardMirror, Empty, Empty ]
            , [ Empty, HorizontalSplitter, Empty, HorizontalSplitter, ForwardMirror, Empty, Empty, VerticalSplitter, Empty, Empty ]
            , [ Empty, VerticalSplitter, Empty, Empty, Empty, Empty, HorizontalSplitter, VerticalSplitter, Empty, BackwardMirror ]
            , [ Empty, Empty, ForwardMirror, ForwardMirror, Empty, VerticalSplitter, Empty, Empty, Empty, Empty ]
            ]
        , part1output = 46
        , part2output = 51
        }
  }

type Input :: Type
type Input = [[Part]]

type Part :: Type
data Part
  = Empty
  | HorizontalSplitter
  | VerticalSplitter
  | ForwardMirror
  | BackwardMirror
  deriving stock (Show, Eq)

type Port :: Type
data Port
  = West
  | North
  | East
  | South
  deriving stock (Show, Eq, Ord)

parser :: Parser Input
parser = some part `endBy` newline where
  part = asum
    [ char '.' $> Empty
    , char '-' $> HorizontalSplitter
    , char '|' $> VerticalSplitter
    , char '/' $> ForwardMirror
    , char '\\' $> BackwardMirror
    ]

part1 :: Input -> Int
part1 = (Map.! ((0,0),East)) . numReachable

part2 :: Input -> Int
part2 = maximum . numReachable

type Pose :: Type
type Pose = (Position, Port)

type Position :: Type
type Position = (Int, Int)

network :: Input -> LMap Pose [Pose]
network rows = Map.fromList do
  let numRows = length rows
  (y, columns) <- zip [0..] rows
  let numColumns = length columns
  (x, part) <- zip [0..] columns
  let here = (y,x)
      north = [ ((y - 1, x), North) | y > 0 ]
      south = [ ((y + 1, x), South) | y + 1 < numRows ]
      east  = [ ((y, x + 1), East)  | x + 1 < numColumns ]
      west  = [ ((y, x - 1), West)  | x > 0 ]
  id
    [ ( (here, North)
      , case part of
          HorizontalSplitter  -> east <> west
          ForwardMirror       -> east
          BackwardMirror      -> west
          _                   -> north
      )
    , ( (here, South)
      , case part of
          HorizontalSplitter  -> east <> west
          ForwardMirror       -> west
          BackwardMirror      -> east
          _                   -> south
      )
    , ( (here, East)
      , case part of
          VerticalSplitter    -> north <> south
          ForwardMirror       -> north
          BackwardMirror      -> south
          _                   -> east
      )
    , ( (here, West)
      , case part of
          VerticalSplitter    -> north <> south
          ForwardMirror       -> south
          BackwardMirror      -> north
          _                   -> west
      )
    ]

score :: Ord a => Set.Set (a, b) -> Int
score = Set.size . Set.map fst

numReachable :: Input -> LMap Pose Int
numReachable = Map.map score . (search <*> edges) . network

edges :: LMap Pose [Pose] -> [Pose]
edges net = concat
    [ [ ((y,x), d) | y <- [0..maxY], (x,d) <- [(0,East),(maxX,West)] ] 
    , [ ((y,x), d) | (y,d) <- [(0,South),(maxY,North)], x <- [0..maxX] ]
    ]
  where ((maxY,maxX),_) = maximum (Map.keysSet net)

search :: (Show a, Ord a) => LMap a [a] -> [a] -> LMap a (Set.Set a)
search adj seed = fst 
  do dfs adj `traverse` seed `execState` (Map.empty, Map.empty)
  `Map.restrictKeys` Set.fromList seed

dfs :: (Show a, Ord a) => LMap a [a] -> a -> State (LMap a (Set a), LMap a (Set a)) (Set a)
dfs adj a = gets (Map.lookup a *** Map.member a) >>= \case
  (Just cached, _) -> pure cached
  (_, True) -> pure (Set.singleton a)
  _ -> do
    modify do second (Map.insert a do Set.singleton a)
    found <- Set.insert a . Set.unions <$> traverse (dfs adj) (adj Map.! a)
    pending <- state \(cache, path) -> (path Map.! a, (cache, Map.delete a path))

    modify do first do Map.union do Map.fromSet (const found) pending

    gets (listToMaybe . Set.toList . Set.intersection found . Map.keysSet . snd) >>= \case
      Just ancestor -> modify do second do Map.insertWith (<>) ancestor pending
      Nothing -> pure ()

    pure found
