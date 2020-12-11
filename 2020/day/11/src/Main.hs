{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Control.Monad (filterM, mapM_)
import Control.Monad.State.Strict (State, execState, gets, modify)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = (getArgs >>=) . mapM_ $ \filename -> do
  putStrLn filename

  grid <- parseGrid <$> readFile filename

  putStrLn "\tpart 1: number of filled seats in stable seating configuration"
  putStrLn $ "\t" ++ show (part1 grid)

  putStrLn "\tpart 2: number of filled seats in revised stable seating configuration"
  putStrLn $ "\t" ++ show (part2 grid)

type Grid = Map Position Tile

type Position = (Int, Int)

data Tile = Floor | Unstable | Empty | Full
  deriving (Eq)

parseGrid :: String -> Grid
parseGrid src = Map.fromList $ do
  (y, line) <- zip [0 ..] $ lines src
  (x, char) <- zip [0 ..] line
  return
    ( (x, y),
      case char of
        '.' -> Floor
        'L' -> Unstable
        _ -> error $ "unexpected seating layout symbol " ++ show char ++ ", expected 'L' or '.'"
    )

part1 :: Grid -> Int
part1 = countFull . stabilize adjacent 4

part2 :: Grid -> Int
part2 = countFull . stabilize visible 5

countFull :: Grid -> Int
countFull = length . filter (== Full) . Map.elems

stabilize :: (Position -> Grid -> Grid) -> Int -> Grid -> Grid
stabilize neighbors threshold = execState . loop =<< id
  where
    loop :: Grid -> State Grid ()
    loop (Map.keys . Map.filter (== Unstable) -> candidates) = do
      isFillableNow <- gets (flip isFillable)
      case filterM isFillableNow candidates of
        (_, []) -> return ()
        (Set.toList -> flagged, fillable) -> do
          setAll fillable Full
          unfillable <- filterM (gets . isUnstable) flagged
          setAll unfillable Empty
          loop . Map.unions =<< traverse (gets . neighbors) unfillable

    setAll :: [Position] -> Tile -> State Grid ()
    setAll ps tile = modify . Map.union $ Map.fromList [(p, tile) | p <- ps]

    isFillable :: Position -> Grid -> (Set Position, Bool)
    isFillable p grid = do
      let chairs = Map.keysSet . Map.filter (`elem` [Unstable, Full]) $ neighbors p grid
      if Set.size chairs >= threshold
        then (Set.empty, False)
        else (chairs, True)

    isUnstable :: Position -> Grid -> Bool
    isUnstable p grid = Just Unstable == Map.lookup p grid

adjacent :: Position -> Grid -> Grid
adjacent (x, y) grid = Map.fromList $ do
  (dx, dy) <- deltas
  let p = (x + dx, y + dy)
  case Map.lookup p grid of
    Nothing -> []
    Just tile -> return (p, tile)

visible :: Position -> Grid -> Grid
visible (x, y) grid = Map.fromList $ do
  (dx, dy) <- deltas
  let loop p@(x, y) = case Map.lookup p grid of
        Nothing -> []
        Just Floor -> loop (x + dx, y + dy)
        Just tile -> return (p, tile)
  loop (x + dx, y + dy)

deltas :: [Position]
deltas =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]
