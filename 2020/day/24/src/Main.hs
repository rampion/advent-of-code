{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    tiles <- readTiles filename

    putStrLn "\tpart 1: how many tiles are left with their black side facing up"
    let actual1 = part1 tiles
    expected1 <- readExpected (filename ++ ".part1")
    putStrLn $ "\t" ++ check actual1 expected1 ++ " " ++ show actual1

    putStrLn "\tpart 1: how many tiles are left with their black side facing up after 100 days"
    let actual2 = part2 tiles
    expected2 <- readExpected (filename ++ ".part2")
    putStrLn $ "\t" ++ check actual2 expected2 ++ " " ++ show actual2

readTiles :: FilePath -> IO [Tile]
readTiles = fmap (map parseTile . lines) . readFile
  where
    parseTile :: String -> Tile
    parseTile [] = []
    parseTile ('e' : cs) = E : parseTile cs
    parseTile ('s' : 'e' : cs) = SE : parseTile cs
    parseTile ('s' : 'w' : cs) = SW : parseTile cs
    parseTile ('w' : cs) = W : parseTile cs
    parseTile ('n' : 'w' : cs) = NW : parseTile cs
    parseTile ('n' : 'e' : cs) = NE : parseTile cs
    parseTile cs = error $ "invalid tile instructions: " ++ show cs

readExpected :: Read a => FilePath -> IO (Maybe a)
readExpected filename =
  (Just . read . head . words <$> readFile filename) <|> return Nothing

type Tile = [Direction]

data Direction = E | SE | SW | W | NW | NE
  deriving (Eq)

check :: Eq a => a -> Maybe a -> String
check actual = \case
  Just expected | expected == actual -> ansi green "✓"
  Nothing -> ansi yellow "?"
  _ -> ansi red "✗"
  where
    ansi :: Int -> String -> String
    ansi color text = "\x1b[" ++ show color ++ "m" ++ text ++ "\x1b[m"

    red, yellow, green :: Int
    red = 31
    yellow = 33
    green = 32

part1 :: [Tile] -> Int
part1 = Set.size . initialize

type Floor = Set.Set (Int, Int)

initialize :: [Tile] -> Floor
initialize = List.foldl' toggle Set.empty . map canonical
  where
    canonical :: Tile -> (Int, Int)
    canonical = List.foldl' move (0, 0)

    move (!ne, !e) = \case
      E -> (ne, e + 1)
      SE -> (ne - 1, e + 1)
      SW -> (ne - 1, e)
      W -> (ne, e - 1)
      NW -> (ne + 1, e - 1)
      NE -> (ne + 1, e)

    toggle black coord
      | Set.member coord black = Set.delete coord black
      | otherwise = Set.insert coord black

part2 :: [Tile] -> Int
part2 = Set.size . (!! 100) . iterate step . initialize
  where
    step current =
      let keep = Set.filter (borders current 1) current
          create = Set.filter (borders current 2) . Set.unions $ Set.map neighbors current
       in Set.union keep create

    borders black count = (== count) . Set.size . (`Set.intersection` black) . neighbors

    neighbors (ne, e) =
      Set.fromList
        [ (ne, e + 1),
          (ne - 1, e + 1),
          (ne - 1, e),
          (ne, e - 1),
          (ne + 1, e - 1),
          (ne + 1, e)
        ]
