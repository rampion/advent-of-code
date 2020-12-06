module Main where

import Data.Foldable (traverse_)
import Data.List (foldl1')
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = (getArgs >>=) . traverse_ $ \inputFile -> do
  putStrLn inputFile

  input <- groups <$> readFile inputFile

  putStrLn $ "\tpart 1: sum over groups of number of questions that some member answered yes to"
  putStrLn $ "\t" ++ show (part1 input)

  putStrLn $ "\tpart 2: sum over groups of number of questions that all members answered yes to"
  putStrLn $ "\t" ++ show (part2 input)

type Group = [Member]

type Member = [Answer]

type Answer = Char

groups :: String -> [Group]
groups = map lines . splitOn "\n\n"

part1 :: [Group] -> Int
part1 = sum . map (Set.size . Set.unions . map Set.fromList)

part2 :: [Group] -> Int
part2 = sum . map (Set.size . foldl1' Set.intersection . map Set.fromList)
