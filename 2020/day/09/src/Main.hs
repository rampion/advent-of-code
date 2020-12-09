{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror -Wextra -Wno-name-shadowing #-}

module Main where

import Control.Monad (filterM, (>=>))
import Control.Monad.State (State, evalState, gets, modify)
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List (foldl', tails)
import System.Environment (getArgs)

main :: IO ()
main = (getArgs >>=) . traverse_ $ \filename -> do
  putStrLn filename

  let window = getWindow filename
  numbers <- map read . lines <$> readFile filename

  let invalidNumber = part1 window numbers

  putStrLn $ "\tpart 1: first number that isn't the sum of two distinct values from the " ++ show window ++ " prior numbers"
  putStrLn $ "\t" ++ show invalidNumber

  putStrLn $ "\tpart 2: encryption weakness of list of numbers"
  putStrLn $ "\t" ++ show (part2 invalidNumber numbers)

part2 :: Int -> [Int] -> Int
part2 target numbers = maximum range + minimum range
  where
    range = search 0 0 numbers numbers
    search total count start end = case total `compare` target of
      EQ | count > 1 -> take count start
      LT -> case end of
        x : end -> search (total + x) (count + 1) start end
        [] -> error $ "unable to find range summing to " ++ show target
      _ -> case start of
        ~(x : start) -> search (total - x) (count - 1) start end

getWindow :: String -> Int
getWindow = read . takeWhileEnd isDigit

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

part1 :: Int -> [Int] -> Int
part1 window numbers = case filterM ((<*) <$> isNotSum <*> shift) suffix `evalState` (sumCounts, numbers) of
  [] -> error $ "expected some number to not be the sum of two of the previous " ++ show window
  x : _ -> x
  where
    isNotSum :: Int -> State (IntMap Int, [Int]) Bool
    isNotSum x = gets $ maybe True (== 0) . Map.lookup x . fst

    shift :: Int -> State (IntMap Int, [Int]) ()
    shift x = modify $ \(m, ~(y : zs)) -> (foldl' (remove y >=> insert x) m (take (window - 1) zs), zs)

    prefix :: [Int]
    suffix :: [Int]
    (prefix, suffix) = splitAt window numbers

    sumCounts :: IntMap Int
    sumCounts = Map.fromListWith (+) [(x + y, 1) | x : ys <- tails prefix, y <- ys, x /= y]

    remove :: Int -> IntMap Int -> Int -> IntMap Int
    remove x m y
      | x == y = m
      | otherwise = Map.update (\case 1 -> Nothing; n -> Just (n - 1)) (x + y) m

    insert :: Int -> IntMap Int -> Int -> IntMap Int
    insert x m y
      | x == y = m
      | otherwise = Map.insertWith (+) (x + y) 1 m
