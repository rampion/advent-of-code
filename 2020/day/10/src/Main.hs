{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable (traverse_)
import qualified Data.IntMap as Map
import Data.List (foldl', sort)
import System.Environment (getArgs)

main :: IO ()
main = (getArgs >>=) . traverse_ $ \filename -> do
  putStrLn filename

  adaptors <- map read . lines <$> readFile filename

  putStrLn "\tpart 1: product of 1-joltage differences and 3-joltage differences"
  putStrLn $ "\t" ++ show (part1 adaptors)

  putStrLn "\tpart 2: number of different adaptor chains"
  putStrLn $ "\t" ++ show (part2 adaptors)

type Joltage = Int

type JoltageMap = Map.IntMap

part1 :: [Joltage] -> Integer
part1 rs = (m Map.! 1) * (m Map.! 3)
  where
    m = differenceCounts rs

differenceCounts :: [Joltage] -> JoltageMap Integer
differenceCounts = Map.fromListWith (+) . map (,1) . (3 :) . (zipWith (-) =<< tail) . (0 :) . sort

-- dynamic programming!
part2 :: [Joltage] -> Integer
part2 = third . foldl' step (0, 0, 1, 0) . sort
  where
    third (_, _, c, _) = c
    step (!a, !b, !c, n) x = case x - n of
      1 -> (b, c, a + b + c, x)
      2 -> (c, 0, b + c, x)
      3 -> (0, 0, c, x)
      _ -> (0, 0, 0, x)
