module AdventOfCode.Day6 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec
import Data.Map.Strict

day6parser :: Parser String
day6parser = many (oneOf ['a'..'z'])

day6part1 :: String -> Int
day6part1 = findUnique 4

findUnique :: Int -> String -> Int
findUnique width = \input ->
    let (chunk, later) = Prelude.splitAt width input
        counts = fromListWith (+) do zip chunk (repeat 1)
    in loop counts (zip3 [width..] input later)
  where 
    loop :: Map Char Int -> [(Int,Char,Char)] -> Int
    loop counts ((pos, old, new):ts)
      | size counts == width = pos
      | otherwise = loop (insertWith (+) new 1 do update dec old counts) ts
    loop _ [] = -1

    dec 1 = Nothing
    dec n = Just (n - 1)


day6part2 :: String -> Int
day6part2 = findUnique 14
