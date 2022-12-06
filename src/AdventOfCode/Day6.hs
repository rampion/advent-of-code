module AdventOfCode.Day6 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec
import Data.Map.Strict

day6parser :: Parser String
day6parser = many (oneOf ['a'..'z'])

day6part1 :: String -> Int
day6part1 = \input ->
    let (chunk, later) = Prelude.splitAt 4 input
        counts = fromListWith (+) do zip chunk (repeat 1)
    in loop counts (zip3 [4..] input later)
  where 
    loop :: Map Char Int -> [(Int,Char,Char)] -> Int
    loop counts ((pos, old, new):ts)
      | size counts == 4 = pos
      | otherwise = loop (insertWith (+) new 1 do update dec old counts) ts
    loop _ [] = -1

    dec 1 = Nothing
    dec n = Just (n - 1)


day6part2 :: String -> Int
day6part2 = error "unimplemented"
