module AdventOfCode.Day8 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec
import Data.List (transpose)
import Control.Applicative (ZipList(..))
import Data.Functor.Compose (Compose (..))
import Data.Coerce (coerce)

day8parser :: Parser (Grid Int)
day8parser = toGrid <$> rows where
  rows = row `endBy` newline
  row = many height
  height = read . pure <$> digit

type Grid = Compose ZipList ZipList

toGrid :: [[a]] -> Grid a
toGrid = coerce

fromGrid :: Grid a -> [[a]]
fromGrid = coerce

day8part1 :: Grid Int -> Int
day8part1 = sum . map (length . filter id) . fromGrid . visible

visible :: Grid Int -> Grid Bool
visible = fmap (any id) . visibilities

visibilities :: Grid Int -> Grid [Bool]
visibilities trees = traverse ($ trees) [leftToRight, rightToLeft, topToBottom, bottomToTop] 

leftToRight :: Grid Int -> Grid Bool
leftToRight = mapRows isLeftVisible

rightToLeft :: Grid Int -> Grid Bool
rightToLeft = mapRows isRightVisible

topToBottom :: Grid Int -> Grid Bool
topToBottom = mapCols isLeftVisible

bottomToTop :: Grid Int -> Grid Bool
bottomToTop = mapCols isRightVisible

isRightVisible :: [Int] -> [Bool]
isRightVisible = reverse . isLeftVisible . reverse

isLeftVisible :: [Int] -> [Bool]
isLeftVisible row = zipWith (>) row do scanl max (-1) row

mapRows :: ([a] -> [b]) -> Grid a -> Grid b
mapRows f = coerce (map f)

mapCols :: ([a] -> [b]) -> Grid a -> Grid b
mapCols f = coerce (transpose . map f . transpose)

day8part2 :: Grid Int -> Int
day8part2 = maximum . scenicScores

scenicScores :: Grid Int -> Grid Int
scenicScores trees = product <$> traverse ($trees)
  [ mapRows leftScore
  , mapRows rightScore
  , mapCols leftScore
  , mapCols rightScore
  ]

leftScore, rightScore :: [Int] -> [Int]
leftScore = fmap (snd . head) . tail . scanl step [] where
  step ps house = 
    let (inc,exc) = break (\(h,_) -> h >= house) ps
        score = sum (map snd inc) + if null ps then 0 else 1
    in (house, score) : exc
    
rightScore = reverse . leftScore . reverse
