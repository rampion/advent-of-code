{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -Werror -Wextra -Wno-name-shadowing #-}

module Main where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', (\\))
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    cups <- readInput filename

    putStrLn "\tpart 1: labels on the cups after cup 1 after 100 steps"
    let actual1 = part1 cups
    expected1 <- readExpected (filename ++ ".part1")
    putStrLn $ "\t" ++ check actual1 expected1 ++ " " ++ show actual1

    putStrLn "\tpart 2: product of the labels on the two cups after cup 1 after ten million steps with one million cups"
    let actual2 = part2 cups
    expected2 <- readExpected (filename ++ ".part2")
    putStrLn $ "\t" ++ check actual2 expected2 ++ " " ++ show actual2

--------------------------------------------------------------------------------

part2 :: [Cup] -> Int
part2 cs =
  product
    . take 2
    . tail
    . unlink
    . snd
    . (!! 10_000_000)
    . iterate (uncurry step)
    . (head cs,)
    . link
    $ cs ++ [10 .. 1_000_000]

--------------------------------------------------------------------------------

part1 :: [Cup] -> Int
part1 cs =
  fromDigits
    . takeWhile (/= 1)
    . tail
    . unlink
    . snd
    . (!! 100)
    . iterate (uncurry step)
    . (head cs,)
    $ link cs

step :: Cup -> CupList -> (Cup, CupList)
step !a !m = (e, m')
  where
    b = m IntMap.! a
    c = m IntMap.! b
    d = m IntMap.! c
    e = m IntMap.! d
    a' = head . (\\ [b, c, d]) $ [a - 1, a - 2 .. 1] ++ [n, n - 1 .. a + 1]
    e' = m IntMap.! a'
    n = IntMap.size m
    m' = IntMap.fromList [(a, e), (a', b), (d, e')] `IntMap.union` m

-- maps each cup to its next neighbor
type CupList = IntMap.IntMap Cup

link :: [Cup] -> CupList
link cs = IntMap.fromList $ zip cs (tail cs ++ [head cs])

unlink :: CupList -> [Cup]
unlink m = iterate (m IntMap.!) 1

fromDigits :: [Int] -> Int
fromDigits = foldl' (\n d -> 10 * n + d) 0

--------------------------------------------------------------------------------

readInput :: FilePath -> IO [Cup]
readInput filename = map (read . return) . head . words <$> readFile filename

readExpected :: Read a => FilePath -> IO (Maybe a)
readExpected filename =
  (Just . read . head . words <$> readFile filename) <|> return Nothing

check :: Eq a => a -> Maybe a -> String
check actual = \case
  Just expected | actual == expected -> ansi green "✓"
  Nothing -> ansi yellow "?"
  _ -> ansi red "✗"
  where
    ansi :: Int -> String -> String
    ansi color text = "\x1b[" ++ show color ++ "m" ++ text ++ "\x1b[m"

    green, yellow, red :: Int
    green = 32
    yellow = 33
    red = 31

--------------------------------------------------------------------------------

type Cup = Int
