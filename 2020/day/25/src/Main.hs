{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    publicKeys <- readPublicKeys filename

    putStrLn "\tpart 1: what encryption key is the handshake trying to establish"
    let actual1 = part1 publicKeys
    expected1 <- readExpected (filename ++ ".part1")
    putStrLn $ "\t" ++ check actual1 expected1 ++ " " ++ show actual1

readExpected :: Read a => FilePath -> IO (Maybe a)
readExpected filename =
  (Just . read . head . words <$> readFile filename) <|> return Nothing

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

readPublicKeys :: FilePath -> IO (Int, Int)
readPublicKeys filename =
  (map read . lines <$> readFile filename) >>= \case
    [card, door] -> return (card, door)
    keys -> error $ "wrong number of keys: " ++ show keys

part1 :: (Int, Int) -> Int
part1 = \(card, door) -> powMod (discreteLog card * discreteLog door)
  where
    base = 20201227
    common = 7

    powMod 0 = 1
    powMod n =
      let (q, r) = n `quotRem` 2
       in (powMod q ^ (2 :: Int) * common ^ r) `rem` base

    discreteLog n = length . fst $ break (== n) logs

    logs = iterate (\n -> common * n `rem` base) 1
