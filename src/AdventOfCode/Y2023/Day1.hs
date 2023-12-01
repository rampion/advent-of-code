{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode.Y2023.Day1 where

import AdventOfCode.Y2023.Prelude
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = do
        check parser part1 part2 do
          Example
            { raw =
                [text|
                  1abc2
                  pqr3stu8vwx
                  a1b2c3d4e5f
                  treb7uchet
                |]
                  <> "\n"
            , parsed =
                [ "1abc2"
                , "pqr3stu8vwx"
                , "a1b2c3d4e5f"
                , "treb7uchet"
                ]
            , part1output = Just 142
            , part2output = Just 142
            }
        check parser part1 part2 do
          Example
            { raw =
                [text|
                  two1nine
                  eightwothree
                  abcone2threexyz
                  xtwone3four
                  4nineeightseven2
                  zoneight234
                  7pqrstsixteen
                |]
                  <> "\n"
            , parsed =
                [ "two1nine"
                , "eightwothree"
                , "abcone2threexyz"
                , "xtwone3four"
                , "4nineeightseven2"
                , "zoneight234"
                , "7pqrstsixteen"
                ]
            , part1output = Nothing
            , part2output = Just 281
            }
    }

parser :: Parser [String]
parser = lineOfText `endBy` newline
  where
    lineOfText = some (digit <|> lower)

part1 :: [String] -> Maybe Int
part1 = fmap sum . traverse calibrationValue

calibrationValue :: String -> Maybe Int
calibrationValue input = do
  tens <- findDigit input
  unit <- findDigit (reverse input)
  pure do
    10 * tens + unit

findDigit :: String -> Maybe Int
findDigit = \input -> case dropWhile (not . isDigit) input of
  [] -> Nothing
  d : _ -> Just (fromEnum d - zero)
  where
    zero = fromEnum '0'

part2 :: [String] -> Maybe Int
part2 = fmap sum . traverse calibrationValue'

calibrationValue' :: String -> Maybe Int
calibrationValue' input = do
  tens <- findFirst digits input
  unit <- findFirst (first reverse <$> digits) (reverse input)
  pure do
    10 * tens + unit

digits :: [(String, Int)]
digits =
  ("zero", 0)
    : ("one", 1)
    : ("two", 2)
    : ("three", 3)
    : ("four", 4)
    : ("five", 5)
    : ("six", 6)
    : ("seven", 7)
    : ("eight", 8)
    : ("nine", 9)
    : [(show n, n) | n <- [0 .. 9]]

findFirst :: [(String, a)] -> String -> Maybe a
findFirst _ [] = Nothing
findFirst ps t =
  listToMaybe [v | (k, v) <- ps, k `isPrefixOf` t] <|> findFirst ps (tail t)
