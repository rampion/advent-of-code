{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Control.Monad (guard, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Function (fix)
import Data.List (zipWith5)
import Data.Time (Day, defaultTimeLocale, formatTime, fromGregorian)
import NeatInterpolation (text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.Exit (die)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec
import Text.Parsec.Text (Parser, parseFromFile)
import Prelude

-- $> main

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 04

data Letter
  = X
  | M
  | A
  | S
  deriving (Show, Eq)

parse1 :: Parser [[Letter]]
parse1 = many letter `endBy1` newline
  where
    letter = choice [x, m, a, s]
    x = X <$ char 'X'
    m = M <$ char 'M'
    a = A <$ char 'A'
    s = S <$ char 'S'

data SearchState = Next
  { westToEast :: Letter
  , eastToWest :: Letter
  , northwestToSoutheast :: Letter
  , southeastToNorthwest :: Letter
  , northToSouth :: Letter
  , southToNorth :: Letter
  , northeastToSouthwest :: Letter
  , southwestToNortheast :: Letter
  }
  deriving (Show, Eq)

blank :: SearchState
blank =
  Next
    { westToEast = X
    , eastToWest = S
    , northwestToSoutheast = X
    , southeastToNorthwest = S
    , northToSouth = X
    , southToNorth = S
    , northeastToSouthwest = X
    , southwestToNortheast = S
    }

part1 :: [[Letter]] -> Int
part1 = sum . map sum . fst . buildTable

buildTable :: [[Letter]] -> ([[Int]], [[SearchState]])
buildTable ls = fix \(~(_, table)) -> unzip do
  zipWith buildRow ls (repeat blank : table)

buildRow :: [Letter] -> [SearchState] -> ([Int], [SearchState])
buildRow cs above = fix \(~(_, row)) -> unzip do
  zipWith5 buildCell cs (blank : row) (blank : above) above (drop 1 above ++ [blank])

buildCell :: Letter -> SearchState -> SearchState -> SearchState -> SearchState -> (Int, SearchState)
buildCell c Next {westToEast, eastToWest} Next {northwestToSoutheast, southeastToNorthwest} Next {northToSouth, southToNorth} Next {northeastToSouthwest, southwestToNortheast} =
  ( length do
      (expected, final) <-
        [ (westToEast, S)
          , (eastToWest, X)
          , (northwestToSoutheast, S)
          , (southeastToNorthwest, X)
          , (northToSouth, S)
          , (southToNorth, X)
          , (northeastToSouthwest, S)
          , (southwestToNortheast, X)
          ]
      guard $ c == expected && c == final
  , Next
      { westToEast = nextMatch westToEast c
      , eastToWest = prevMatch eastToWest c
      , northwestToSoutheast = nextMatch northwestToSoutheast c
      , southeastToNorthwest = prevMatch southeastToNorthwest c
      , northToSouth = nextMatch northToSouth c
      , southToNorth = prevMatch southToNorth c
      , northeastToSouthwest = nextMatch northeastToSouthwest c
      , southwestToNortheast = prevMatch southwestToNortheast c
      }
  )

nextMatch :: Letter -> Letter -> Letter
nextMatch expected = \case
  X -> M
  c | c /= expected -> X
  M -> A
  A -> S
  S -> X

prevMatch :: Letter -> Letter -> Letter
prevMatch expected = \case
  S -> A
  c | c /= expected -> S
  A -> M
  M -> X
  X -> S

firstExample :: [[Letter]]
firstExample =
  [ [M, M, M, S, X, X, M, A, S, M]
  , [M, S, A, M, X, M, S, M, S, A]
  , [A, M, X, S, X, M, A, A, M, M]
  , [M, S, A, M, A, S, M, S, M, X]
  , [X, M, A, S, A, M, X, A, M, M]
  , [X, X, A, M, M, X, X, A, M, A]
  , [S, M, S, M, S, A, S, X, S, S]
  , [S, A, X, A, M, A, S, A, A, A]
  , [M, A, M, M, M, X, M, M, M, M]
  , [M, X, M, X, A, X, M, A, S, X]
  ]

part2 :: [[Letter]] -> Int
part2 = sum . map (length . filter id) . buildTable2

buildTable2 :: [[Letter]] -> [[Bool]]
buildTable2 ls = zipWith3 buildRow2 (repeat X : ls) ls (drop 1 ls ++ [repeat X])

buildRow2 :: [Letter] -> [Letter] -> [Letter] -> [Bool]
buildRow2 above row below =
  zipWith5 buildCell2 (X : above) (drop 1 above ++ [X]) row (X : below) (drop 1 below ++ [X])

buildCell2 :: Letter -> Letter -> Letter -> Letter -> Letter -> Bool
buildCell2 nw ne cell sw se =
  cell == A && isLeg nw se && isLeg sw ne

isLeg :: Letter -> Letter -> Bool
isLeg = \cases
  S M -> True
  M S -> True
  _ _ -> False

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
          MMMSXXMASM
          MSAMXMSMSA
          AMXSXMAAMM
          MSAMASMSMX
          XMASAMXAMM
          XXAMMXXAMA
          SMSMSASXSS
          SAXAMASAAA
          MAMMMXMMMM
          MXMXAXMASX
        |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 18

  it "finds one match in XMAS" do
    part1 [[X, M, A, S]] `shouldBe` 1

  it "finds one match in XXMAS" do
    -- part1 [[X, X, M, A, S]] `shouldBe` 1
    let actual = fmap westToEast <$> buildRow [X, X, M, A, S] (repeat blank)
    actual `shouldBe` ([0, 0, 0, 0, 1], [M, M, A, S, X])

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 9

solveDay :: Show b => Parser a -> (a -> b) -> Day -> IO ()
solveDay parser solver day = do
  inputPath <- downloadInput day

  parseFromFile parser inputPath >>= \case
    Left err -> die (show err)
    Right input -> print (solver input)

dayFormat :: String
dayFormat = "%Y/day/%-d"

downloadInput :: Day -> IO FilePath
downloadInput day = do
  let inputPath = formatTime defaultTimeLocale ("input/" <> dayFormat) day

  inputAvailable <- doesFileExist inputPath
  unless inputAvailable do
    cookie <- BS.readFile "cookie"
    manager <- newTlsManager
    request <- parseRequest do
      formatTime defaultTimeLocale ("https://adventofcode.com/" <> dayFormat <> "/input") day

    response <- httpLbs request {requestHeaders = [("Cookie", cookie)]} manager
    case statusCode (responseStatus response) of
      200 -> LBS.writeFile inputPath (responseBody response)
      _ -> die do show response

  pure inputPath
