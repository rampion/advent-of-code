{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (comparing)
import Data.Set qualified as Set
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
  solveDay parse1 part2 $ fromGregorian 2024 12 05

type Page = Int

data Puzzle = Puzzle
  { pageOrderingRules :: [(Page, Page)]
  , updates :: [NonEmpty Page]
  }
  deriving (Show, Eq)

parse1 :: Parser Puzzle
parse1 = Puzzle <$> many rule <* newline <*> many update <* eof
  where
    rule = (,) <$> page <* char '|' <*> page <* newline
    update = (:|) <$> page <*> many (char ',' *> page) <* newline
    page = read <$> many1 digit

part1 :: Puzzle -> Int
part1 = sum . map middle . (filter <$> isInOrder . pageOrderingRules <*> updates)

part2 :: Puzzle -> Int
part2 Puzzle {pageOrderingRules, updates} =
  sum
    . map (middle . putInOrder pageOrderingRules)
    $ filter (not . isInOrder pageOrderingRules) updates

middle :: NonEmpty Page -> Page
middle (p :| ps) = (p : ps) !! i
  where
    n = 1 + length ps
    i = n `div` 2

putInOrder :: [(Page, Page)] -> NonEmpty Page -> NonEmpty Page
putInOrder (Set.fromList -> lt) = NonEmpty.unfoldr next . graph
  where
    graph ps = lesserSort $ fmap (assocLesser (NonEmpty.toList ps)) ps

    lesserSort = NonEmpty.sortBy (comparing (Set.size . snd))
    assocLesser ps p = (p, Set.fromList [p' | p' <- ps, (p', p) `Set.member` lt])

    next ((p, _) :| as) =
      (p, lesserSort . fmap (fmap (Set.delete p)) <$> NonEmpty.nonEmpty as)

isInOrder :: [(Page, Page)] -> NonEmpty Page -> Bool
isInOrder (Set.fromList -> lt) (p :| ps) = not $ any (`Set.member` lt) do
  p0 : pt <- List.tails (p : ps)
  p1 <- pt
  pure (p1, p0)

firstExample :: Puzzle
firstExample =
  Puzzle
    { pageOrderingRules =
        [ (47, 53)
        , (97, 13)
        , (97, 61)
        , (97, 47)
        , (75, 29)
        , (61, 13)
        , (75, 53)
        , (29, 13)
        , (97, 29)
        , (53, 29)
        , (61, 53)
        , (97, 53)
        , (61, 29)
        , (47, 13)
        , (75, 47)
        , (97, 75)
        , (47, 61)
        , (75, 61)
        , (47, 29)
        , (75, 13)
        , (53, 13)
        ]
    , updates =
        [ 75 :| [47, 61, 53, 29]
        , 97 :| [61, 53, 29, 13]
        , 75 :| [29, 13]
        , 75 :| [97, 47, 61, 53]
        , 61 :| [13, 29]
        , 97 :| [13, 75, 29, 47]
        ]
    }

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            47|53
            97|13
            97|61
            97|47
            75|29
            61|13
            75|53
            29|13
            97|29
            53|29
            61|53
            97|53
            61|29
            47|13
            75|47
            97|75
            47|61
            75|61
            47|29
            75|13
            53|13

            75,47,61,53,29
            97,61,53,29,13
            75,29,13
            75,97,47,61,53
            61,13,29
            97,13,75,29,47
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 143

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 123

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
