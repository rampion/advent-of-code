{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Data.Function ((&))
import Data.Functor ((<&>))
-- import Data.Map (Map)
-- import Data.Map qualified as Map

import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (fromGregorian)
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import NeatInterpolation (text)
import SolveDay (solveDay)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude

-- $> main
-- 6_347_382_685_229 = too low
-- 6_347_435_614_860 = too high

main :: IO ()
main = do
  solveDay parse1 part2 $ fromGregorian 2024 12 09

type Puzzle = [(Skip, Id, Size)]

newtype Id = Id Int
  deriving newtype (Show, Enum, Eq, Ord)

newtype Size = Size {unSize :: Int}
  deriving newtype (Show, Eq, Ord)

newtype Skip = Skip {unSkip :: Int}
  deriving newtype (Show, Eq)

parse1 :: Parser Puzzle
parse1 = zipWith (&) [Id 0 ..] <$> entries <* newline <* eof
  where
    entries = (:) <$> firstEntry <*> laterEntries
    firstEntry = file <&> \n i -> (Skip 0, i, n)
    laterEntries = many (flip . (,,) <$> free <*> file)
    free = Skip <$> dig
    file = Size <$> dig
    dig = read . pure <$> digit

part1 :: Puzzle -> Int
part1 = checksum . compact1

part2 :: Puzzle -> Int
part2 = checksum . compact2

checksum :: [(Skip, Id, Size)] -> Int
checksum = loop 0 0
  where
    loop !off !tot = \case
      [] -> tot
      (Skip skip, Id i, Size n) : ps -> do
        let off' = off + skip
        loop (off' + n) (tot + i * n * (2 * off' + n - 1) `div` 2) ps

-- $/> showDisk $ compact2 firstExample

compact2 :: Puzzle -> Puzzle
compact2 = \disk -> loop 0 (Set.fromList [(ui, i) | (_, i, ui) <- disk]) disk
  where
    loop rem avail = \case
      (fi, i, ui) : rest
        | Just (uj, j) <- search fi avail ->
            (Skip rem, j, uj) : loop 0 (Set.delete (uj, j) avail) ((fi `minus` uj, i, ui) : rest)
        | Set.member (ui, i) avail ->
            (Skip (rem + unSkip fi), i, ui) : loop 0 (Set.delete (ui, i) avail) rest
        | otherwise -> loop (rem + unSkip fi + unSize ui) avail rest
      [] -> []

search :: Skip -> Set (Size, Id) -> Maybe (Size, Id)
-- search (Skip 0) = const Nothing
search (Skip fi) =
  safeMaximumBy snd
    . Set.toList
    . Set.takeWhileAntitone \(Size uj, _) -> uj <= fi

safeMaximumBy :: (Ord b) => (a -> b) -> [a] -> Maybe a
safeMaximumBy f = \case
  [] -> Nothing
  a : as -> Just $ List.foldl' maxf a as
  where
    maxf a0 a1 = if f a0 >= f a1 then a0 else a1

minus :: Skip -> Size -> Skip
minus (Skip fi) (Size uj) = Skip (fi - uj)

showDisk :: Puzzle -> String
showDisk = concatMap \(Skip skip, Id i, Size size) ->
  replicate skip '.' <> replicate size (toEnum (i + fromEnum '0'))

insert :: (Id, Size) -> Puzzle -> Puzzle
insert (j, Size !uj) = \case
  (Skip !fi, i, Size !ui) : rest
    | i == j -> (Skip if fi >= ui then 0 else fi, i, Size ui) : rest
    | fi >= uj -> (Skip 0, j, Size uj) : (Skip (fi - uj), i, Size ui) : delete j rest
    | otherwise -> (Skip fi, i, Size ui) : insert (j, Size uj) rest
  [] -> []

delete :: Id -> Puzzle -> Puzzle
delete j = \case
  (Skip !fi, i, Size !ui) : rest
    | i == j -> case rest of
        [] -> []
        (Skip fk, k, uk) : rest -> (Skip (fk + fi + ui), k, uk) : rest
    | otherwise -> (Skip fi, i, Size ui) : delete j rest
  [] -> []

compact1 :: Puzzle -> Puzzle
compact1 = loop <*> reverse
  where
    loop = \cases
      ((Skip fi, i, Size ui) : fore) ((Skip fj, j, Size uj) : back) ->
        case compare i j of
          LT -> case compare fi uj of
            LT -> (Skip 0, j, Size fi) : (Skip 0, i, Size ui) : loop fore ((Skip fj, j, Size (uj - fi)) : back)
            EQ -> (Skip 0, j, Size uj) : (Skip 0, i, Size ui) : loop fore back
            GT -> (Skip 0, j, Size uj) : loop ((Skip (fi - uj), i, Size ui) : fore) back
          EQ -> [(Skip 0, j, Size uj)]
          GT -> []
      _ _ -> []

firstExample :: Puzzle
firstExample =
  [ (Skip 0, Id 0, Size 2)
  , (Skip 3, Id 1, Size 3)
  , (Skip 3, Id 2, Size 1)
  , (Skip 3, Id 3, Size 3)
  , (Skip 1, Id 4, Size 2)
  , (Skip 1, Id 5, Size 4)
  , (Skip 1, Id 6, Size 4)
  , (Skip 1, Id 7, Size 3)
  , (Skip 1, Id 8, Size 4)
  , (Skip 0, Id 9, Size 2)
  ]

-- $> runTests

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            2333133121414131402
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 1928

  it "solves part two with the first example" do
    part2 firstExample `shouldBe` 2858
