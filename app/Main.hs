{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Prelude
import Options.Generic (ParseRecord, getRecord, Generic, ParseField)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.Exit (die)
import AdventOfCode
import Text.Parsec.String (parseFromFile)

data App = App String (Maybe Part)
  deriving stock (Generic, Show)

instance ParseRecord App

data Part = Part1 | Part2
  deriving stock (Read, Show, Generic)

instance ParseField Part

solvers :: Map.Map String Solver
solvers = Map.fromList
  [ ( "2022/day/1", Solver day1parser day1part1 day1part2)
  , ( "2022/day/2", Solver day2parser day2part1 day2part2)
  ]

main :: IO ()
main = do
  App day part <- getRecord "Advent of Code solver"

  Solver parser part1 part2 <- fromMaybe
    do die do "no available solver for " <> day
    do pure <$> Map.lookup day solvers

  parseFromFile parser ("input/" <> day) >>= \case
    Left err -> die (show err)
    Right input -> case fromMaybe Part1 part of
      Part1 -> print (part1 input)
      Part2 -> print (part2 input)
