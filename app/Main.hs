{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import AdventOfCode
import Control.Monad (unless)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Options.Generic (ParseRecord, getRecord, Generic, ParseField)
import Prelude
import System.Directory (doesFileExist)
import System.Exit (die)
import Text.Parsec.String (parseFromFile)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

data App = App String (Maybe Part)
  deriving stock (Generic, Show)

-- TODO: make sure path matches YYYY/day/DD
-- TODO: make the input/YYYY/day directory
-- TODO: ask for a cookie

instance ParseRecord App

data Part = Part1 | Part2
  deriving stock (Read, Show, Generic)

instance ParseField Part

solvers :: Map.Map String Solver
solvers = Map.fromList
  [ ( "2022/day/1", Solver day1parser day1part1 day1part2)
  , ( "2022/day/2", Solver day2parser day2part1 day2part2)
  , ( "2022/day/3", Solver day3parser day3part1 day3part2)
  , ( "2022/day/4", Solver day4parser day4part1 day4part2)
  , ( "2022/day/5", Solver day5parser day5part1 day5part2)
  , ( "2022/day/6", Solver day6parser day6part1 day6part2)
  , ( "2022/day/7", Solver day7parser day7part1 day7part2)
  , ( "2022/day/8", Solver day8parser day8part1 day8part2)
  , ( "2022/day/9", Solver day9parser day9part1 day9part2)
  , ( "2022/day/10", Solver day10parser day10part1 day10part2)
  , ( "2022/day/11", Solver day11parser day11part1 day11part2)
  , ( "2022/day/12", Solver day12parser day12part1 day12part2)
  , ( "2022/day/13", Solver day13parser day13part1 day13part2)
  , ( "2022/day/14", Solver day14parser day14part1 day14part2)
  , ( "2022/day/15", Solver day15parser day15part1 day15part2)
  , ( "2022/day/16", Solver day16parser day16part1 day16part2)
  , ( "2022/day/17", Solver day17parser day17part1 day17part2)
  , ( "2022/day/18", Solver day18parser day18part1 day18part2)
  , ( "2022/day/19", Solver day19parser day19part1 day19part2)
  , ( "2022/day/20", Solver day20parser day20part1 day20part2)
  , ( "2022/day/21", Solver day21parser day21part1 day21part2)
  , ( "2022/day/22", Solver day22parser day22part1 day22part2)
  , ( "2022/day/23", Solver day23parser day23part1 day23part2)
  , ( "2022/day/24", Solver day24parser day24part1 day24part2)
  , ( "2022/day/25", Solver day25parser day25part1 day25part2)
  ]

main :: IO ()
main = do
  App day part <- getRecord "Advent of Code solver"

  Solver parser part1 part2 <- fromMaybe
    do die do "no available solver for " <> day
    do pure <$> Map.lookup day solvers

  let inputPath = "input/" <> day

  inputAvailable <- doesFileExist inputPath
  unless inputAvailable do
    cookie <- BS.readFile "cookie"
    manager <- newTlsManager
    request <- parseRequest ("https://adventofcode.com/" <> day <> "/input")
    response <- httpLbs request { requestHeaders = [("Cookie", cookie)] } manager
    case statusCode (responseStatus response) of
      200 -> LBS.writeFile inputPath (responseBody response)
      _ -> die do show response

  parseFromFile parser inputPath >>= \case
    Left err -> die (show err)
    Right input -> case fromMaybe Part1 part of
      Part1 -> print (part1 input)
      Part2 -> print (part2 input)
