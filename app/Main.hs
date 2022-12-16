{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import AdventOfCode (solvers)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Options.Generic (Generic, ParseField, ParseRecord, getRecord)
import System.Directory (doesFileExist)
import System.Exit (die)
import Text.Parsec.String (parseFromFile)
import Prelude

data App = App String (Maybe Part)
  deriving stock (Generic, Show)

-- TODO: make sure path matches YYYY/day/DD
-- TODO: make the input/YYYY/day directory
-- TODO: ask for a cookie

instance ParseRecord App

data Part = Part1 | Part2
  deriving stock (Read, Show, Generic)

instance ParseField Part

main :: IO ()
main = do
  App day part <- getRecord "Advent of Code solver"

  Solver{parser,part1,part2} <- fromMaybe
    do die do "no available solver for " <> day
    do pure <$> Map.lookup day solvers

  let inputPath = "input/" <> day

  inputAvailable <- doesFileExist inputPath
  unless inputAvailable do
    cookie <- BS.readFile "cookie"
    manager <- newTlsManager
    request <- parseRequest ("https://adventofcode.com/" <> day <> "/input")
    response <- httpLbs request {requestHeaders = [("Cookie", cookie)]} manager
    case statusCode (responseStatus response) of
      200 -> LBS.writeFile inputPath (responseBody response)
      _ -> die do show response

  parseFromFile parser inputPath >>= \case
    Left err -> die (show err)
    Right input -> case fromMaybe Part1 part of
      Part1 -> print (part1 input)
      Part2 -> print (part2 input)
