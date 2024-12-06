{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module SolveDay where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Time (Day, defaultTimeLocale, formatTime)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.Exit (die)
import Text.Parsec.Text (Parser, parseFromFile)
import Prelude

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

