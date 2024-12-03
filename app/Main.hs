{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (unless, replicateM)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import NeatInterpolation (text)
import System.Directory (doesFileExist)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.Text (parseFromFile, Parser)
import Test.Hspec (shouldBe, it, hspec)
import Prelude
import Data.Time (Day, fromGregorian, formatTime, defaultTimeLocale)

-- $> main
main :: IO ()
main = do
  solveDay parse1 part1 $ fromGregorian 2024 12 03

data Instruction
  = Mul Int Int
  | Do
  | Don't
  deriving (Show, Eq)

unused :: ()
unused = replicateM @IO `seq` ()

parse1 :: Parser [Instruction]
parse1 = (next <*> parse1) <|> ([] <$ eof) where
  next = ((:) <$> instruction) <|> (id <$ anyChar)
  instruction = try mul <|> try don't <|> try do_
  don't = Don't <$ string "don't()"
  do_ = Do <$ string "do()"
  mul = Mul <$ string "mul(" <*> int <* char ',' <*> int <* char ')'
  int = read <$> many1 digit

part1 :: [Instruction] -> Int
part1 = fst . foldl' step init where
  step (!n,op) = \case
    Mul a b -> (n `op` (a * b), op)
    Don't -> (n, const)
    Do -> (n, (+))
  init = (0, (+))

firstExample :: [Instruction]
firstExample =
  [ Mul 2 4
  , Mul 5 5
  , Mul 11 8
  , Mul 8 5
  ]

secondExample :: [Instruction]
secondExample =
  [ Mul 2 4
  , Don't
  , Mul 5 5
  , Mul 11 8
  , Do
  , Mul 8 5
  ]

-- $> runTests
runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw = [text|
          xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
        |] <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "parses the second example" do
    let raw = [text|
          xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
        |] <> "\n"

    parse parse1 "second example" raw `shouldBe` Right secondExample

  it "solves part one with the first example" do
    part1 firstExample `shouldBe` 161

  it "solves part two with the second example" do
    part1 secondExample `shouldBe` 48

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
