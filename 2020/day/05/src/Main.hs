{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Monad (replicateM)
import Data.List (foldl', sort)
import Options.Generic (getRecord)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  filepath <- getRecord "Solve"
  boardingPassIds <- either (die . show) (return . map boardingPassId) =<< parseFromFile input filepath

  putStrLn "part 1: highest seat id on a boarding pass"
  print $ maximum boardingPassIds

  putStrLn "part 2: IDs of seats missing from list"
  print $ findMissingIds boardingPassIds

data BoardingPass = BoardingPass {row :: Int, column :: Int}

boardingPassId :: BoardingPass -> Int
boardingPassId (BoardingPass {row, column}) = row * 8 + column

findMissingIds :: [Int] -> [Int]
findMissingIds = concat . (zipWith (\x y -> [x + 1 .. y - 1]) <*> tail) . sort

type Parser a = forall s u m. Stream s m Char => ParsecT s u m a

input :: Parser [BoardingPass]
input = (boardingPass `endBy1` newline) <* eof

boardingPass :: Parser BoardingPass
boardingPass = BoardingPass <$> rowP <*> columnP

rowP :: Parser Int
rowP = fromBits <$> replicateM 7 frontOrBack

columnP :: Parser Int
columnP = fromBits <$> replicateM 3 leftOrRight

fromBits :: [Bool] -> Int
fromBits = foldl' (\n b -> 2 * n + if b then 1 else 0) 0

frontOrBack :: Parser Bool
frontOrBack = (False <$ char 'F') <|> (True <$ char 'B')

leftOrRight :: Parser Bool
leftOrRight = (False <$ char 'L') <|> (True <$ char 'R')
