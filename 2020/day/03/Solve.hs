{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.List (intercalate)
import System.Exit (die)
import System.Environment (getArgs)
import Text.Parsec
import Data.Vector (Vector)
import qualified Data.Vector as Vector

infixr 2 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = flip (.)

type Parser a = forall s u m. Stream s m Char => ParsecT s u m a

data TreeLocations = TreeLocations { getTreeLocations :: Vector (Vector Bool) }

treeLocations :: Parser TreeLocations
treeLocations = TreeLocations . Vector.fromList <$> endBy1 treeLine newline <* eof where
  treeLine = Vector.fromList <$> many1 treePattern
  treePattern = True <$ char '#' <|> False <$ char '.'

(!) :: TreeLocations -> (Int, Int) -> Bool
TreeLocations rows ! (x,y) = row Vector.! (x `rem` Vector.length row)
  where row = rows Vector.! y

width :: TreeLocations -> Int
width (TreeLocations rows) = maybe 0 length $ rows Vector.!? 0

height :: TreeLocations -> Int
height = Vector.length . getTreeLocations

pretty :: TreeLocations -> String
pretty = intercalate "\n" . map (map (\b -> if b then '#' else '.') . Vector.toList) . Vector.toList . getTreeLocations

route :: (Int, Int) -> [(Int, Int)]
route (dx,dy) = zip [0,dx..] [0,dy..]

collisions :: (Int, Int) -> TreeLocations -> Int
collisions (dx,dy) ts = length . filter (ts !) $ zip [0,dx..] [0,dy..height ts - 1]

main :: IO ()
main = do
  inputPath <- getArgs >>= \case
    [inputPath] -> return inputPath
    args        -> die $ "incorrect number of arguments, expected one path, got " ++ show args

  input <- readFile inputPath >>= return . parse treeLocations inputPath >>= \case
    Left err    -> die $ show err
    Right input -> return input

  -- putStrLn "input:"
  -- putStrLn $ pretty input

  putStrLn "part one: number of collisions for the slope of right 3, down 1"
  print $ collisions (3, 1) input

  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  putStrLn $ "part two: product of collisions for slopes " ++ show slopes
  print . product $ (`collisions` input) <$> slopes

  return ()
