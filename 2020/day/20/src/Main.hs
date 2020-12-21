{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Werror -Wextra -Wno-name-shadowing #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity)
import Data.List (tails, transpose)
import System.Environment (getArgs)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (parseFromFile)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    tiles <- either (fail . show) return =<< parseFromFile tiles filename
    let arrangement = arrange tiles

    putStrLn "\tpart 1: product of ids of four corner tiles"
    expected1 <- parseExpected (filename ++ ".part1")
    let actual1 = part1 arrangement
    putStrLn $ "\t" ++ check actual1 expected1 ++ " " ++ show actual1

    putStrLn "\tpart 2: number of #s that are not part of a sea monster"
    expected2 <- parseExpected (filename ++ ".part2")
    let actual2 = part2 arrangement
    putStrLn $ "\t" ++ check actual2 expected2 ++ " " ++ show actual2

--------------------------------------------------------------------------------

{-
printImage :: Image -> IO ()
printImage = putStrLn . unlines . map (map (\b -> if b then '#' else '.'))
-}

part2 :: Arrangement -> Int
part2 = determineRoughness . buildImage
  where
    determineRoughness = (-) <$> popCount <*> seaMonsterBits

    seaMonsterBits = (* popCount seaMonster) . countAllMatches seaMonster

    countAllMatches pat txt = maximum do
      (_, _, transform) <- transforms
      return . length $ findMatches (transform pat) txt

buildImage :: Arrangement -> Image
buildImage =
  concatMap $
    map (concatMap (init . tail)) . init . tail . transpose . map tileImage

popCount :: Image -> Int
popCount = length . filter id . concat

seaMonster :: Image
seaMonster =
  map (== '#')
    <$> [ "                  # ",
          "#    ##    ##    ###",
          " #  #  #  #  #  #   "
        ]

implies :: Bool -> Bool -> Bool
implies True = id
implies False = const True

findMatches :: Image -> Image -> [(Int, Int)]
findMatches pat txt = do
  let xmax = length (head txt) - length (head pat)
  let ymax = length txt - length pat

  (y, txt) <- zip [0 .. ymax] $ tails txt
  (x, txt) <- zip [0 .. xmax] . transpose $ map tails txt

  guard . and . concat $ zipWith (zipWith implies) pat txt
  return (x, y)

--------------------------------------------------------------------------------

{-
printArrangement :: Arrangement -> IO ()
printArrangement = traverse_ \row -> do
  traverse_ printHeader row
  putStrLn ""
  traverse_ printLine . transpose $ map tileImage row
  where
    printHeader :: Sketch -> IO ()
    printHeader Sketch {tileId, flopped, rotation} = do
      putStr " "
      putStr (show tileId)
      putStr "["
      putStr $ case rotation of
        None -> "0"
        Quarter -> "¼"
        Half -> "½"
        ThreeQuarter -> "¾"
      putStr "]"
      putStr $ if flopped then "ᵗ" else " "
      putStr "  "
    printLine :: [[Bit]] -> IO ()
    printLine patterns = do
      traverse_ printImage patterns
      putStrLn ""
    printImage :: [Bit] -> IO ()
    printImage = putStr . (' ' :) . map (\b -> if b then '#' else '.')
-}

part1 :: Arrangement -> Int
part1 = product . corners
  where
    corners :: Arrangement -> [ID]
    corners rows = [tileId (pos rows) | pos <- [head . head, head . last, last . head, last . last]]

type Arrangement = [[Sketch]]

arrange :: [Tile] -> Arrangement
arrange = head . arrangements
  where
    arrangements = squares =<< (isqrt . length)

    isqrt = round @Double @Int . sqrt . fromIntegral

    squares n ts = rows (repeat $ const True) n n ts

    rows _ 0 _ ~[] = return []
    rows aboves m n ts = do
      (r, ts) <- row aboves (const True) n ts
      rs <- rows (map ((==) . bottom) r) (m - 1) n ts
      return (r : rs)

    row _ _ 0 ts = return ([], ts)
    row ~(above : aboves) beside n ts = do
      (r, ts) <- views ts
      r <- flipsAndRotations r
      guard $ beside (left r) && above (top r)
      (rs, ts) <- row aboves (right r ==) (n - 1) ts
      return (r : rs, ts)

    views [] = []
    views (t : ts) = (t, ts) : (fmap (t :) <$> views ts)

    flipsAndRotations (tileId, tileImage) = do
      (rotation, flopped, transform) <- transforms
      return
        Sketch
          { rotation,
            flopped,
            tileId,
            tileImage = transform tileImage
          }

left, top, right, bottom :: Sketch -> [Bit]
left = map head . tileImage
top = head . tileImage
right = map last . tileImage
bottom = last . tileImage

transforms :: [(Rotation, Bool, Image -> Image)]
transforms =
  [ (None, False, id),
    (None, True, transpose),
    (Quarter, False, map reverse . transpose),
    (Quarter, True, map reverse),
    (Half, False, reverse . map reverse),
    (Half, True, reverse . map reverse . transpose),
    (ThreeQuarter, False, reverse . transpose),
    (ThreeQuarter, True, reverse)
  ]

data Sketch = Sketch
  { rotation :: Rotation,
    flopped :: Bool,
    tileId :: Int,
    tileImage :: Image
  }

data Rotation = None | Quarter | Half | ThreeQuarter

--------------------------------------------------------------------------------

parseExpected :: FilePath -> IO (Maybe Int)
parseExpected path = (Just . read . head . words <$> readFile path) <|> return Nothing

check :: Eq a => a -> Maybe a -> String
check actual = \case
  Nothing -> ansi yellow "?"
  Just expected | actual == expected -> ansi green "✓"
  _ -> ansi red "✗"
  where
    ansi :: Int -> String -> String
    ansi color text = "\x1b[" ++ show color ++ "m" ++ text ++ "\x1b[m"

    yellow = 33
    red = 31
    green = 32

--------------------------------------------------------------------------------

type Parser a = forall s. Stream s Identity Char => Parsec s () a

tiles :: Parser [Tile]
tiles = (tile `sepBy1` newline) <* eof
  where
    tile :: Parser Tile
    tile = (,) <$> tileId <* newline <*> tileImage

    tileId :: Parser ID
    tileId = string "Tile " *> (read <$> many1 digit) <* char ':'

    tileImage :: Parser Image
    tileImage = many1 bit `endBy1` newline

    bit :: Parser Bool
    bit = (False <$ char '.') <|> (True <$ char '#')

--------------------------------------------------------------------------------

type Tile = (ID, Image)

type Image = [[Bit]]

type ID = Int

type Bit = Bool
