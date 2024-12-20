{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import System.IO
-- import Control.Concurrent (threadDelay)
-- import Data.Ratio ((%))
import Control.Monad qualified as Monad
-- import Control.Monad.Trans.State.Strict (State)
-- import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable qualified as Foldable
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
import Data.List qualified as List
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (fromGregorian)
import NeatInterpolation (text)
import SolveDay
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec hiding (State)
import Text.Parsec.Text (Parser)
import Prelude

-- $/> solveDayIO parse1 (dumpFrame "frame1912.txt" 101 103 . frame . map (jump 1912 101 103)) $ fromGregorian 2024 12 14
-- $/> solveDayIO parse1 (dumpFrame "frame1129.txt" 101 103 . frame . map (jump 1129 101 103)) $ fromGregorian 2024 12 14
-- $/> solveDayIO parse1 (dumpFrame "frame9495.txt" 101 103 . frame . map (jump 9495 101 103)) $ fromGregorian 2024 12 14
-- $/> solveDayIO parse1 (dumpFrame "frame8313.txt" 101 103 . frame . map (jump 8313 101 103)) $ fromGregorian 2024 12 14
-- $/> solveDayIO parse1 (dumpFrame "frame3006.txt" 101 103 . frame . map (jump 3006 101 103)) $ fromGregorian 2024 12 14
-- $/> solveDayIO parse1 (dumpFrame "frame9175.txt" 101 103 . frame . map (jump 9175 101 103)) $ fromGregorian 2024 12 14
main :: IO ()
main = do
  solveDay parse1 part1 $ fromGregorian 2024 12 14

part2 :: T
part2 = 6577

-- $> dumpFrame "frame.txt" 101 103 . frame $ map (jump 6577 101 103) day14
dedupe :: Eq a => [a] -> [a]
dedupe [] = []
dedupe (a:as) = a : dedupe (dropWhile (a ==) as)

-- $/> List.scanl1 max [(maxLine 101 103 (day14frame t), t) | t <- [0..10402]]
day14frame :: T -> Frame
day14frame t = frame $ map (jump t 101 103) day14

-- $/> List.maximum [(maxLine 101 103 . frame $ map (jump n 101 103) day14, n) | n <- [0..10402]]
maxLine :: X -> Y -> Frame -> Int
maxLine w h ps = maximum do
  y <- [0..h-1]
  let noline = \case
        [] -> []
        x:xs | Set.member Point{x,y} ps -> line 1 xs
             | otherwise -> noline xs

      line len = \case
        [] -> [len]
        x:xs | Set.member Point{x,y} ps -> line (len + 1) xs
             | otherwise -> len : noline xs
  noline [0..w-1]

anyOverlaps :: X -> Y -> Point -> Puzzle -> [Recur]
anyOverlaps w h t = Maybe.mapMaybe (overlaps w h t)

-- $/> take 10 $ do { ymin <- [0..99]; x <- [0..101]; anyOverlapsAll 101 103 [Point x y| y <- [ymin..ymin+3]] day14 }
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 4256 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 3903 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 1110 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 1691 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 6936 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 2565 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 4841 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 8984 101 103) day14
--
-- clustered vvvv
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 6879 101 103) day14
-- $/> dumpFrame "frame.txt" 101 103 . frame $ map (jump 6880 101 103) day14
anyOverlapsAll :: X -> Y -> [Point] -> Puzzle -> [Recur]
anyOverlapsAll w h ts puzzle =
  mergeAll $ map (anyOverlaps w h `flip` puzzle) ts

mergeAll :: [[Recur]] -> [Recur]
mergeAll = \case [] -> []; rs:rest -> loop rs rest
  where
    loop rs = \case
      [] -> rs
      rs':rest -> flip loop rest do
        r <- rs
        r' <- rs'
        Maybe.maybeToList $ merge r r'

merges :: [Recur] -> Maybe Recur
merges = \case [] -> Nothing; r:rs -> loop r rs
  where
    loop r = \case
      [] -> Just r
      r':rest -> do
        r <- merge r r'
        loop r rest

-- all of the points have a period of 10403 seconds
-- $/> length day14
-- $/> filter ((< 10403) . period) $ flip Maybe.mapMaybe day14 $ \b -> overlaps 101 103 (position b) b
-- $/> merges $ flip Maybe.mapMaybe day14 $ \b -> overlaps 101 103 (position b) b
-- $/> flip any day14 $ \b -> jump 10403 101 103 b /= b
overlaps :: X -> Y -> Point -> Robot -> Maybe Recur
overlaps w h t r = do
  ox <- overlap r.position.x r.velocity.x w t.x
  oy <- overlap r.position.y r.velocity.y h t.y
  merge ox oy

data Recur = Recur
  { offset :: T
  , period :: T
  }
  deriving (Show, Eq, Ord)

merge :: Recur -> Recur -> Maybe Recur
merge a b
  | a.period == b.period = do
      Monad.guard $ a.offset == b.offset
      pure a
  | otherwise = do
      let (q, r) = (b.offset - a.offset) `quotRem` (a.period - b.period)
      Monad.guard $ r == 0
      let period = lcm a.period b.period
      pure Recur
        { offset = wrap (a.offset + q * a.period) period
        , period
        }



-- $/> overlap 4 9 101 50
-- $/> overlap 5 7 103 51
-- $/> overlaps 101 103 (Point 50 51) (Robot (Point 4 5) (Point 9 7))

overlap :: Integral a => a -> a -> a -> a -> Maybe Recur
overlap p v d t = do
  let (g,iv,_) = newton (wrap v d) d
      (q, r) = (t - p) `quotRem` g
  Monad.guard $ r == 0
  let period = fromIntegral $ d `div` g
  pure Recur
    { offset = fromIntegral (q * wrap iv d) `rem` period
    , period
    }

newton :: Integral a => a -> a -> (a, a, a)
newton = \a b -> if a < b then loop 1 0 0 1 a b else loop 0 1 1 0 b a
  where
    loop ai bi aj bj i j
      | i == 0 = (j, aj, bj)
      | otherwise =
          let (q,r) = j `quotRem` i in
          loop (aj - q*ai) (bj - q*bi) ai bi r i

type Puzzle = [Robot]

data Robot = Robot
  { position :: Point
  , velocity :: Point
  }
  deriving (Show, Eq)

data Point = Point {x :: X, y :: Y}
  deriving (Show, Eq, Ord)

newtype X = X { getX :: Int }
  deriving newtype (Show, Eq, Ord, Num, Enum, Integral, Real)

newtype Y = Y { getY :: Int }
  deriving newtype (Show, Eq, Ord, Num, Enum, Integral, Real)

newtype T = T { getT :: Int }
  deriving newtype (Show, Eq, Ord, Num, Enum, Integral, Real)

parse1 :: Parser Puzzle
parse1 = (robot `endBy` newline) <* eof
  where
    robot = Robot <$> position <* char ' ' <*> velocity
    position = string "p=" *> coord
    velocity = string "v=" *> coord
    coord = Point <$> x <* char ',' <*> y
    x = X <$> int
    y = Y <$> int
    int = (*) <$> sign <*> digits
    sign = option 1 (-1 <$ char '-')
    digits = read <$> many1 digit

part1 :: Puzzle -> Int
part1 = safetyFactor 100 101 103

animate :: Puzzle -> IO ()
animate = Foldable.traverse_ (uncurry $ printFrame 101 103) . filter (dense (Point 50 51) . snd) . zip  [0..] . frames 101 103

dense :: Point -> Frame -> Bool
dense p set = length (component set p) > 10

component :: Frame -> Point -> Frame
component set = \p -> loop (Set.singleton p) [p] where
  loop seen = \case
    [] -> seen
    p:ps ->
      let qs = [q | q <- [p{x=p.x+1}, p{x=p.x-1}, p{y=p.y+1}, p{y=p.y+1}], Set.member q set, not (Set.member q seen)]
       in loop (Set.union seen (Set.fromList qs)) (qs <> ps)

dumpFrame :: FilePath -> X -> Y -> Frame -> IO ()
dumpFrame path w h positions = do
  withFile path WriteMode \fh ->
    hPutStrLn fh do
      List.intercalate "\n" do
        y <- [0..h-1]
        pure do
          x <- [0..w-1]
          pure if Set.member Point{x,y} positions then '#' else '.'

-- $/> printFrame 11 7 0 firstExample
printFrame :: X -> Y -> T -> Frame -> IO ()
printFrame w h n positions = do
  -- putStr $ concat  ["\n" | _ <- [1..117 :: Int]]
  -- putStr "\ESC[2J"
  -- putStr "\ESC[;H"
  print n
  putStrLn do
    List.intercalate "\n" do
      y <- [0..h-1]
      pure do
        x <- [0..w-1]
        pure if Set.member Point{x,y} positions then '#' else '.'
  -- threadDelay 80_000

frames :: X -> Y -> [Robot] -> [Frame]
frames w h = map frame . iterate (map (step w h))

frame :: [Robot] -> Frame
frame robots = Set.fromList [r.position | r <- robots]

type Frame = Set Point

step :: X -> Y -> Robot -> Robot
step w h r = r
  { position = Point
    { x = wrap (r.position.x + r.velocity.x) w
    , y = wrap (r.position.y + r.velocity.y) h
    }
  }

part1Demo :: Puzzle -> Int
part1Demo = safetyFactor 100 11 7

safetyFactor :: T -> X -> Y -> [Robot] -> Int
safetyFactor t w h = product . quadrants t w h

quadrants :: T -> X -> Y -> [Robot] -> Map Quadrant Int
quadrants t w h = Map.fromListWith (+) . (`zip` repeat 1) . Maybe.mapMaybe (quadrant w h . walk t)

jump :: T -> X -> Y -> Robot -> Robot
jump t w h r = r
  { position = Point
      { x = (r.position.x + r.velocity.x * fromIntegral t) `wrap` w
      , y = (r.position.y + r.velocity.y * fromIntegral t) `wrap` h
      }
  }

walk :: T -> Robot -> Robot
walk t r = r
  { position = Point
      { x = r.position.x + r.velocity.x * fromIntegral t
      , y = r.position.y + r.velocity.y * fromIntegral t
      }
  }

quadrant :: X -> Y -> Robot -> Maybe Quadrant
quadrant w h r = case (wrap r.position.x w `compare` (w `div` 2), wrap r.position.y h `compare` (h `div` 2)) of
  (LT, LT) -> Just NW
  (LT, GT) -> Just SW
  (GT, LT) -> Just NE
  (GT, GT) -> Just SE
  _ -> Nothing

wrap :: Integral a => a -> a -> a
wrap a b = rem (rem a b + b) b

data Quadrant = NW | NE | SW | SE
  deriving (Show, Eq, Ord)

firstExample :: Puzzle
firstExample =
  [ Robot { position = Point { x = 0, y = 4 }, velocity = Point { x = 3, y = -3 } }
  , Robot { position = Point { x = 6, y = 3 }, velocity = Point { x = -1, y = -3 } }
  , Robot { position = Point { x = 10, y = 3 }, velocity = Point { x = -1, y = 2 } }
  , Robot { position = Point { x = 2, y = 0 }, velocity = Point { x = 2, y = -1 } }
  , Robot { position = Point { x = 0, y = 0 }, velocity = Point { x = 1, y = 3 } }
  , Robot { position = Point { x = 3, y = 0 }, velocity = Point { x = -2, y = -2 } }
  , Robot { position = Point { x = 7, y = 6 }, velocity = Point { x = -1, y = -3 } }
  , Robot { position = Point { x = 3, y = 0 }, velocity = Point { x = -1, y = -2 } }
  , Robot { position = Point { x = 9, y = 3 }, velocity = Point { x = 2, y = 3 } }
  , Robot { position = Point { x = 7, y = 3 }, velocity = Point { x = -1, y = 2 } }
  , Robot { position = Point { x = 2, y = 4 }, velocity = Point { x = 2, y = -3 } }
  , Robot { position = Point { x = 9, y = 5 }, velocity = Point { x = -3, y = -3 } }
  ]

runTests :: IO ()
runTests = hspec do
  it "parses the first example" do
    let raw =
          [text|
            p=0,4 v=3,-3
            p=6,3 v=-1,-3
            p=10,3 v=-1,2
            p=2,0 v=2,-1
            p=0,0 v=1,3
            p=3,0 v=-2,-2
            p=7,6 v=-1,-3
            p=3,0 v=-1,-2
            p=9,3 v=2,3
            p=7,3 v=-1,2
            p=2,4 v=2,-3
            p=9,5 v=-3,-3
          |]
            <> "\n"

    parse parse1 "first example" raw `shouldBe` Right firstExample

  it "solves part one with the first example" do
    part1Demo firstExample `shouldBe` 12

day14 :: Puzzle
day14 =
  [ Robot { position = Point { x = 32, y = 46 }, velocity = Point { x = 96, y = -70 } }
  , Robot { position = Point { x = 7, y = 74 }, velocity = Point { x = -14, y = -96 } }
  , Robot { position = Point { x = 34, y = 94 }, velocity = Point { x = 17, y = -18 } }
  , Robot { position = Point { x = 41, y = 3 }, velocity = Point { x = 67, y = 78 } }
  , Robot { position = Point { x = 6, y = 31 }, velocity = Point { x = 37, y = 11 } }
  , Robot { position = Point { x = 66, y = 47 }, velocity = Point { x = -79, y = 46 } }
  , Robot { position = Point { x = 22, y = 12 }, velocity = Point { x = -57, y = -18 } }
  , Robot { position = Point { x = 91, y = 38 }, velocity = Point { x = -46, y = 46 } }
  , Robot { position = Point { x = 77, y = 99 }, velocity = Point { x = 50, y = -76 } }
  , Robot { position = Point { x = 39, y = 71 }, velocity = Point { x = 52, y = -75 } }
  , Robot { position = Point { x = 80, y = 66 }, velocity = Point { x = -3, y = -55 } }
  , Robot { position = Point { x = 58, y = 66 }, velocity = Point { x = -60, y = 62 } }
  , Robot { position = Point { x = 3, y = 24 }, velocity = Point { x = 96, y = 45 } }
  , Robot { position = Point { x = 87, y = 80 }, velocity = Point { x = -20, y = -96 } }
  , Robot { position = Point { x = 57, y = 70 }, velocity = Point { x = -26, y = -83 } }
  , Robot { position = Point { x = 67, y = 45 }, velocity = Point { x = 82, y = 47 } }
  , Robot { position = Point { x = 27, y = 82 }, velocity = Point { x = 96, y = -62 } }
  , Robot { position = Point { x = 96, y = 73 }, velocity = Point { x = 88, y = 83 } }
  , Robot { position = Point { x = 52, y = 3 }, velocity = Point { x = -13, y = -37 } }
  , Robot { position = Point { x = 100, y = 18 }, velocity = Point { x = -44, y = -21 } }
  , Robot { position = Point { x = 74, y = 70 }, velocity = Point { x = 15, y = 78 } }
  , Robot { position = Point { x = 81, y = 78 }, velocity = Point { x = 78, y = 46 } }
  , Robot { position = Point { x = 32, y = 67 }, velocity = Point { x = 64, y = 68 } }
  , Robot { position = Point { x = 86, y = 25 }, velocity = Point { x = 55, y = 92 } }
  , Robot { position = Point { x = 100, y = 83 }, velocity = Point { x = -70, y = 56 } }
  , Robot { position = Point { x = 94, y = 9 }, velocity = Point { x = -76, y = -18 } }
  , Robot { position = Point { x = 43, y = 71 }, velocity = Point { x = -33, y = -56 } }
  , Robot { position = Point { x = 29, y = 27 }, velocity = Point { x = -17, y = -57 } }
  , Robot { position = Point { x = 59, y = 27 }, velocity = Point { x = 66, y = 52 } }
  , Robot { position = Point { x = 3, y = 52 }, velocity = Point { x = 78, y = 90 } }
  , Robot { position = Point { x = 21, y = 101 }, velocity = Point { x = 10, y = 9 } }
  , Robot { position = Point { x = 50, y = 10 }, velocity = Point { x = -51, y = -18 } }
  , Robot { position = Point { x = 25, y = 86 }, velocity = Point { x = 16, y = 47 } }
  , Robot { position = Point { x = 88, y = 39 }, velocity = Point { x = -88, y = -16 } }
  , Robot { position = Point { x = 64, y = 58 }, velocity = Point { x = -36, y = 88 } }
  , Robot { position = Point { x = 3, y = 73 }, velocity = Point { x = -46, y = 82 } }
  , Robot { position = Point { x = 77, y = 61 }, velocity = Point { x = -37, y = -35 } }
  , Robot { position = Point { x = 89, y = 8 }, velocity = Point { x = 55, y = 72 } }
  , Robot { position = Point { x = 10, y = 81 }, velocity = Point { x = -81, y = -61 } }
  , Robot { position = Point { x = 91, y = 61 }, velocity = Point { x = 55, y = 48 } }
  , Robot { position = Point { x = 11, y = 9 }, velocity = Point { x = -22, y = -39 } }
  , Robot { position = Point { x = 25, y = 16 }, velocity = Point { x = 87, y = -84 } }
  , Robot { position = Point { x = 18, y = 22 }, velocity = Point { x = -6, y = 79 } }
  , Robot { position = Point { x = 11, y = 27 }, velocity = Point { x = 35, y = -38 } }
  , Robot { position = Point { x = 35, y = 77 }, velocity = Point { x = 92, y = 68 } }
  , Robot { position = Point { x = 63, y = 100 }, velocity = Point { x = 91, y = -26 } }
  , Robot { position = Point { x = 8, y = 13 }, velocity = Point { x = -39, y = -4 } }
  , Robot { position = Point { x = 32, y = 96 }, velocity = Point { x = 26, y = -60 } }
  , Robot { position = Point { x = 0, y = 1 }, velocity = Point { x = -98, y = 71 } }
  , Robot { position = Point { x = 26, y = 44 }, velocity = Point { x = 68, y = -16 } }
  , Robot { position = Point { x = 80, y = 72 }, velocity = Point { x = -96, y = 87 } }
  , Robot { position = Point { x = 38, y = 74 }, velocity = Point { x = 93, y = 21 } }
  , Robot { position = Point { x = 99, y = 69 }, velocity = Point { x = -76, y = 30 } }
  , Robot { position = Point { x = 15, y = 96 }, velocity = Point { x = -23, y = -87 } }
  , Robot { position = Point { x = 60, y = 18 }, velocity = Point { x = -43, y = -38 } }
  , Robot { position = Point { x = 77, y = 97 }, velocity = Point { x = -79, y = 36 } }
  , Robot { position = Point { x = 98, y = 41 }, velocity = Point { x = 37, y = -71 } }
  , Robot { position = Point { x = 18, y = 7 }, velocity = Point { x = 78, y = -74 } }
  , Robot { position = Point { x = 98, y = 48 }, velocity = Point { x = 5, y = 80 } }
  , Robot { position = Point { x = 18, y = 100 }, velocity = Point { x = -66, y = -81 } }
  , Robot { position = Point { x = 23, y = 87 }, velocity = Point { x = -49, y = -94 } }
  , Robot { position = Point { x = 57, y = 78 }, velocity = Point { x = -44, y = -83 } }
  , Robot { position = Point { x = 97, y = 8 }, velocity = Point { x = -92, y = 43 } }
  , Robot { position = Point { x = 96, y = 70 }, velocity = Point { x = 64, y = 72 } }
  , Robot { position = Point { x = 7, y = 28 }, velocity = Point { x = -65, y = -52 } }
  , Robot { position = Point { x = 55, y = 80 }, velocity = Point { x = -10, y = -89 } }
  , Robot { position = Point { x = 66, y = 76 }, velocity = Point { x = -27, y = -61 } }
  , Robot { position = Point { x = 72, y = 97 }, velocity = Point { x = 72, y = -73 } }
  , Robot { position = Point { x = 94, y = 97 }, velocity = Point { x = 87, y = -12 } }
  , Robot { position = Point { x = 45, y = 83 }, velocity = Point { x = -9, y = -20 } }
  , Robot { position = Point { x = 6, y = 99 }, velocity = Point { x = -39, y = -88 } }
  , Robot { position = Point { x = 74, y = 14 }, velocity = Point { x = 73, y = -45 } }
  , Robot { position = Point { x = 3, y = 102 }, velocity = Point { x = -39, y = -60 } }
  , Robot { position = Point { x = 1, y = 102 }, velocity = Point { x = 37, y = 43 } }
  , Robot { position = Point { x = 7, y = 72 }, velocity = Point { x = -37, y = 29 } }
  , Robot { position = Point { x = 34, y = 17 }, velocity = Point { x = -85, y = 86 } }
  , Robot { position = Point { x = 61, y = 66 }, velocity = Point { x = -4, y = 25 } }
  , Robot { position = Point { x = 1, y = 78 }, velocity = Point { x = -22, y = 56 } }
  , Robot { position = Point { x = 10, y = 14 }, velocity = Point { x = -15, y = 30 } }
  , Robot { position = Point { x = 79, y = 57 }, velocity = Point { x = -28, y = 81 } }
  , Robot { position = Point { x = 7, y = 87 }, velocity = Point { x = -22, y = -74 } }
  , Robot { position = Point { x = 88, y = 101 }, velocity = Point { x = 46, y = -57 } }
  , Robot { position = Point { x = 6, y = 80 }, velocity = Point { x = 11, y = 28 } }
  , Robot { position = Point { x = 47, y = 97 }, velocity = Point { x = 44, y = -21 } }
  , Robot { position = Point { x = 71, y = 74 }, velocity = Point { x = 47, y = -57 } }
  , Robot { position = Point { x = 48, y = 77 }, velocity = Point { x = 45, y = 82 } }
  , Robot { position = Point { x = 42, y = 37 }, velocity = Point { x = 42, y = -85 } }
  , Robot { position = Point { x = 83, y = 28 }, velocity = Point { x = -71, y = -85 } }
  , Robot { position = Point { x = 84, y = 60 }, velocity = Point { x = 71, y = 81 } }
  , Robot { position = Point { x = 14, y = 83 }, velocity = Point { x = -85, y = -79 } }
  , Robot { position = Point { x = 52, y = 27 }, velocity = Point { x = -8, y = 38 } }
  , Robot { position = Point { x = 90, y = 45 }, velocity = Point { x = -28, y = 67 } }
  , Robot { position = Point { x = 79, y = 0 }, velocity = Point { x = 15, y = -47 } }
  , Robot { position = Point { x = 92, y = 41 }, velocity = Point { x = -54, y = -78 } }
  , Robot { position = Point { x = 63, y = 29 }, velocity = Point { x = 74, y = -99 } }
  , Robot { position = Point { x = 24, y = 97 }, velocity = Point { x = -99, y = 22 } }
  , Robot { position = Point { x = 41, y = 52 }, velocity = Point { x = 34, y = -63 } }
  , Robot { position = Point { x = 38, y = 29 }, velocity = Point { x = 50, y = 3 } }
  , Robot { position = Point { x = 50, y = 32 }, velocity = Point { x = -33, y = -56 } }
  , Robot { position = Point { x = 11, y = 26 }, velocity = Point { x = 95, y = 25 } }
  , Robot { position = Point { x = 88, y = 49 }, velocity = Point { x = 90, y = -56 } }
  , Robot { position = Point { x = 72, y = 18 }, velocity = Point { x = -54, y = -78 } }
  , Robot { position = Point { x = 16, y = 43 }, velocity = Point { x = -57, y = 12 } }
  , Robot { position = Point { x = 33, y = 53 }, velocity = Point { x = 56, y = 26 } }
  , Robot { position = Point { x = 13, y = 91 }, velocity = Point { x = -14, y = 42 } }
  , Robot { position = Point { x = 97, y = 37 }, velocity = Point { x = 71, y = 39 } }
  , Robot { position = Point { x = 43, y = 55 }, velocity = Point { x = 78, y = 7 } }
  , Robot { position = Point { x = 5, y = 20 }, velocity = Point { x = -56, y = -58 } }
  , Robot { position = Point { x = 43, y = 48 }, velocity = Point { x = -68, y = -98 } }
  , Robot { position = Point { x = 50, y = 80 }, velocity = Point { x = -9, y = -40 } }
  , Robot { position = Point { x = 88, y = 49 }, velocity = Point { x = -4, y = -77 } }
  , Robot { position = Point { x = 91, y = 62 }, velocity = Point { x = 79, y = -86 } }
  , Robot { position = Point { x = 18, y = 1 }, velocity = Point { x = -92, y = 68 } }
  , Robot { position = Point { x = 78, y = 20 }, velocity = Point { x = -37, y = 58 } }
  , Robot { position = Point { x = 89, y = 3 }, velocity = Point { x = 30, y = 98 } }
  , Robot { position = Point { x = 30, y = 46 }, velocity = Point { x = 77, y = 39 } }
  , Robot { position = Point { x = 43, y = 41 }, velocity = Point { x = -59, y = 39 } }
  , Robot { position = Point { x = 13, y = 55 }, velocity = Point { x = -48, y = 12 } }
  , Robot { position = Point { x = 98, y = 47 }, velocity = Point { x = 98, y = -92 } }
  , Robot { position = Point { x = 92, y = 9 }, velocity = Point { x = -9, y = 97 } }
  , Robot { position = Point { x = 6, y = 55 }, velocity = Point { x = -15, y = 63 } }
  , Robot { position = Point { x = 56, y = 1 }, velocity = Point { x = -94, y = 57 } }
  , Robot { position = Point { x = 54, y = 44 }, velocity = Point { x = 75, y = -98 } }
  , Robot { position = Point { x = 26, y = 61 }, velocity = Point { x = 95, y = 91 } }
  , Robot { position = Point { x = 82, y = 101 }, velocity = Point { x = -3, y = 49 } }
  , Robot { position = Point { x = 56, y = 74 }, velocity = Point { x = 7, y = -14 } }
  , Robot { position = Point { x = 76, y = 91 }, velocity = Point { x = 89, y = 23 } }
  , Robot { position = Point { x = 59, y = 27 }, velocity = Point { x = 58, y = -10 } }
  , Robot { position = Point { x = 43, y = 49 }, velocity = Point { x = 75, y = 82 } }
  , Robot { position = Point { x = 43, y = 19 }, velocity = Point { x = -37, y = 70 } }
  , Robot { position = Point { x = 55, y = 51 }, velocity = Point { x = -84, y = -28 } }
  , Robot { position = Point { x = 55, y = 54 }, velocity = Point { x = -19, y = -66 } }
  , Robot { position = Point { x = 47, y = 60 }, velocity = Point { x = -76, y = -8 } }
  , Robot { position = Point { x = 2, y = 52 }, velocity = Point { x = 29, y = 74 } }
  , Robot { position = Point { x = 48, y = 29 }, velocity = Point { x = -68, y = 38 } }
  , Robot { position = Point { x = 45, y = 44 }, velocity = Point { x = -25, y = 39 } }
  , Robot { position = Point { x = 16, y = 0 }, velocity = Point { x = -5, y = -45 } }
  , Robot { position = Point { x = 35, y = 43 }, velocity = Point { x = 34, y = 87 } }
  , Robot { position = Point { x = 53, y = 9 }, velocity = Point { x = 34, y = 3 } }
  , Robot { position = Point { x = 73, y = 62 }, velocity = Point { x = 66, y = 54 } }
  , Robot { position = Point { x = 31, y = 17 }, velocity = Point { x = 26, y = -45 } }
  , Robot { position = Point { x = 6, y = 94 }, velocity = Point { x = 27, y = 50 } }
  , Robot { position = Point { x = 30, y = 63 }, velocity = Point { x = -18, y = 84 } }
  , Robot { position = Point { x = 41, y = 33 }, velocity = Point { x = 17, y = 46 } }
  , Robot { position = Point { x = 38, y = 6 }, velocity = Point { x = -14, y = 94 } }
  , Robot { position = Point { x = 95, y = 51 }, velocity = Point { x = 13, y = 67 } }
  , Robot { position = Point { x = 14, y = 13 }, velocity = Point { x = 61, y = -44 } }
  , Robot { position = Point { x = 39, y = 21 }, velocity = Point { x = 42, y = 58 } }
  , Robot { position = Point { x = 90, y = 16 }, velocity = Point { x = 97, y = -10 } }
  , Robot { position = Point { x = 24, y = 86 }, velocity = Point { x = -19, y = -95 } }
  , Robot { position = Point { x = 18, y = 68 }, velocity = Point { x = -74, y = 75 } }
  , Robot { position = Point { x = 98, y = 41 }, velocity = Point { x = 54, y = 39 } }
  , Robot { position = Point { x = 77, y = 52 }, velocity = Point { x = -53, y = -62 } }
  , Robot { position = Point { x = 46, y = 40 }, velocity = Point { x = -52, y = -1 } }
  , Robot { position = Point { x = 46, y = 42 }, velocity = Point { x = 8, y = -70 } }
  , Robot { position = Point { x = 57, y = 0 }, velocity = Point { x = 66, y = -73 } }
  , Robot { position = Point { x = 76, y = 27 }, velocity = Point { x = 91, y = 45 } }
  , Robot { position = Point { x = 26, y = 102 }, velocity = Point { x = 93, y = 33 } }
  , Robot { position = Point { x = 82, y = 21 }, velocity = Point { x = 3, y = 88 } }
  , Robot { position = Point { x = 87, y = 32 }, velocity = Point { x = 48, y = 17 } }
  , Robot { position = Point { x = 7, y = 10 }, velocity = Point { x = 11, y = -62 } }
  , Robot { position = Point { x = 1, y = 48 }, velocity = Point { x = -5, y = -77 } }
  , Robot { position = Point { x = 48, y = 97 }, velocity = Point { x = 76, y = -40 } }
  , Robot { position = Point { x = 98, y = 9 }, velocity = Point { x = 88, y = -39 } }
  , Robot { position = Point { x = 88, y = 21 }, velocity = Point { x = 90, y = 25 } }
  , Robot { position = Point { x = 43, y = 89 }, velocity = Point { x = -21, y = 18 } }
  , Robot { position = Point { x = 3, y = 79 }, velocity = Point { x = 70, y = -96 } }
  , Robot { position = Point { x = 96, y = 5 }, velocity = Point { x = 35, y = -84 } }
  , Robot { position = Point { x = 73, y = 13 }, velocity = Point { x = 40, y = -44 } }
  , Robot { position = Point { x = 27, y = 88 }, velocity = Point { x = 22, y = 50 } }
  , Robot { position = Point { x = 29, y = 44 }, velocity = Point { x = -17, y = 19 } }
  , Robot { position = Point { x = 18, y = 4 }, velocity = Point { x = 2, y = 37 } }
  , Robot { position = Point { x = 22, y = 9 }, velocity = Point { x = 93, y = 30 } }
  , Robot { position = Point { x = 62, y = 84 }, velocity = Point { x = -78, y = 35 } }
  , Robot { position = Point { x = 17, y = 100 }, velocity = Point { x = -15, y = 9 } }
  , Robot { position = Point { x = 67, y = 5 }, velocity = Point { x = 43, y = -16 } }
  , Robot { position = Point { x = 100, y = 38 }, velocity = Point { x = 75, y = 68 } }
  , Robot { position = Point { x = 88, y = 16 }, velocity = Point { x = 59, y = -19 } }
  , Robot { position = Point { x = 100, y = 83 }, velocity = Point { x = -79, y = 4 } }
  , Robot { position = Point { x = 53, y = 88 }, velocity = Point { x = 8, y = -68 } }
  , Robot { position = Point { x = 90, y = 71 }, velocity = Point { x = -97, y = 89 } }
  , Robot { position = Point { x = 11, y = 2 }, velocity = Point { x = 70, y = 37 } }
  , Robot { position = Point { x = 77, y = 6 }, velocity = Point { x = -2, y = 69 } }
  , Robot { position = Point { x = 58, y = 19 }, velocity = Point { x = -86, y = -5 } }
  , Robot { position = Point { x = 79, y = 95 }, velocity = Point { x = -68, y = 90 } }
  , Robot { position = Point { x = 7, y = 15 }, velocity = Point { x = 11, y = -93 } }
  , Robot { position = Point { x = 51, y = 49 }, velocity = Point { x = -50, y = -23 } }
  , Robot { position = Point { x = 4, y = 32 }, velocity = Point { x = -41, y = 5 } }
  , Robot { position = Point { x = 80, y = 57 }, velocity = Point { x = -44, y = -36 } }
  , Robot { position = Point { x = 97, y = 13 }, velocity = Point { x = 11, y = -53 } }
  , Robot { position = Point { x = 80, y = 68 }, velocity = Point { x = 5, y = 69 } }
  , Robot { position = Point { x = 40, y = 41 }, velocity = Point { x = -34, y = 45 } }
  , Robot { position = Point { x = 61, y = 33 }, velocity = Point { x = 64, y = -53 } }
  , Robot { position = Point { x = 65, y = 16 }, velocity = Point { x = -34, y = -95 } }
  , Robot { position = Point { x = 51, y = 43 }, velocity = Point { x = -93, y = 46 } }
  , Robot { position = Point { x = 22, y = 38 }, velocity = Point { x = -90, y = 27 } }
  , Robot { position = Point { x = 91, y = 54 }, velocity = Point { x = 65, y = 28 } }
  , Robot { position = Point { x = 86, y = 39 }, velocity = Point { x = -47, y = -84 } }
  , Robot { position = Point { x = 66, y = 35 }, velocity = Point { x = 59, y = -16 } }
  , Robot { position = Point { x = 41, y = 85 }, velocity = Point { x = 34, y = 63 } }
  , Robot { position = Point { x = 46, y = 15 }, velocity = Point { x = -51, y = 72 } }
  , Robot { position = Point { x = 87, y = 0 }, velocity = Point { x = -79, y = -55 } }
  , Robot { position = Point { x = 32, y = 22 }, velocity = Point { x = -93, y = -10 } }
  , Robot { position = Point { x = 71, y = 6 }, velocity = Point { x = 56, y = -80 } }
  , Robot { position = Point { x = 12, y = 40 }, velocity = Point { x = -64, y = 21 } }
  , Robot { position = Point { x = 39, y = 16 }, velocity = Point { x = -8, y = 65 } }
  , Robot { position = Point { x = 66, y = 69 }, velocity = Point { x = 99, y = 82 } }
  , Robot { position = Point { x = 87, y = 29 }, velocity = Point { x = -62, y = -91 } }
  , Robot { position = Point { x = 27, y = 41 }, velocity = Point { x = -91, y = -91 } }
  , Robot { position = Point { x = 99, y = 100 }, velocity = Point { x = 38, y = 29 } }
  , Robot { position = Point { x = 57, y = 10 }, velocity = Point { x = 16, y = 92 } }
  , Robot { position = Point { x = 55, y = 88 }, velocity = Point { x = -17, y = -39 } }
  , Robot { position = Point { x = 37, y = 18 }, velocity = Point { x = -50, y = -17 } }
  , Robot { position = Point { x = 72, y = 73 }, velocity = Point { x = -11, y = 35 } }
  , Robot { position = Point { x = 95, y = 64 }, velocity = Point { x = -63, y = 83 } }
  , Robot { position = Point { x = 34, y = 56 }, velocity = Point { x = 25, y = -81 } }
  , Robot { position = Point { x = 48, y = 35 }, velocity = Point { x = 18, y = 57 } }
  , Robot { position = Point { x = 10, y = 99 }, velocity = Point { x = -10, y = -52 } }
  , Robot { position = Point { x = 17, y = 75 }, velocity = Point { x = 78, y = 48 } }
  , Robot { position = Point { x = 68, y = 90 }, velocity = Point { x = 58, y = 15 } }
  , Robot { position = Point { x = 45, y = 100 }, velocity = Point { x = -12, y = 1 } }
  , Robot { position = Point { x = 94, y = 9 }, velocity = Point { x = -4, y = 85 } }
  , Robot { position = Point { x = 1, y = 52 }, velocity = Point { x = 13, y = -62 } }
  , Robot { position = Point { x = 63, y = 82 }, velocity = Point { x = 79, y = 94 } }
  , Robot { position = Point { x = 98, y = 56 }, velocity = Point { x = -5, y = -15 } }
  , Robot { position = Point { x = 39, y = 67 }, velocity = Point { x = 16, y = 18 } }
  , Robot { position = Point { x = 85, y = 0 }, velocity = Point { x = 81, y = 71 } }
  , Robot { position = Point { x = 45, y = 22 }, velocity = Point { x = 84, y = 24 } }
  , Robot { position = Point { x = 100, y = 58 }, velocity = Point { x = -65, y = -48 } }
  , Robot { position = Point { x = 6, y = 67 }, velocity = Point { x = -74, y = 6 } }
  , Robot { position = Point { x = 77, y = 10 }, velocity = Point { x = -77, y = 37 } }
  , Robot { position = Point { x = 80, y = 74 }, velocity = Point { x = -37, y = -48 } }
  , Robot { position = Point { x = 47, y = 86 }, velocity = Point { x = -69, y = -46 } }
  , Robot { position = Point { x = 21, y = 61 }, velocity = Point { x = -15, y = 34 } }
  , Robot { position = Point { x = 22, y = 73 }, velocity = Point { x = -57, y = 69 } }
  , Robot { position = Point { x = 54, y = 52 }, velocity = Point { x = 58, y = 40 } }
  , Robot { position = Point { x = 90, y = 52 }, velocity = Point { x = -57, y = -12 } }
  , Robot { position = Point { x = 96, y = 98 }, velocity = Point { x = 18, y = -61 } }
  , Robot { position = Point { x = 25, y = 16 }, velocity = Point { x = -49, y = -10 } }
  , Robot { position = Point { x = 78, y = 23 }, velocity = Point { x = -3, y = 72 } }
  , Robot { position = Point { x = 42, y = 65 }, velocity = Point { x = -42, y = -89 } }
  , Robot { position = Point { x = 88, y = 92 }, velocity = Point { x = -88, y = -13 } }
  , Robot { position = Point { x = 39, y = 95 }, velocity = Point { x = -94, y = -40 } }
  , Robot { position = Point { x = 30, y = 1 }, velocity = Point { x = 12, y = 32 } }
  , Robot { position = Point { x = 88, y = 82 }, velocity = Point { x = -64, y = 42 } }
  , Robot { position = Point { x = 80, y = 19 }, velocity = Point { x = -14, y = 62 } }
  , Robot { position = Point { x = 49, y = 56 }, velocity = Point { x = -21, y = 87 } }
  , Robot { position = Point { x = 71, y = 17 }, velocity = Point { x = 32, y = 67 } }
  , Robot { position = Point { x = 75, y = 88 }, velocity = Point { x = -45, y = -68 } }
  , Robot { position = Point { x = 14, y = 48 }, velocity = Point { x = 53, y = -57 } }
  , Robot { position = Point { x = 29, y = 78 }, velocity = Point { x = 77, y = -83 } }
  , Robot { position = Point { x = 79, y = 0 }, velocity = Point { x = 22, y = -37 } }
  , Robot { position = Point { x = 42, y = 96 }, velocity = Point { x = 20, y = -47 } }
  , Robot { position = Point { x = 76, y = 55 }, velocity = Point { x = 38, y = -2 } }
  , Robot { position = Point { x = 99, y = 40 }, velocity = Point { x = 96, y = 18 } }
  , Robot { position = Point { x = 39, y = 60 }, velocity = Point { x = 2, y = 77 } }
  , Robot { position = Point { x = 20, y = 30 }, velocity = Point { x = 35, y = -51 } }
  , Robot { position = Point { x = 50, y = 27 }, velocity = Point { x = 58, y = 18 } }
  , Robot { position = Point { x = 15, y = 82 }, velocity = Point { x = 19, y = 49 } }
  , Robot { position = Point { x = 67, y = 5 }, velocity = Point { x = 10, y = -56 } }
  , Robot { position = Point { x = 75, y = 59 }, velocity = Point { x = 32, y = -89 } }
  , Robot { position = Point { x = 66, y = 69 }, velocity = Point { x = 39, y = 46 } }
  , Robot { position = Point { x = 24, y = 75 }, velocity = Point { x = 25, y = -67 } }
  , Robot { position = Point { x = 26, y = 22 }, velocity = Point { x = -75, y = -64 } }
  , Robot { position = Point { x = 21, y = 90 }, velocity = Point { x = 36, y = 90 } }
  , Robot { position = Point { x = 47, y = 61 }, velocity = Point { x = 17, y = 47 } }
  , Robot { position = Point { x = 54, y = 95 }, velocity = Point { x = -98, y = 69 } }
  , Robot { position = Point { x = 57, y = 81 }, velocity = Point { x = 75, y = 83 } }
  , Robot { position = Point { x = 66, y = 59 }, velocity = Point { x = 15, y = 82 } }
  , Robot { position = Point { x = 66, y = 90 }, velocity = Point { x = 27, y = 62 } }
  , Robot { position = Point { x = 79, y = 85 }, velocity = Point { x = -86, y = -89 } }
  , Robot { position = Point { x = 33, y = 93 }, velocity = Point { x = -34, y = 30 } }
  , Robot { position = Point { x = 99, y = 97 }, velocity = Point { x = 80, y = -59 } }
  , Robot { position = Point { x = 75, y = 42 }, velocity = Point { x = 97, y = -70 } }
  , Robot { position = Point { x = 27, y = 60 }, velocity = Point { x = 35, y = 6 } }
  , Robot { position = Point { x = 70, y = 23 }, velocity = Point { x = 48, y = -44 } }
  , Robot { position = Point { x = 64, y = 40 }, velocity = Point { x = 92, y = 66 } }
  , Robot { position = Point { x = 60, y = 56 }, velocity = Point { x = 73, y = 60 } }
  , Robot { position = Point { x = 26, y = 23 }, velocity = Point { x = 43, y = -3 } }
  , Robot { position = Point { x = 30, y = 3 }, velocity = Point { x = 60, y = 24 } }
  , Robot { position = Point { x = 70, y = 87 }, velocity = Point { x = -62, y = -75 } }
  , Robot { position = Point { x = 30, y = 28 }, velocity = Point { x = 1, y = 31 } }
  , Robot { position = Point { x = 15, y = 94 }, velocity = Point { x = -23, y = -5 } }
  , Robot { position = Point { x = 8, y = 83 }, velocity = Point { x = -17, y = 55 } }
  , Robot { position = Point { x = 88, y = 50 }, velocity = Point { x = 55, y = -2 } }
  , Robot { position = Point { x = 68, y = 10 }, velocity = Point { x = -11, y = 71 } }
  , Robot { position = Point { x = 49, y = 93 }, velocity = Point { x = 68, y = 56 } }
  , Robot { position = Point { x = 98, y = 64 }, velocity = Point { x = 53, y = -41 } }
  , Robot { position = Point { x = 12, y = 10 }, velocity = Point { x = -24, y = -53 } }
  , Robot { position = Point { x = 58, y = 11 }, velocity = Point { x = 41, y = 85 } }
  , Robot { position = Point { x = 49, y = 43 }, velocity = Point { x = -86, y = 95 } }
  , Robot { position = Point { x = 47, y = 75 }, velocity = Point { x = -48, y = -38 } }
  , Robot { position = Point { x = 37, y = 14 }, velocity = Point { x = -63, y = -70 } }
  , Robot { position = Point { x = 64, y = 60 }, velocity = Point { x = -52, y = -56 } }
  , Robot { position = Point { x = 98, y = 91 }, velocity = Point { x = 87, y = -80 } }
  , Robot { position = Point { x = 78, y = 88 }, velocity = Point { x = -2, y = -48 } }
  , Robot { position = Point { x = 80, y = 27 }, velocity = Point { x = 72, y = 45 } }
  , Robot { position = Point { x = 53, y = 12 }, velocity = Point { x = -67, y = 31 } }
  , Robot { position = Point { x = 20, y = 3 }, velocity = Point { x = -8, y = -9 } }
  , Robot { position = Point { x = 67, y = 12 }, velocity = Point { x = 16, y = -93 } }
  , Robot { position = Point { x = 49, y = 85 }, velocity = Point { x = -22, y = 18 } }
  , Robot { position = Point { x = 2, y = 98 }, velocity = Point { x = -18, y = -52 } }
  , Robot { position = Point { x = 47, y = 101 }, velocity = Point { x = -13, y = 17 } }
  , Robot { position = Point { x = 46, y = 72 }, velocity = Point { x = -59, y = -41 } }
  , Robot { position = Point { x = 21, y = 22 }, velocity = Point { x = -33, y = -73 } }
  , Robot { position = Point { x = 21, y = 37 }, velocity = Point { x = -13, y = -56 } }
  , Robot { position = Point { x = 55, y = 48 }, velocity = Point { x = -60, y = -85 } }
  , Robot { position = Point { x = 74, y = 8 }, velocity = Point { x = -45, y = -11 } }
  , Robot { position = Point { x = 0, y = 25 }, velocity = Point { x = 12, y = 72 } }
  , Robot { position = Point { x = 75, y = 19 }, velocity = Point { x = 34, y = 48 } }
  , Robot { position = Point { x = 55, y = 1 }, velocity = Point { x = -18, y = 91 } }
  , Robot { position = Point { x = 69, y = 79 }, velocity = Point { x = -94, y = -19 } }
  , Robot { position = Point { x = 24, y = 84 }, velocity = Point { x = 52, y = -96 } }
  , Robot { position = Point { x = 68, y = 9 }, velocity = Point { x = 82, y = -4 } }
  , Robot { position = Point { x = 89, y = 56 }, velocity = Point { x = -12, y = 6 } }
  , Robot { position = Point { x = 99, y = 49 }, velocity = Point { x = 89, y = -30 } }
  , Robot { position = Point { x = 88, y = 9 }, velocity = Point { x = 47, y = -25 } }
  , Robot { position = Point { x = 62, y = 24 }, velocity = Point { x = -16, y = 35 } }
  , Robot { position = Point { x = 22, y = 73 }, velocity = Point { x = 93, y = 89 } }
  , Robot { position = Point { x = 1, y = 86 }, velocity = Point { x = -13, y = 28 } }
  , Robot { position = Point { x = 99, y = 97 }, velocity = Point { x = -89, y = -33 } }
  , Robot { position = Point { x = 95, y = 68 }, velocity = Point { x = -13, y = 55 } }
  , Robot { position = Point { x = 33, y = 24 }, velocity = Point { x = 93, y = 45 } }
  , Robot { position = Point { x = 70, y = 55 }, velocity = Point { x = 48, y = -8 } }
  , Robot { position = Point { x = 42, y = 9 }, velocity = Point { x = -24, y = 16 } }
  , Robot { position = Point { x = 9, y = 90 }, velocity = Point { x = 52, y = -40 } }
  , Robot { position = Point { x = 7, y = 71 }, velocity = Point { x = 26, y = 56 } }
  , Robot { position = Point { x = 52, y = 12 }, velocity = Point { x = 24, y = 23 } }
  , Robot { position = Point { x = 0, y = 66 }, velocity = Point { x = -35, y = 93 } }
  , Robot { position = Point { x = 49, y = 72 }, velocity = Point { x = -42, y = -7 } }
  , Robot { position = Point { x = 65, y = 32 }, velocity = Point { x = 52, y = -92 } }
  , Robot { position = Point { x = 74, y = 28 }, velocity = Point { x = 7, y = -58 } }
  , Robot { position = Point { x = 64, y = 12 }, velocity = Point { x = -59, y = 90 } }
  , Robot { position = Point { x = 100, y = 28 }, velocity = Point { x = 88, y = 45 } }
  , Robot { position = Point { x = 83, y = 57 }, velocity = Point { x = -50, y = 18 } }
  , Robot { position = Point { x = 81, y = 25 }, velocity = Point { x = -79, y = 66 } }
  , Robot { position = Point { x = 29, y = 93 }, velocity = Point { x = -65, y = 43 } }
  , Robot { position = Point { x = 83, y = 63 }, velocity = Point { x = -79, y = 34 } }
  , Robot { position = Point { x = 88, y = 18 }, velocity = Point { x = 5, y = -31 } }
  , Robot { position = Point { x = 62, y = 11 }, velocity = Point { x = -36, y = 37 } }
  , Robot { position = Point { x = 33, y = 63 }, velocity = Point { x = 51, y = 75 } }
  , Robot { position = Point { x = 28, y = 51 }, velocity = Point { x = 27, y = 80 } }
  , Robot { position = Point { x = 35, y = 59 }, velocity = Point { x = 69, y = -51 } }
  , Robot { position = Point { x = 22, y = 76 }, velocity = Point { x = -82, y = -41 } }
  , Robot { position = Point { x = 35, y = 46 }, velocity = Point { x = 85, y = 46 } }
  , Robot { position = Point { x = 6, y = 22 }, velocity = Point { x = 62, y = -73 } }
  , Robot { position = Point { x = 5, y = 95 }, velocity = Point { x = -81, y = -48 } }
  , Robot { position = Point { x = 52, y = 61 }, velocity = Point { x = 26, y = 69 } }
  , Robot { position = Point { x = 16, y = 4 }, velocity = Point { x = -66, y = 79 } }
  , Robot { position = Point { x = 13, y = 47 }, velocity = Point { x = -73, y = 94 } }
  , Robot { position = Point { x = 10, y = 96 }, velocity = Point { x = -65, y = -95 } }
  , Robot { position = Point { x = 72, y = 86 }, velocity = Point { x = -36, y = -26 } }
  , Robot { position = Point { x = 68, y = 45 }, velocity = Point { x = -60, y = -7 } }
  , Robot { position = Point { x = 65, y = 14 }, velocity = Point { x = -46, y = -29 } }
  , Robot { position = Point { x = 1, y = 22 }, velocity = Point { x = 19, y = -31 } }
  , Robot { position = Point { x = 80, y = 80 }, velocity = Point { x = 57, y = 84 } }
  , Robot { position = Point { x = 20, y = 69 }, velocity = Point { x = -31, y = -35 } }
  , Robot { position = Point { x = 97, y = 24 }, velocity = Point { x = 80, y = 37 } }
  , Robot { position = Point { x = 38, y = 41 }, velocity = Point { x = -59, y = -64 } }
  , Robot { position = Point { x = 8, y = 27 }, velocity = Point { x = -60, y = -94 } }
  , Robot { position = Point { x = 46, y = 81 }, velocity = Point { x = 90, y = 60 } }
  , Robot { position = Point { x = 5, y = 85 }, velocity = Point { x = 86, y = -55 } }
  , Robot { position = Point { x = 54, y = 31 }, velocity = Point { x = 94, y = 95 } }
  , Robot { position = Point { x = 29, y = 82 }, velocity = Point { x = -67, y = -69 } }
  , Robot { position = Point { x = 8, y = 21 }, velocity = Point { x = -89, y = 87 } }
  , Robot { position = Point { x = 8, y = 59 }, velocity = Point { x = -56, y = -97 } }
  , Robot { position = Point { x = 33, y = 20 }, velocity = Point { x = 79, y = -1 } }
  , Robot { position = Point { x = 62, y = 80 }, velocity = Point { x = 72, y = -47 } }
  , Robot { position = Point { x = 7, y = 7 }, velocity = Point { x = 69, y = -52 } }
  , Robot { position = Point { x = 0, y = 80 }, velocity = Point { x = 71, y = -34 } }
  , Robot { position = Point { x = 88, y = 0 }, velocity = Point { x = 99, y = -77 } }
  , Robot { position = Point { x = 53, y = 19 }, velocity = Point { x = 16, y = -45 } }
  , Robot { position = Point { x = 90, y = 85 }, velocity = Point { x = 29, y = -75 } }
  , Robot { position = Point { x = 5, y = 31 }, velocity = Point { x = 3, y = -85 } }
  , Robot { position = Point { x = 38, y = 20 }, velocity = Point { x = 7, y = -32 } }
  , Robot { position = Point { x = 85, y = 2 }, velocity = Point { x = -12, y = -87 } }
  , Robot { position = Point { x = 87, y = 64 }, velocity = Point { x = -49, y = -49 } }
  , Robot { position = Point { x = 34, y = 33 }, velocity = Point { x = 84, y = 43 } }
  , Robot { position = Point { x = 42, y = 80 }, velocity = Point { x = 77, y = 56 } }
  , Robot { position = Point { x = 68, y = 44 }, velocity = Point { x = 60, y = 90 } }
  , Robot { position = Point { x = 67, y = 94 }, velocity = Point { x = -80, y = 77 } }
  , Robot { position = Point { x = 75, y = 44 }, velocity = Point { x = 14, y = 73 } }
  , Robot { position = Point { x = 25, y = 74 }, velocity = Point { x = 69, y = -75 } }
  , Robot { position = Point { x = 54, y = 67 }, velocity = Point { x = -9, y = 96 } }
  , Robot { position = Point { x = 34, y = 100 }, velocity = Point { x = 1, y = 9 } }
  , Robot { position = Point { x = 83, y = 73 }, velocity = Point { x = 98, y = -86 } }
  , Robot { position = Point { x = 58, y = 56 }, velocity = Point { x = -31, y = -40 } }
  , Robot { position = Point { x = 16, y = 21 }, velocity = Point { x = -82, y = 93 } }
  , Robot { position = Point { x = 75, y = 58 }, velocity = Point { x = 73, y = -22 } }
  , Robot { position = Point { x = 61, y = 9 }, velocity = Point { x = 32, y = -67 } }
  , Robot { position = Point { x = 15, y = 7 }, velocity = Point { x = 27, y = 9 } }
  , Robot { position = Point { x = 14, y = 77 }, velocity = Point { x = -48, y = 49 } }
  , Robot { position = Point { x = 48, y = 20 }, velocity = Point { x = 84, y = -24 } }
  , Robot { position = Point { x = 93, y = 55 }, velocity = Point { x = -12, y = 11 } }
  , Robot { position = Point { x = 95, y = 78 }, velocity = Point { x = -54, y = -83 } }
  , Robot { position = Point { x = 72, y = 33 }, velocity = Point { x = 6, y = -23 } }
  , Robot { position = Point { x = 50, y = 88 }, velocity = Point { x = 97, y = -73 } }
  , Robot { position = Point { x = 97, y = 67 }, velocity = Point { x = 79, y = -23 } }
  , Robot { position = Point { x = 27, y = 76 }, velocity = Point { x = 94, y = -18 } }
  , Robot { position = Point { x = 99, y = 67 }, velocity = Point { x = -54, y = -56 } }
  , Robot { position = Point { x = 79, y = 3 }, velocity = Point { x = 39, y = 24 } }
  , Robot { position = Point { x = 32, y = 22 }, velocity = Point { x = 9, y = 59 } }
  , Robot { position = Point { x = 89, y = 1 }, velocity = Point { x = -63, y = 36 } }
  , Robot { position = Point { x = 52, y = 82 }, velocity = Point { x = -51, y = -36 } }
  , Robot { position = Point { x = 39, y = 70 }, velocity = Point { x = 93, y = 1 } }
  , Robot { position = Point { x = 6, y = 91 }, velocity = Point { x = 56, y = 6 } }
  , Robot { position = Point { x = 6, y = 53 }, velocity = Point { x = -85, y = -37 } }
  , Robot { position = Point { x = 24, y = 80 }, velocity = Point { x = -66, y = 56 } }
  , Robot { position = Point { x = 30, y = 91 }, velocity = Point { x = -21, y = -11 } }
  , Robot { position = Point { x = 99, y = 18 }, velocity = Point { x = -4, y = 99 } }
  , Robot { position = Point { x = 58, y = 53 }, velocity = Point { x = -18, y = 33 } }
  , Robot { position = Point { x = 36, y = 51 }, velocity = Point { x = -8, y = -29 } }
  , Robot { position = Point { x = 0, y = 87 }, velocity = Point { x = 46, y = -6 } }
  , Robot { position = Point { x = 44, y = 7 }, velocity = Point { x = 7, y = 86 } }
  , Robot { position = Point { x = 13, y = 34 }, velocity = Point { x = -65, y = -9 } }
  , Robot { position = Point { x = 28, y = 6 }, velocity = Point { x = 60, y = -59 } }
  , Robot { position = Point { x = 97, y = 62 }, velocity = Point { x = 85, y = -76 } }
  , Robot { position = Point { x = 47, y = 11 }, velocity = Point { x = -43, y = 44 } }
  , Robot { position = Point { x = 11, y = 56 }, velocity = Point { x = 53, y = -29 } }
  , Robot { position = Point { x = 25, y = 91 }, velocity = Point { x = 95, y = 23 } }
  , Robot { position = Point { x = 91, y = 49 }, velocity = Point { x = 52, y = -57 } }
  , Robot { position = Point { x = 84, y = 22 }, velocity = Point { x = -20, y = -38 } }
  , Robot { position = Point { x = 31, y = 8 }, velocity = Point { x = -50, y = -10 } }
  , Robot { position = Point { x = 1, y = 70 }, velocity = Point { x = -30, y = 1 } }
  , Robot { position = Point { x = 65, y = 78 }, velocity = Point { x = 23, y = 7 } }
  , Robot { position = Point { x = 57, y = 6 }, velocity = Point { x = -10, y = -80 } }
  , Robot { position = Point { x = 86, y = 93 }, velocity = Point { x = 48, y = -55 } }
  , Robot { position = Point { x = 1, y = 98 }, velocity = Point { x = 55, y = 90 } }
  , Robot { position = Point { x = 39, y = 59 }, velocity = Point { x = 52, y = 48 } }
  , Robot { position = Point { x = 13, y = 21 }, velocity = Point { x = 80, y = -96 } }
  , Robot { position = Point { x = 30, y = 28 }, velocity = Point { x = -36, y = 37 } }
  , Robot { position = Point { x = 94, y = 92 }, velocity = Point { x = 29, y = -19 } }
  , Robot { position = Point { x = 44, y = 25 }, velocity = Point { x = 92, y = -65 } }
  , Robot { position = Point { x = 61, y = 56 }, velocity = Point { x = -49, y = -70 } }
  , Robot { position = Point { x = 54, y = 3 }, velocity = Point { x = 16, y = 37 } }
  , Robot { position = Point { x = 53, y = 58 }, velocity = Point { x = -43, y = 96 } }
  , Robot { position = Point { x = 98, y = 36 }, velocity = Point { x = 97, y = -24 } }
  , Robot { position = Point { x = 57, y = 25 }, velocity = Point { x = 43, y = 30 } }
  , Robot { position = Point { x = 15, y = 73 }, velocity = Point { x = 16, y = 4 } }
  , Robot { position = Point { x = 72, y = 80 }, velocity = Point { x = 48, y = -41 } }
  , Robot { position = Point { x = 79, y = 50 }, velocity = Point { x = 97, y = 25 } }
  , Robot { position = Point { x = 79, y = 74 }, velocity = Point { x = -66, y = 86 } }
  , Robot { position = Point { x = 69, y = 78 }, velocity = Point { x = 47, y = 48 } }
  , Robot { position = Point { x = 73, y = 13 }, velocity = Point { x = -61, y = 92 } }
  , Robot { position = Point { x = 5, y = 85 }, velocity = Point { x = 36, y = -27 } }
  , Robot { position = Point { x = 29, y = 98 }, velocity = Point { x = 43, y = -13 } }
  , Robot { position = Point { x = 1, y = 96 }, velocity = Point { x = 46, y = 83 } }
  , Robot { position = Point { x = 26, y = 53 }, velocity = Point { x = 69, y = 95 } }
  , Robot { position = Point { x = 87, y = 91 }, velocity = Point { x = 38, y = -26 } }
  , Robot { position = Point { x = 8, y = 90 }, velocity = Point { x = -91, y = 8 } }
  , Robot { position = Point { x = 30, y = 4 }, velocity = Point { x = 3, y = 19 } }
  , Robot { position = Point { x = 53, y = 75 }, velocity = Point { x = -19, y = -13 } }
  , Robot { position = Point { x = 91, y = 80 }, velocity = Point { x = 51, y = 74 } }
  , Robot { position = Point { x = 7, y = 84 }, velocity = Point { x = 17, y = 87 } }
  , Robot { position = Point { x = 61, y = 52 }, velocity = Point { x = 35, y = 32 } }
  , Robot { position = Point { x = 45, y = 22 }, velocity = Point { x = 49, y = -86 } }
  , Robot { position = Point { x = 23, y = 86 }, velocity = Point { x = 87, y = -67 } }
  , Robot { position = Point { x = 20, y = 11 }, velocity = Point { x = -57, y = -18 } }
  , Robot { position = Point { x = 29, y = 37 }, velocity = Point { x = 26, y = 60 } }
  , Robot { position = Point { x = 56, y = 38 }, velocity = Point { x = -94, y = -16 } }
  , Robot { position = Point { x = 6, y = 20 }, velocity = Point { x = 2, y = 83 } }
  , Robot { position = Point { x = 32, y = 63 }, velocity = Point { x = -24, y = 6 } }
  , Robot { position = Point { x = 29, y = 67 }, velocity = Point { x = 43, y = -83 } }
  , Robot { position = Point { x = 30, y = 33 }, velocity = Point { x = 68, y = 15 } }
  , Robot { position = Point { x = 20, y = 36 }, velocity = Point { x = -76, y = -4 } }
  , Robot { position = Point { x = 40, y = 51 }, velocity = Point { x = -34, y = -84 } }
  , Robot { position = Point { x = 98, y = 94 }, velocity = Point { x = -88, y = -27 } }
  , Robot { position = Point { x = 66, y = 34 }, velocity = Point { x = -36, y = 81 } }
  , Robot { position = Point { x = 64, y = 1 }, velocity = Point { x = -61, y = 50 } }
  , Robot { position = Point { x = 39, y = 92 }, velocity = Point { x = -59, y = -34 } }
  , Robot { position = Point { x = 16, y = 24 }, velocity = Point { x = 43, y = 4 } }
  , Robot { position = Point { x = 3, y = 44 }, velocity = Point { x = 56, y = -28 } }
  , Robot { position = Point { x = 18, y = 10 }, velocity = Point { x = -22, y = -3 } }
  , Robot { position = Point { x = 87, y = 47 }, velocity = Point { x = -53, y = 67 } }
  , Robot { position = Point { x = 28, y = 66 }, velocity = Point { x = 9, y = -42 } }
  , Robot { position = Point { x = 61, y = 71 }, velocity = Point { x = -94, y = -55 } }
  , Robot { position = Point { x = 45, y = 39 }, velocity = Point { x = 55, y = 19 } }
  , Robot { position = Point { x = 15, y = 50 }, velocity = Point { x = 16, y = -45 } }
  , Robot { position = Point { x = 42, y = 89 }, velocity = Point { x = 15, y = -69 } }
  , Robot { position = Point { x = 4, y = 42 }, velocity = Point { x = 20, y = -77 } }
  , Robot { position = Point { x = 4, y = 12 }, velocity = Point { x = 25, y = 84 } }
  , Robot { position = Point { x = 58, y = 42 }, velocity = Point { x = -52, y = 6 } }
  , Robot { position = Point { x = 43, y = 71 }, velocity = Point { x = 93, y = -19 } }
  , Robot { position = Point { x = 11, y = 83 }, velocity = Point { x = -81, y = 83 } }
  , Robot { position = Point { x = 19, y = 96 }, velocity = Point { x = 37, y = 62 } }
  , Robot { position = Point { x = 10, y = 5 }, velocity = Point { x = -56, y = -25 } }
  , Robot { position = Point { x = 98, y = 75 }, velocity = Point { x = 79, y = 89 } }
  , Robot { position = Point { x = 55, y = 6 }, velocity = Point { x = 40, y = -26 } }
  , Robot { position = Point { x = 27, y = 26 }, velocity = Point { x = 85, y = -24 } }
  , Robot { position = Point { x = 76, y = 55 }, velocity = Point { x = 73, y = 60 } }
  , Robot { position = Point { x = 13, y = 12 }, velocity = Point { x = -6, y = -32 } }
  , Robot { position = Point { x = 15, y = 76 }, velocity = Point { x = -15, y = 13 } }
  , Robot { position = Point { x = 53, y = 10 }, velocity = Point { x = -67, y = -52 } }
  , Robot { position = Point { x = 18, y = 53 }, velocity = Point { x = 69, y = -49 } }
  , Robot { position = Point { x = 65, y = 39 }, velocity = Point { x = -52, y = 73 } }
  , Robot { position = Point { x = 39, y = 80 }, velocity = Point { x = -67, y = -41 } }
  , Robot { position = Point { x = 43, y = 5 }, velocity = Point { x = -12, y = -58 } }
  , Robot { position = Point { x = 24, y = 42 }, velocity = Point { x = -7, y = -85 } }
  , Robot { position = Point { x = 12, y = 48 }, velocity = Point { x = -57, y = 46 } }
  , Robot { position = Point { x = 55, y = 98 }, velocity = Point { x = -18, y = 36 } }
  ]
