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

-- import Control.Concurrent (threadDelay)
-- import Data.Ratio ((%))
-- import Control.Monad qualified as Monad
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
import SolveDay (solveDayIO)
import Test.Hspec (hspec, it, shouldBe)
import Text.Parsec hiding (State)
import Text.Parsec.Text (Parser)
import Prelude

-- $> main

main :: IO ()
main = do
-- $> animate firstExample
  solveDayIO parse1 animate $ fromGregorian 2024 12 14

type Puzzle = [Robot Int Int]

data Robot w h = Robot
  { position :: Point w h
  , velocity :: Point w h
  }
  deriving (Show, Eq)

data Point w h = Point {x :: w, y :: h}
  deriving (Show, Eq, Ord)

parse1 :: Parser Puzzle
parse1 = (robot `endBy` newline) <* eof
  where
    robot = Robot <$> position <* char ' ' <*> velocity
    position = string "p=" *> coord
    velocity = string "v=" *> coord
    coord = Point <$> int <* char ',' <*> int
    int = (*) <$> sign <*> digits
    sign = option 1 (-1 <$ char '-')
    digits = read <$> many1 digit

part1 :: Puzzle -> Int
part1 = safetyFactor @Int 100 101 103

animate :: Puzzle -> IO ()
animate = Foldable.traverse_ (uncurry $ printFrame 101 103) . filter (dense (Point 50 51) . snd) . zip  [0..] . frames 101 103

dense :: (Integral w, Integral h) => Point w h -> Set (Point w h) -> Bool
dense p set = length (component set p) > 10

component :: (Integral w, Integral h) => Set (Point w h) -> Point w h -> Set (Point w h)
component set = \p -> loop (Set.singleton p) [p] where
  loop seen = \case
    [] -> seen
    p:ps ->
      let qs = [q | q <- [p{x=p.x+1}, p{x=p.x-1}, p{y=p.y+1}, p{y=p.y+1}], Set.member q set, not (Set.member q seen)]
       in loop (Set.union seen (Set.fromList qs)) (qs <> ps)


-- $/> printFrame 11 7 0 firstExample
printFrame :: Int -> Int -> Int -> Frame -> IO ()
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

frames :: (Integral w, Integral h) => w -> h -> [Robot w h] -> [Set (Point w h)]
frames w h = map frame . iterate (map (step w h))

frame :: (Integral w, Integral h) => [Robot w h] -> Set (Point w h)
frame robots = Set.fromList [r.position | r <- robots]

type Frame = Set (Point Int Int)

step :: (Integral w, Integral h) => w -> h -> Robot w h -> Robot w h
step w h r = r
  { position = Point
    { x = wrap (r.position.x + r.velocity.x) w
    , y = wrap (r.position.y + r.velocity.y) h
    }
  }

part1Demo :: Puzzle -> Int
part1Demo = safetyFactor @Int 100 11 7

safetyFactor :: (Integral time, Integral w, Integral h) => time -> w -> h -> [Robot w h] -> Int
safetyFactor t w h = product . quadrants t w h

quadrants :: (Integral time, Integral w, Integral h) => time -> w -> h -> [Robot w h] -> Map Quadrant Int
quadrants t w h = Map.fromListWith (+) . (`zip` repeat 1) . Maybe.mapMaybe (quadrant w h . walk t)

walk :: (Integral time, Integral w, Integral h) => time -> Robot w h -> Robot w h
walk t r = r
  { position = Point
      { x = r.position.x + r.velocity.x * fromIntegral t
      , y = r.position.y + r.velocity.y * fromIntegral t
      }
  }

quadrant :: (Integral w, Integral h) => w -> h -> Robot w h -> Maybe Quadrant
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

-- $> runTests

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
