{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module Main where

import Data.Foldable (traverse_)
import Data.List (foldl') --, scanl')
import System.Environment (getArgs)
import Prelude hiding (Either (..))

main :: IO ()
main = (getArgs >>=) . traverse_ $ \filename -> do
  putStrLn filename

  instructions <- parseInstructions <$> readFile filename

  putStrLn $ "\tpart 1: manhattan distance from current location to end of instructions"
  putStrLn $ "\t" ++ show (part1 instructions)

  putStrLn $ "\tpart 2: manhattan distance from current location to end of waypoint instructions"
  putStrLn $ "\t" ++ show (part2 instructions)

parseInstructions :: String -> [Instruction]
parseInstructions = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction (c : ns)
  | c == 'N' = Absolute (North d)
  | c == 'S' = Absolute (South d)
  | c == 'E' = Absolute (East d)
  | c == 'W' = Absolute (West d)
  | c == 'L' = Relative (Left t)
  | c == 'R' = Relative (Right t)
  | c == 'F' = Relative (Forward d)
  where
    d = read ns
    t = case d of
      90 -> Ninety
      180 -> OneEighty
      270 -> TwoSeventy
      _ -> error $ "illegal angle " ++ show d ++ ", expected turns in increments of 90 degrees"
parseInstruction line = error $ "illegal instruction " ++ show line

part1 :: [Instruction] -> Int
part1 = magnitude . absolute east

part2 :: [Instruction] -> Int
part2 = magnitude . waypoint (1, 10)

magnitude :: [Absolute] -> Int
magnitude = distance (0, 0) . foldl' move (0, 0)

distance :: Position -> Position -> Int
distance (lat, lon) (lat', lon') = abs (lat - lat') + abs (lon - lon')

move :: Position -> Absolute -> Position
move (!lat, !lon) = \case
  North dist -> (lat + dist, lon)
  South dist -> (lat - dist, lon)
  East dist -> (lat, lon + dist)
  West dist -> (lat, lon - dist)

absolute :: Heading -> [Instruction] -> [Absolute]
absolute dir = \case
  [] -> []
  Absolute inst : is -> inst : absolute dir is
  Relative (Left deg) : is -> absolute (left deg dir) is
  Relative (Right deg) : is -> absolute (right deg dir) is
  Relative (Forward dist) : is -> forward dir dist : absolute dir is
  where
    left :: Degrees -> Heading -> Heading
    left t (Compass t') = Compass $ invert t <> t'

    right :: Degrees -> Heading -> Heading
    right t (Compass t') = Compass $ t <> t'

    forward :: Heading -> Distance -> Absolute
    forward (Compass Zero) = North
    forward (Compass Ninety) = East
    forward (Compass OneEighty) = South
    forward (Compass TwoSeventy) = West

waypoint :: Position -> [Instruction] -> [Absolute]
waypoint pos = \case
  [] -> []
  Absolute inst : is -> waypoint (move pos inst) is
  Relative (Left deg) : is -> waypoint (rotate (invert deg) pos) is
  Relative (Right deg) : is -> waypoint (rotate deg pos) is
  Relative (Forward steps) : is -> North (fst pos * steps) : East (snd pos * steps) : waypoint pos is
  where
    rotate :: Degrees -> Position -> Position
    rotate Zero p = p
    rotate Ninety (lat, lon) = (- lon, lat)
    rotate OneEighty (lat, lon) = (- lat, - lon)
    rotate TwoSeventy (lat, lon) = (lon, - lat)

type Position = (Int, Int)

data Heading = Compass Degrees
  deriving (Show)

east :: Heading
east = Compass Ninety

data Instruction = Absolute Absolute | Relative Relative
  deriving (Show)

data Absolute
  = North Distance
  | South Distance
  | East Distance
  | West Distance
  deriving (Show)

data Relative
  = Left Degrees
  | Right Degrees
  | Forward Distance
  deriving (Show)

type Distance = Int

data Degrees = Zero | Ninety | OneEighty | TwoSeventy
  deriving (Show)

instance Semigroup Degrees where
  Zero <> t = t
  Ninety <> Ninety = OneEighty
  Ninety <> OneEighty = TwoSeventy
  Ninety <> TwoSeventy = Zero
  OneEighty <> OneEighty = Zero
  OneEighty <> TwoSeventy = Ninety
  TwoSeventy <> TwoSeventy = OneEighty
  t <> t' = t' <> t

instance Monoid Degrees where
  mempty = Zero

invert :: Degrees -> Degrees
invert Zero = Zero
invert Ninety = TwoSeventy
invert OneEighty = OneEighty
invert TwoSeventy = Ninety
