{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
module AdventOfCode.Day10 where

import Prelude hiding (cycle)
import AdventOfCode.Solver
import Text.Parsec hiding ((<|>))
import Control.Applicative
import Data.List (scanl', unfoldr)

data Instruction = Noop | Addx !Int
  deriving stock (Show, Eq)

data RegisterState = RegisterState { cycle :: Int, value :: Int }
  deriving stock (Show, Eq)

day10parser :: Parser [Instruction]
day10parser = instruction `endBy` newline where
  instruction = noop <|> addx
  noop = Noop <$ string "noop"
  addx = Addx <$ string "addx " <*> int
  int = read <$> (negative <|> positive)
  negative = (:) <$> char '-' <*> positive
  positive = some digit

toRegisterStates :: [Instruction] -> [RegisterState]
toRegisterStates = zipWith RegisterState [1..] . concatMap fst . scanl' step ([1],1) where
  step (_,v) = \case
    Noop -> ([v],v)
    Addx x -> ([v,x + v], x + v)

signalStrength :: RegisterState -> Int
signalStrength RegisterState{cycle,value} = cycle * value

sampleRegisterStates :: [Int] -> [RegisterState] -> [RegisterState]
sampleRegisterStates = loop undefined where
  loop r ns [] = map (\n -> r { cycle = n }) ns
  loop _ [] _ = []
  loop r ns@(n:nt) ws@(w:wt) = case compare (cycle w) n of
    LT -> loop w ns wt
    EQ -> w : loop w nt wt
    GT -> r { cycle = n } : loop r nt ws

day10part1 :: [Instruction] -> Int
day10part1 = sum . map signalStrength . sampleRegisterStates [20,60..220] . toRegisterStates

data Image = Image { fromImage :: String }
  deriving stock Eq

instance Show Image where
  show = unlines . unfoldr step . fromImage where
    step [] = Nothing
    step ps = Just (splitAt 40 ps)

day10part2 :: [Instruction] -> Image
day10part2 = Image . init . map check . toRegisterStates where
  check RegisterState{cycle,value}
      | abs (rem (cycle - 1) 40 - value) <= 1 = '#'
      | otherwise = '.'
