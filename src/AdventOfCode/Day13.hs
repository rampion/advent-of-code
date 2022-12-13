{-# LANGUAGE OverloadedLists #-}
module AdventOfCode.Day13 where

import Prelude
import AdventOfCode.Solver
import Text.Parsec hiding ((<|>))
import Control.Applicative
import Data.List (sort)
import GHC.Exts

newtype Packet = Packet [PacketValue]
  deriving stock (Eq, Ord)

instance IsList Packet where
  type Item Packet = PacketValue
  fromList = Packet
  toList (Packet as) = as

instance Show Packet where
  show (Packet as) = show as

data PacketValue = IntValue Int | ListValue [PacketValue]
  deriving stock (Eq)

instance Num PacketValue where
  fromInteger = IntValue . fromInteger
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

instance IsList PacketValue where
  type Item PacketValue = PacketValue
  fromList = ListValue
  toList = \case
    ListValue as -> as
    a -> [a]

instance Show PacketValue where
  show = \case
    IntValue a -> show a
    ListValue as -> show as

instance Ord PacketValue where
  IntValue a `compare` IntValue b = a `compare` b 
  a `compare` b = Packet (toList a) `compare` Packet (toList b)

day13parser :: Parser [(Packet,Packet)]
day13parser = pair `sepBy` newline where
  pair = liftA2 (,) packet packet
  packet = Packet <$> values <* newline
  values = char '[' *> (value `sepBy` char ',') <* char ']'
  value = int <|> list
  int = IntValue . read <$> some digit
  list = ListValue <$> values

day13part1 :: [(Packet,Packet)] -> Int
day13part1 = sum . map fst . select . zip [1..] where
  select :: [(Int,(Packet,Packet))] -> [(Int,(Packet,Packet))]
  select = filter (uncurry (<) . snd)

day13part2 :: [(Packet,Packet)] -> Int
day13part2 
  = product
  . fmap fst
  . filter ((`elem` dividers) . snd)
  . zip [1..]
  . order

order :: [(Packet,Packet)] -> [Packet]
order
  = sort
  . (dividers ++)
  . concatMap fromPair
  where
    fromPair (t0,t1) = [t0,t1]

dividers :: [Packet]
dividers =
  [ [[2]]
  , [[6]]
  ]
