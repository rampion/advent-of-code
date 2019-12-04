module Main where

import Control.Monad (guard)
import Test.DocTest (doctest)
import Data.List (groupBy)

input :: [Int]
input = [256310..732736]

main :: IO ()
main = print . length $ do
  n <- show <$> input
  guard $ twoAdjacentSame n
  guard $ neverDecrease n
  return ()

test :: IO ()
test = doctest ["2019/day/4/part1.hs"]

-- |
-- >>> twoAdjacentSame "111111"
-- True
-- >>> twoAdjacentSame "123456"
-- False
-- >>> twoAdjacentSame "654321"
-- False
-- >>> twoAdjacentSame "557766"
-- True
twoAdjacentSame :: String -> Bool
twoAdjacentSame = any ((>=2) . length) . groupBy (==)

-- |
-- >>> neverDecrease "111111"
-- True
-- >>> neverDecrease "123456"
-- True
-- >>> neverDecrease "654321"
-- False
-- >>> neverDecrease "557766"
-- False
neverDecrease :: String -> Bool
neverDecrease as = all (uncurry (<=)) $ zip as (tail as)
