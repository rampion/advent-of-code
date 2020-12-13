{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

module Main where

import Data.Foldable (traverse_)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    scenario <- parseScenario <$> readFile filename

    putStrLn "\tpart 1: ID of earliest bus * number of minutes you'll need to wait"
    putStrLn $ "\t" ++ show (part1 scenario)

    putStrLn "\tpart 2: earliest timestamp that results in the buses departing one per minute in order"
    putStrLn $ "\t" ++ show (part2 scenario)

--------------------------------------------------------------------------------

parseScenario :: String -> Scenario
parseScenario src = case lines src of
  [now, buses] -> (read now, map parseBus (split ',' buses))
  _ -> error $ "illegal scenario: " ++ show src

parseBus :: String -> Bus
parseBus "x" = Nothing
parseBus time = Just (read time)

split :: Eq a => a -> [a] -> [[a]]
split a as = case break (== a) as of
  (xs, _ : ys) -> xs : split a ys
  (xs, []) -> [xs]

--------------------------------------------------------------------------------

part1 :: Scenario -> Int
part1 = uncurry \now -> uncurry (*) . minimumBy (comparing fst) . map (wait now) . catMaybes
  where
    wait :: Time -> Time -> (Time, Time)
    wait now interval = ((- now) `mod` interval, interval)

--------------------------------------------------------------------------------

part2 :: Scenario -> Int
part2 = uncurry \_ -> solve . pairs
  where
    pairs :: [Maybe Time] -> [(Int, Time)]
    pairs = catMaybes . zipWith (fmap . (,)) [0 ..]

    solve :: [(Int, Time)] -> Time
    solve cps = sum [term (t `div` p) c p | (c, p) <- cps] `mod` t
      where
        t = product $ snd <$> cps

    term q c p = ((- c * inverse q p) `mod` p) * q

    inverse x y = fst $ case compare x y of
      LT -> loop 1 0 0 1 x y
      EQ -> error "assumed non-equal"
      GT -> loop 0 1 1 0 y x

    loop !c0 !c1 !d0 !d1 !x !y
      | 1 == x = (c0, c1)
      | 0 == x = error "assumed coprime"
      | otherwise = loop (d0 - q * c0) (d1 - q * c1) c0 c1 r x
      where
        (q, r) = y `quotRem` x

--------------------------------------------------------------------------------

type Scenario = (Time, [Bus])

type Time = Int

type Bus = Maybe Time
