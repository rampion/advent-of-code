-- :! ghc --make % && %:r < %:h/input
{-# LANGUAGE ViewPatterns #-}
module Main where

main :: IO ()
main = print . sum . map (totalFuel . fuel . read) . lines =<< getContents

type Mass = Int

fuel :: Mass -> Mass
fuel m = m `div` 3 - 2

totalFuel :: Mass -> Mass
totalFuel m@(fuel -> n) | n <= 0    = m
                        | otherwise = m + totalFuel n

recursiveSum :: Num a => (a -> a) -> a -> a
recursiveSum f = sum . takeWhile (> 0) . tail . iterate f
