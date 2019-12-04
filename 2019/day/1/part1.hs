-- :! ghc --make % && %:r < %:h/input
module Main where

main :: IO ()
main = do
  ns <- map read . lines <$> getContents
  print $ sum [ n `div` 3 - 2 | n <- ns ]
