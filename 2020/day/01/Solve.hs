module Main where

import Data.List (tails)
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- map (read :: String -> Int). lines <$> (readFile . head =<< getArgs)
  let inputSet = Set.fromList input
  let paired = Set.intersection inputSet (Set.map (2020 -) inputSet)
  print paired
  if Set.size paired == 2
    then print (product paired)
    else putStrLn "no pair that adds to 2020 found"
  let triples = Set.fromList [[a,b,c] | (a:as) <- tails input, (b:bs) <- tails as, b <= a, c <- bs, a + b + c == 2020]
  print triples
  if Set.size triples > 0
    then print (Set.map product triples)
    else putStrLn "no triple that adds to 2020 found"

