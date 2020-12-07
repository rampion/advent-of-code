module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable (traverse_)
import System.Environment (getArgs)

main :: IO ()
main = (getArgs >>=) . traverse_ $ \inputFile -> do
  putStrLn inputFile

  rules <- parseRules <$> readFile inputFile

  putStrLn "\tpart 1: how many different bags could recursively contain a shiny gold bag"
  putStrLn $ "\t" ++ show (part1 rules)
  
  putStrLn "\tpart 2: how many different bags are recursively contained in a shiny gold bag"
  putStrLn $ "\t" ++ show (part2 rules)
  

data Rule = Rule {
  outer :: Bag,
  inner :: [(Bag, Count)]
}

type Bag = (String, String)
type Count = Int

part1 :: [Rule] -> Int
part1 = length . filter (maybe False (>0) . Map.lookup ("shiny", "gold")) . Map.elems . recursiveContents

part2 :: [Rule] -> Int
part2 = maybe 0 (sum . Map.elems) . Map.lookup ("shiny", "gold") . recursiveContents

recursiveContents :: [Rule] -> Map Bag (Map Bag Count)
recursiveContents rules = contents where
  contents = Map.fromList $ search <$> rules
  search (Rule outer inner) = (outer, Map.unionsWith (+) $ Map.fromList inner : [fmap (*count) (contents Map.! bag) | (bag, count) <- inner])

parseRules :: String -> [Rule]
parseRules = map (parseRule . words) . lines where
  parseRule (adverb : color : _bags : _contains : inner) = Rule (adverb, color) (parseInner inner)
  parseRule invalidRule = error $ "invalid rule " ++ show (unwords invalidRule)

  parseInner ["no","other","bags."] = []
  parseInner bagList = parseBagList bagList

  parseBagList (count : adverb : color : _bags : bagList) = ((adverb, color), read count) : parseBagList bagList
  parseBagList [] = []
  parseBagList invalidBagList = error $ "invalid bag list " ++ show (unwords invalidBagList)
