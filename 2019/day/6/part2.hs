{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Main where

import Test.DocTest (doctest)
import Text.Parsec.String (parseFromFile, Parser)
import Text.Parsec (many1, upper, digit, newline, char, endBy)
import Control.Applicative ((<|>))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Tuple (swap)

test :: IO ()
test = doctest ["2019/day/6/part2.hs"]

main :: IO ()
main = print . countTransfers =<< either (fail . show) return =<< parseFromFile adjacencies "2019/day/6/input"

type Planet = String
type Orbit = (Planet, Planet)
type Orbits = Map Planet Planet
type OrbitalAdjacencies = Map Planet [Planet]

-- $setup
-- >>> import Text.Parsec (parse)
-- >>> import Data.List (sort)

-- |
-- >>> fmap (fmap sort) $ parse adjacencies "example" "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN\n"
-- Right (fromList [("B",["C","COM","G"]),("C",["B","D"]),("COM",["B"]),("D",["C","E","I"]),("E",["D","F","J"]),("F",["E"]),("G",["B","H"]),("H",["G"]),("I",["D","SAN"]),("J",["E","K"]),("K",["J","L","YOU"]),("L",["K"]),("SAN",["I"]),("YOU",["K"])])
adjacencies :: Parser OrbitalAdjacencies
adjacencies = toAdjacencies <$> (orbit `endBy` newline)

-- |
-- >>> parse orbit "example" "COM)B"
-- Right ("B","COM")
-- >>> parse orbit "example" "B)C"
-- Right ("C","B")
orbit :: Parser Orbit
orbit = flip (,) <$> planet <* char ')' <*> planet

planet :: Parser Planet
planet = many1 (upper <|> digit)

-- |
-- >>> countTransfers $ Map.fromList [("B",["C","COM","G"]),("C",["B","D"]),("COM",["B"]),("D",["C","E","I"]),("E",["D","F","J"]),("F",["E"]),("G",["B","H"]),("H",["G"]),("I",["D","SAN"]),("J",["E","K"]),("K",["J","L","YOU"]),("L",["K"]),("SAN",["I"]),("YOU",["K"])]
-- 4
countTransfers :: OrbitalAdjacencies -> Int
countTransfers m = (bfs m "SAN" ! "YOU") - 2


-- |
-- >>> flip bfs "SAN" $ Map.fromList [("B",["C","COM","G"]),("C",["B","D"]),("COM",["B"]),("D",["C","E","I"]),("E",["D","F","J"]),("F",["E"]),("G",["B","H"]),("H",["G"]),("I",["D","SAN"]),("J",["E","K"]),("K",["J","L","YOU"]),("L",["K"]),("SAN",["I"]),("YOU",["K"])]
-- fromList [("B",4),("C",3),("COM",5),("D",2),("E",3),("F",4),("G",5),("H",6),("I",1),("J",4),("K",5),("L",6),("SAN",0),("YOU",6)]
bfs :: forall k. Ord k => Map k [k] -> k -> Map k Int
bfs m = \seed -> let (d,q) = search 1 Map.empty ((seed,0) : q) in d where
  search :: Ord k => Int -> Map k Int -> [(k,Int)] -> (Map k Int, [(k,Int)])
  search 0 d ~[] = (d, [])
  search (subtract 1 -> n) d ~((k,s):q) 
    | Map.member k d  = search n d q
    | otherwise       = 
        let ps = m ! k 
            ~(d',q') = search (n + length ps) (Map.insert k s d) q
        in (d', [(p,s + 1) | p <- ps] ++ q')

-- |
-- >>> fmap sort $ toAdjacencies [("B","COM"),("C","B"),("D","C"),("E","D"),("F","E"),("G","B"),("H","G"),("I","D"),("J","E"),("K","J"),("L","K")]
-- fromList [("B",["C","COM","G"]),("C",["B","D"]),("COM",["B"]),("D",["C","E","I"]),("E",["D","F","J"]),("F",["E"]),("G",["B","H"]),("H",["G"]),("I",["D"]),("J",["E","K"]),("K",["J","L"]),("L",["K"])]
toAdjacencies :: [Orbit] -> OrbitalAdjacencies
toAdjacencies = Map.fromListWith (++) . concatMap (\p -> [return <$> p, return <$> swap p])
