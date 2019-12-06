module Main where

import Test.DocTest (doctest)
import Text.Parsec.String (parseFromFile, Parser)
import Text.Parsec (many1, upper, digit, newline, char, endBy)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map

test :: IO ()
test = doctest ["2019/day/6/part1.hs"]

main :: IO ()
main = print . countOrbits =<< either (fail . show) return =<< parseFromFile orbits "2019/day/6/input"

type Planet = String
type Orbit = (Planet, Planet)
type Orbits = Map Planet Planet

-- $setup
-- >>> import Text.Parsec (parse)

-- |
-- >>> parse orbits "example" "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"
-- Right (fromList [("B","COM"),("C","B"),("D","C"),("E","D"),("F","E"),("G","B"),("H","G"),("I","D"),("J","E"),("K","J"),("L","K")])
orbits :: Parser Orbits
orbits = Map.fromList <$> (orbit `endBy` newline)

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
-- >>> countOrbits $ Map.fromList [("B","COM"),("C","B"),("D","C"),("E","D"),("F","E"),("G","B"),("H","G"),("I","D"),("J","E"),("K","J"),("L","K")]
-- 42
countOrbits :: Orbits -> Int
countOrbits = sum . loeb . fmap fromOrbit where
  fromOrbit :: Planet -> Map Planet Int -> Int
  fromOrbit p m = 1 + Map.findWithDefault 0 p m

-- |
-- >>> loeb [ (+) <$> (!!1) <*> (!!2), const 3, const 7 ]
-- [10,3,7]
loeb :: Functor f => f (f a -> a) -> f a
loeb ffa = fa where fa = fmap ($fa) ffa
