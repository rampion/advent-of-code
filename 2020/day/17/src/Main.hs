{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main (main) where

import Data.Foldable (traverse_)
import Data.List (intercalate, transpose)
-- import Debug.Trace
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    initial <- parsePlane <$> readFile filename

    {-
    putStrLn "\tpart 0: active cubes after six cycles using 2 dimensions"
    putStrLn $ "\t" ++ show (part0 initial)
    -}

    putStrLn "\tpart 1: active cubes after six cycles"
    putStrLn $ "\t" ++ show (part1 initial)

    putStrLn "\tpart 2: active cubes after six cycles using 4 dimensions"
    putStrLn $ "\t" ++ show (part2 initial)

--------------------------------------------------------------------------------

part2 :: Grid n Cell -> Int
part2 = part1 . embed

part1 :: Grid n Cell -> Int
part1 = part0 . embed

part0 :: Grid n Cell -> Int
part0 = popCount . flatten . (!! 6) . iterate step

embed :: Grid n a -> Grid ('Succ n) a
embed = Dimension . (: [])

popCount :: [Cell] -> Int
popCount = length . filter id

flatten :: Grid n a -> [a]
flatten (Point a) = [a]
flatten (Dimension as) = concatMap flatten as

step :: Grid n Cell -> Grid n Cell
step = fmap rule . neighborhoods

rule :: Neighborhood -> Cell
rule Neighborhood {focus, livingNeighbors}
  | focus = 2 <= livingNeighbors && livingNeighbors <= 3
  | otherwise = livingNeighbors == 3

neighborhoods :: Grid n Cell -> Grid n Neighborhood
neighborhoods = \case
  Point a -> Point $ Neighborhood {focus = a, livingNeighbors = 0}
  Dimension as -> Dimension case fmap neighborhoods as of
    [] -> []
    [b] -> let a = boundary b in [a, b, a]
    (b : c : ds) -> boundary b : (b `addNeighbors` c) : loop b c ds
  where
    loop a b [] = [b `addNeighbors` a, boundary b]
    loop a b (c : ds) = (b `addNeighbors` a `addNeighbors` c) : loop b c ds

boundary :: Grid n Neighborhood -> Grid n Neighborhood
boundary = fmap \Neighborhood {focus, livingNeighbors} ->
  Neighborhood False (popCount [focus] + livingNeighbors)

addNeighbors :: Grid n Neighborhood -> Grid n Neighborhood -> Grid n Neighborhood
Dimension xs `addNeighbors` Dimension ys = Dimension (zipWith addNeighbors xs ys)
Point x `addNeighbors` Point y =
  Point $
    Neighborhood
      { focus = focus x,
        livingNeighbors = popCount [focus y] + livingNeighbors y + livingNeighbors x
      }

--------------------------------------------------------------------------------

parsePlane :: String -> Grid Two Cell
parsePlane = Dimension . map (Dimension . map (Point . parseCell)) . lines

parseCell :: Char -> Cell
parseCell '.' = False
parseCell '#' = True
parseCell ch = error $ "illegal cell: " ++ show ch

--------------------------------------------------------------------------------

data Neighborhood = Neighborhood {focus :: Bool, livingNeighbors :: Int}

data Nat = Zero | Succ Nat

type One = 'Succ 'Zero

type Two = 'Succ One

type Three = 'Succ Two

type Four = 'Succ Three

data Grid (n :: Nat) a where
  Point :: a -> Grid 'Zero a
  Dimension :: [Grid n a] -> Grid ('Succ n) a

deriving instance Functor (Grid n)

type Cell = Bool

instance Show (Grid 'Zero Neighborhood) where
  show (Point (Neighborhood _ c)) = show c

instance Show (Grid 'Zero Cell) where
  show (Point False) = "."
  show (Point True) = "#"

instance Show (Grid 'Zero a) => Show (Grid One a) where
  show (Dimension as) = show =<< as

instance Show (Grid One a) => Show (Grid Two a) where
  show (Dimension as) = unlines (show <$> as)

instance Show (Grid Two a) => Show (Grid Three a) where
  show (Dimension as) = unlines . fmap (intercalate " ") . transpose $ fmap (lines . show) as

instance Show (Grid Three a) => Show (Grid Four a) where
  show (Dimension as) = intercalate "\n\n" $ fmap show as
