{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Data.Foldable (traverse_)
import Data.List (intercalate, transpose)
-- import Debug.Trace
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    initial <- parsePlane <$> readFile filename

    putStrLn "\tpart 1: active cubes after six cycles"
    putStrLn $ "\t" ++ show (part1 initial)

    putStrLn "\tpart 2: active cubes after six cycles using 4 dimensions"
    putStrLn $ "\t" ++ show (part2 initial)

--------------------------------------------------------------------------------

part2 :: Grid n Cell -> Int
part2 = part1 . Dimension . return

part1 :: Grid n Cell -> Int
part1 = popCount . flatten . (!! 6) . iterate step . Dimension . return

popCount :: [Cell] -> Int
popCount = length . filter id

step :: Grid n Cell -> Grid n Cell
step = fmap rule . neighborhoods . expand

rule :: Neighborhood Cell -> Cell
rule Neighborhood {focus, neighbors}
  | focus = popCount neighbors `elem` [2, 3]
  | otherwise = popCount neighbors == 3

consWith3 :: (a -> a -> a -> b) -> [a] -> [b]
consWith3 f = zipWith3 f <*> drop 1 <*> drop 2

flatten :: Grid n a -> [a]
flatten (Point a) = [a]
flatten (Dimension as) = concatMap flatten as

neighborhoods :: Grid n a -> Grid n (Neighborhood a)
neighborhoods (Point a) = Point $ Neighborhood {focus = a, neighbors = []}
neighborhoods (Dimension as) = Dimension $ consWith3 combine (neighborhoods <$> as)
  where
    combine :: Grid n (Neighborhood a) -> Grid n (Neighborhood a) -> Grid n (Neighborhood a) -> Grid n (Neighborhood a)
    combine (Point a) (Point b) (Point c) =
      Point $
        Neighborhood
          { focus = focus b,
            neighbors = [focus a, focus c] ++ neighbors a ++ neighbors b ++ neighbors c
          }
    combine (Dimension as) (Dimension bs) (Dimension cs) = Dimension (zipWith3 combine as bs cs)

expand :: Grid n Cell -> Grid n Cell
expand (Dimension (map expand -> as@(a : _))) = Dimension (z : z : as ++ [z, z])
  where
    z = False <$ a
expand grid = grid

--------------------------------------------------------------------------------

parsePlane :: String -> Grid Two Cell
parsePlane = Dimension . map (Dimension . map (Point . parseCell)) . lines

parseCell :: Char -> Cell
parseCell '.' = False
parseCell '#' = True
parseCell ch = error $ "illegal cell: " ++ show ch

--------------------------------------------------------------------------------

data Neighborhood a = Neighborhood {focus :: a, neighbors :: [a]}

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

instance Show (Grid 'Zero Cell) where
  show (Point False) = "."
  show (Point True) = "#"

instance Show (Grid One Cell) where
  show (Dimension as) = show =<< as

instance Show (Grid Two Cell) where
  show (Dimension as) = unlines (show <$> as)

instance Show (Grid Three Cell) where
  show (Dimension as) = unlines . fmap (intercalate " ") . transpose $ fmap (lines . show) as

instance Show (Grid Four Cell) where
  show (Dimension as) = intercalate "\n\n" $ fmap show as
