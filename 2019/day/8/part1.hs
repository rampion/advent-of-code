{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Test.DocTest (doctest)

import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (chunksOf)

main :: IO ()
main =
  print .
  ((*) <$> countPixel P1 <*> countPixel P2) .
  minimumBy (compare `on` countPixel P0) .
  layers . 
  readImage (Width 25) (Height 6) .
  init =<<  -- drop trailing '\n'
  readFile "2019/day/8/input"

test :: IO ()
test = doctest ["2019/day/8/part1.hs"]

data Pixel = P0 | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9
  deriving (Eq, Show)

newtype Layer = Layer { getRows :: [[Pixel]] }
  deriving newtype Show

data Image = Image { width :: Width, height :: Height, layers :: [Layer] }
  deriving Show

newtype Width = Width { getWidth :: Int }
  deriving newtype Show

newtype Height = Height { getHeight :: Int }
  deriving newtype Show

-- |
-- >>> countPixel P0 (Layer [[P0,P0,P1],[P2,P1,P0]])
-- 3
-- >>> countPixel P1 (Layer [[P0,P0,P1],[P2,P1,P0]])
-- 2
-- >>> countPixel P2 (Layer [[P0,P0,P1],[P2,P1,P0]])
-- 1
countPixel :: Pixel -> Layer -> Int
countPixel p = length . concatMap (filter (==p)) . getRows

-- |
-- >>> readImage (Width 3) (Height 2) "123456789012"
-- Image {width = 3, height = 2, layers = [[[P1,P2,P3],[P4,P5,P6]],[[P7,P8,P9],[P0,P1,P2]]]}
readImage :: Width -> Height -> String -> Image
readImage w h = Image w h . map Layer . chunksOf (getHeight h) . chunksOf (getWidth w) . map readPixel

readPixel :: Char -> Pixel
readPixel '0' = P0
readPixel '1' = P1
readPixel '2' = P2
readPixel '3' = P3
readPixel '4' = P4
readPixel '5' = P5
readPixel '6' = P6
readPixel '7' = P7
readPixel '8' = P8
readPixel '9' = P9
readPixel p = error $ "bad pixel value: " ++ show p
