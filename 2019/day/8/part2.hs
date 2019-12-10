{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Test.DocTest (doctest)

import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (chunksOf)

main :: IO ()
main =
  putStr .
  renderLayer .
  flatten .
  readImage (Width 25) (Height 6) .
  init =<<  -- drop trailing '\n'
  readFile "2019/day/8/input"

test :: IO ()
test = doctest ["2019/day/8/part2.hs"]

data Pixel = Black | White | Transparent
  deriving (Eq, Show)

instance Semigroup Pixel where
  (<>) Transparent p = p
  (<>) p _           = p
instance Monoid Pixel where
  mempty = Transparent

newtype Layer = Layer { getRows :: [[Pixel]] }
  deriving newtype Show

data Image = Image { width :: Width, height :: Height, layers :: [Layer] }
  deriving Show

newtype Width = Width { getWidth :: Int }
  deriving newtype Show

newtype Height = Height { getHeight :: Int }
  deriving newtype Show

-- |
-- >>> flatten $ readImage (Width 2) (Height 2) "0222112222120000"
-- [[Black,White],[White,Black]]
flatten :: Image -> Layer
flatten = \im -> foldr collapse (background im) (layers im) where
  background :: Image -> Layer
  background (Image (Width w) (Height h) _) = Layer . replicate h $ replicate w Transparent

  collapse :: Layer -> Layer -> Layer
  collapse (Layer as) (Layer bs) = Layer $ zipWith (zipWith mappend) as bs

-- |
-- >>> putStr . renderLayer $ Layer [[Black,White,Transparent,Black],[Transparent,White,White,Black]]
-- █░ █
--  ░░█
renderLayer :: Layer -> String
renderLayer = unlines . map (map renderPixel) . getRows

renderPixel :: Pixel -> Char
renderPixel Black       = '█'
renderPixel White       = '░'
renderPixel Transparent = ' '

-- |
-- >>> readImage (Width 3) (Height 2) "000111222012"
-- Image {width = 3, height = 2, layers = [[[Black,Black,Black],[White,White,White]],[[Transparent,Transparent,Transparent],[Black,White,Transparent]]]}
readImage :: Width -> Height -> String -> Image
readImage w h = Image w h . map Layer . chunksOf (getHeight h) . chunksOf (getWidth w) . map readPixel

readPixel :: Char -> Pixel
readPixel '0' = Black
readPixel '1' = White
readPixel '2' = Transparent
readPixel p = error $ "bad pixel value: " ++ show p
