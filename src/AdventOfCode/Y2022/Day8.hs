module AdventOfCode.Y2022.Day8 where

import AdventOfCode.Y2022.Prelude
import Data.List (transpose)
import Control.Applicative (ZipList(..))
import Data.Functor.Compose (Compose (..))
import Data.Coerce (coerce)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = tellSpec do
    let raw = [text|
            30373
            25512
            65332
            33549
            35390
          |]
        parsed = toGrid
          [ [ 3, 0, 3, 7, 3 ]
          , [ 2, 5, 5, 1, 2 ]
          , [ 6, 5, 3, 3, 2 ]
          , [ 3, 3, 5, 4, 9 ]
          , [ 3, 5, 3, 9, 0 ]
          ]

    runCheck solver Example
      { raw
      , parsed
      , part1output=21
      , part2output=8
      }

    describe "visible" do
      it "reports the correct total visibilities" do
        fromGrid (visible parsed) `shouldBe`
            [ [  True,  True,  True,  True,  True ]
            , [  True,  True,  True, False,  True ]
            , [  True,  True, False,  True,  True ]
            , [  True, False,  True, False,  True ]
            , [  True,  True,  True,  True,  True ]
            ]

    describe "visibilities" do
      it "reports the correct partial visibilities" do
        let t = True
            f = False
        fromGrid (visibilities parsed) `shouldBe`
            [ [ [t,f,t,f], [f,f,t,f], [f,f,t,f], [t,t,t,f], [f,t,t,f] ]
            , [ [t,f,f,f], [t,f,t,f], [f,t,t,f], [f,f,f,f], [f,t,f,f] ]
            , [ [t,t,t,t], [f,t,f,f], [f,f,f,f], [f,t,f,f], [f,t,f,f] ]
            , [ [t,f,f,f], [f,f,f,f], [t,f,f,t], [f,f,f,f], [t,t,t,t] ]
            , [ [t,f,f,t], [t,f,f,t], [f,f,f,t], [t,t,t,t], [f,t,f,t] ]
            ]

    describe "leftToRight" do
      it "reports the correct left-to-right visibilities" do
        let t = True
            f = False
        fromGrid (leftToRight parsed) `shouldBe`
            [ [ t, f, f, t, f ]
            , [ t, t, f, f, f ]
            , [ t, f, f, f, f ]
            , [ t, f, t, f, t ]
            , [ t, t, f, t, f ]
            ]

    describe "isLeftVisible" do
      it "reports the correct left-to-right visibilities" do
        isLeftVisible [ 3, 0, 3, 7, 3] `shouldBe` [True, False, False, True, False]

      it "uses the correct scan" do
        scanl max (-1) [3, 0, 3, 7, 3] `shouldBe` [-1,3,3,3,7,7 :: Int]

    describe "scenicScores" do
      it "assigns the correct scores to each location" do
        fromGrid (scenicScores parsed) `shouldBe`
            [ [ 0, 0, 0, 0, 0 ]
            , [ 0, 1, 4, 1, 0 ]
            , [ 0, 6, 1, 2, 0 ]
            , [ 0, 1, 8, 3, 0 ]
            , [ 0, 0, 0, 0, 0 ]
            ]

    describe "leftScores" do
      it "assigns the correct scores to each location" do
        leftScore [3,0,3,7,3] `shouldBe` [0,1,2,3,1]

parser :: Parser (Grid Int)
parser = toGrid <$> rows where
  rows = row `endBy` newline
  row = many height
  height = read . pure <$> digit

type Grid = Compose ZipList ZipList

toGrid :: [[a]] -> Grid a
toGrid = coerce

fromGrid :: Grid a -> [[a]]
fromGrid = coerce

part1 :: Grid Int -> Int
part1 = sum . map (length . filter id) . fromGrid . visible

visible :: Grid Int -> Grid Bool
visible = fmap (any id) . visibilities

visibilities :: Grid Int -> Grid [Bool]
visibilities trees = traverse ($ trees) [leftToRight, rightToLeft, topToBottom, bottomToTop] 

leftToRight :: Grid Int -> Grid Bool
leftToRight = mapRows isLeftVisible

rightToLeft :: Grid Int -> Grid Bool
rightToLeft = mapRows isRightVisible

topToBottom :: Grid Int -> Grid Bool
topToBottom = mapCols isLeftVisible

bottomToTop :: Grid Int -> Grid Bool
bottomToTop = mapCols isRightVisible

isRightVisible :: [Int] -> [Bool]
isRightVisible = reverse . isLeftVisible . reverse

isLeftVisible :: [Int] -> [Bool]
isLeftVisible row = zipWith (>) row do scanl max (-1) row

mapRows :: ([a] -> [b]) -> Grid a -> Grid b
mapRows f = coerce (map f)

mapCols :: ([a] -> [b]) -> Grid a -> Grid b
mapCols f = coerce (transpose . map f . transpose)

part2 :: Grid Int -> Int
part2 = maximum . scenicScores

scenicScores :: Grid Int -> Grid Int
scenicScores trees = product <$> traverse ($trees)
  [ mapRows leftScore
  , mapRows rightScore
  , mapCols leftScore
  , mapCols rightScore
  ]

leftScore, rightScore :: [Int] -> [Int]
leftScore = fmap (snd . head) . tail . scanl step [] where
  step ps house = 
    let (inc,exc) = break (\(h,_) -> h >= house) ps
        score = sum (map snd inc) + if null ps then 0 else 1
    in (house, score) : exc
    
rightScore = reverse . leftScore . reverse
