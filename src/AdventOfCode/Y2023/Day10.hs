{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day10 where

import Data.Maybe (mapMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Monad (guard)
import AdventOfCode.Y2023.Prelude
import Data.Functor (($>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      let example1raw =(<> "\n") [text|
            -L|F7
            7S-7|
            L|7||
            -L-J|
            L|-JF
          |]

          example1parsed =
            [ [ EastWest,   NorthEast,  NorthSouth, SouthEast,  SouthWest   ]
            , [ SouthWest,  Start,      EastWest,   SouthWest,  NorthSouth  ]
            , [ NorthEast,  NorthSouth, SouthWest,  NorthSouth, NorthSouth  ]
            , [ EastWest,   NorthEast,  EastWest,   NorthWest,  NorthSouth  ]
            , [ NorthEast,  NorthSouth, EastWest,   NorthWest,  SouthEast   ]
            ]

      check parser part1 part2 Example
        { raw = example1raw
        , parsed = example1parsed
        , part1output = 4
        , part2output = 1
        }

      let example2raw = (<> "\n") [text|
            7-F7-
            .FJ|7
            SJLL7
            |F--J
            LJ.LJ
          |]

          example2parsed =
            [ [ SouthWest, EastWest, SouthEast, SouthWest, EastWest ] 
            , [ Blank, SouthEast, NorthWest, NorthSouth, SouthWest ] 
            , [ Start, NorthWest, NorthEast, NorthEast, SouthWest ] 
            , [ NorthSouth, SouthEast, EastWest, EastWest, NorthWest ] 
            , [ NorthEast, NorthWest, Blank, NorthEast, NorthWest ] 
            ]

      it "parses the second example correctly" do
        parse parser "example2" example2raw `shouldBe` Right example2parsed

      it "calculates the proper distance for the second example" do
        part1 example2parsed `shouldBe` 8

      it "calculates the proper area for the second example" do
        area example2parsed `shouldBe` Set.fromList [(2,2)]

  }

type Input :: Type
type Input = [[Tile]]

type Tile :: Type
data Tile 
  = Blank
  | NorthEast
  | SouthEast
  | SouthWest
  | NorthWest
  | NorthSouth
  | EastWest
  | Start
  deriving stock (Show, Eq)

parser :: Parser Input
parser = row `endBy` newline where
  row = some tile
  tile = asum
    [ char '.' $> Blank
    , char 'L' $> NorthEast
    , char 'F' $> SouthEast
    , char '7' $> SouthWest
    , char 'J' $> NorthWest
    , char '|' $> NorthSouth
    , char '-' $> EastWest
    , char 'S' $> Start
    ]

deltas :: Tile -> [(Int,Int)]
deltas = \case
  Blank -> []
  NorthEast -> [(-dy,1-dy) | dy <- [0,1]]
  SouthEast -> [(dy,1-dy) | dy <- [0,1]]
  SouthWest -> [(dy,dy-1) | dy <- [0,1]]
  NorthWest -> [(-dy,dy-1) | dy <- [0,1]]
  NorthSouth -> [(dy,0) | dy <- [-1,1]]
  EastWest -> [(0,dx)| dx <- [-1,1]]
  Start -> [(1,0),(0,1),(-1,0),(0,-1)]

part1 :: Input -> Int
part1 = (`div` 2) . length . snd . maxloop

maxloop :: Input -> ((Int, Int), [(Int,Int)])
maxloop input = ((maxY, maxX), maximumBy (comparing length) loops) where
  lead = Map.fromList do
    (y, row) <- zip [0..] input
    (x, tile) <- zip [0..] row
    pure 
      ( (y,x)
      , [(y + dy, x + dx) | (dy,dx) <- deltas tile]
      )

  maxY = length input
  maxX = length (head input)

  mutual = Map.mapWithKey
    do \p -> filter \q -> maybe False (elem p) $ Map.lookup q lead
    do lead
  
  start = head do
    (y, row) <- zip [0..] input
    (x, Start) <- zip [0..] row
    pure (y,x)

  loops = do
    p <- mutual Map.! start
    let path = dfs start p
    guard do
      start `elem` path
    pure do
      start : takeWhile (/= start) path

  dfs prev curr = curr : dfs curr (head (filter (/= prev) (mutual Map.! curr)))

part2 :: Input -> Int
part2 = Set.size . area

area :: Input -> Set.Set (Int,Int)
area = contract . flood . dilate . maxloop

dilate :: ((Int,Int), [(Int,Int)]) -> ((Int,Int), [(Int,Int)])
dilate ((maxY,maxX), loop) = ((2*maxY,2*maxX), expand [(2*y,2*x) | (y,x) <- loop]) where
  expand [] = error "bad loop"
  expand (a:as) = a : dilated  a a as

  dilated start prev [] = [ avg prev start | start /= prev ]
  dilated start prev (curr:next) = avg prev curr : curr : dilated start curr next

avg :: (Int,Int) -> (Int, Int) -> (Int, Int)
avg (ay,ax) (by,bx) = ((ay + by) `div` 2, (ax + bx) `div` 2)

flood :: ((Int,Int), [(Int,Int)]) -> Set.Set (Int,Int)
flood ((maxY,maxX), Set.fromList -> loop) = go
    . Set.partition (\(y,x) -> y == 0 || x == 0 || y == maxY - 1 || x == maxX - 1) 
    $ Set.fromList [(y,x) | y <- [0..maxY-1], x <- [0..maxX-1]] Set.\\ loop
  where

  go (boundary, remaining)
    | Set.null boundary = remaining
    | otherwise = do
        let boundary' = remaining `Set.intersection` adjacent boundary
            remaining' = remaining Set.\\ boundary'
        go (boundary', remaining')

  adjacent boundary = Set.unions do
    (dy,dx) <- [(-1,0),(0,1),(1,0),(0,-1)]
    pure do
      Set.map (\(y,x) -> (y + dy,x + dx)) boundary


contract :: Set.Set (Int,Int) -> Set.Set (Int,Int)
contract = Set.fromList . mapMaybe contractPoint . Set.toList where
  contractPoint (half -> (qy,0), half -> (qx,0)) = Just (qy,qx)
  contractPoint _ = Nothing

  half = (`quotRem` 2)
