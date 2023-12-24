{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day14 where

import AdventOfCode.Y2023.Prelude hiding (Empty)
import Data.List (transpose)
import Data.Map.Strict qualified as Map

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      let example1raw = (<> "\n") [text|
            O....#....
            O.OO#....#
            .....##...
            OO.#O....O
            .O.....O#.
            O.#..O.#.#
            ..O..#O..O
            .......O..
            #....###..
            #OO..#....
          |]
      
      check parser part1 part2 Example
        { raw = example1raw
        , parsed =
            [ [ Rounded, Empty, Empty, Empty, Empty, Cube, Empty, Empty, Empty, Empty ]
            , [ Rounded, Empty, Rounded, Rounded, Cube, Empty, Empty, Empty, Empty, Cube ]
            , [ Empty, Empty, Empty, Empty, Empty, Cube, Cube, Empty, Empty, Empty ]
            , [ Rounded, Rounded, Empty, Cube, Rounded, Empty, Empty, Empty, Empty, Rounded ]
            , [ Empty, Rounded, Empty, Empty, Empty, Empty, Empty, Rounded, Cube, Empty ]
            , [ Rounded, Empty, Cube, Empty, Empty, Rounded, Empty, Cube, Empty, Cube ]
            , [ Empty, Empty, Rounded, Empty, Empty, Cube, Rounded, Empty, Empty, Rounded ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Rounded, Empty, Empty ]
            , [ Cube, Empty, Empty, Empty, Empty, Cube, Cube, Cube, Empty, Empty ]
            , [ Cube, Rounded, Rounded, Empty, Empty, Cube, Empty, Empty, Empty, Empty ]
            ]
        , part1output = 136
        , part2output = 64
        }

      let after1cycle = (<> "\n") [text|
            .....#....
            ....#...O#
            ...OO##...
            .OO#......
            .....OOO#.
            .O#...O#.#
            ....O#....
            ......OOOO
            #...O###..
            #..OO#....
          |]

      it "should produce the expected output after one cycle" do
        fmap oneCycle (parse parser "before" example1raw)
          `shouldBe` parse parser "after" after1cycle
        
      let after2cycles = (<> "\n") [text|
            .....#....
            ....#...O#
            .....##...
            ..O#......
            .....OOO#.
            .O#...O#.#
            ....O#...O
            .......OOO
            #..OO###..
            #.OOO#...O
          |] 

      it "should produce the expected output after two cycles" do
        fmap (oneCycle . oneCycle) (parse parser "before" example1raw)
          `shouldBe` parse parser "after" after2cycles
        
      let after3cycles = (<> "\n") [text|
            .....#....
            ....#...O#
            .....##...
            ..O#......
            .....OOO#.
            .O#...O#.#
            ....O#...O
            .......OOO
            #...O###.O
            #.OOO#...O
          |]

      it "should produce the expected output after three cycles" do
        fmap (oneCycle . oneCycle . oneCycle) (parse parser "before" example1raw)
          `shouldBe` parse parser "after" after3cycles
  }

type Input :: Type
type Input = [[Tile]]

type Tile :: Type
data Tile
  = Rounded
  | Cube
  | Empty
  deriving stock (Show, Eq, Ord)

parser :: Parser Input
parser = line `endBy` newline where
  line = some tile
  tile = asum
    [ char '.' $> Empty
    , char '#' $> Cube
    , char 'O' $> Rounded
    ]

part1 :: Input -> Int
part1 = score . tiltNorth
{-
part1 rows = sum scores where
  numRows = length rows
  columns = transpose rows
  scores = map score columns

  score = loop 0 0 . zip [0..]

  loop !total free = \case
    [] -> total
    (row, tile) : ps -> case tile of
      Rounded -> loop (total + numRows - row + free) free ps
      Cube -> loop total 0 ps
      Empty -> loop total (free + 1) ps
-}

numCycles :: Int
numCycles = 1_000_000_000

sift :: Tile -> [Tile] -> [Tile]
sift g = loop [] where
  loop sifted = \case
    [] -> sifted
    Cube : ts -> sifted <> (Cube : loop [] ts)
    t : ts 
      | t == g    -> loop (g : sifted) ts
      | otherwise -> t : loop sifted ts

tiltWest, tiltNorth, tiltEast, tiltSouth :: Input -> Input
tiltWest = map (sift Empty)
tiltNorth = transpose . tiltWest . transpose
tiltEast = map (sift Rounded)
tiltSouth = transpose . tiltEast . transpose

oneCycle :: Input -> Input
oneCycle = tiltEast . tiltSouth . tiltWest . tiltNorth 

score :: Input -> Int
score = sum . scanl (+) 0 . map numRounded

numRounded :: [Tile] -> Int
numRounded = length . filter (== Rounded)
  
part2 :: Input -> Int
part2 = loop Map.empty 0 where
  loop !m !i grid = case Map.lookup grid m of
    Nothing -> loop (Map.insert grid i m) (i + 1) (oneCycle grid)
    Just offset-> 
      let period = i - offset
          n = (numCycles - offset) `rem` period
      in
      score (iterate oneCycle grid !! n)
