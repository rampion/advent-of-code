{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2022.Day10 where

import AdventOfCode.Y2022.Prelude hiding (cycle)
import Data.List (scanl', unfoldr)
import Data.Maybe (catMaybes)

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = tellSpec do
        let examplePairs =
              [ (Nothing, RegisterState {cycle = 1, value = 1})
              , (Nothing, RegisterState {cycle = 2, value = 1})
              , (Just (Addx 15), RegisterState {cycle = 3, value = 16})
              , (Nothing, RegisterState {cycle = 4, value = 16})
              , (Just (Addx (-11)), RegisterState {cycle = 5, value = 5})
              , (Nothing, RegisterState {cycle = 6, value = 5})
              , (Just (Addx 6), RegisterState {cycle = 7, value = 11})
              , (Nothing, RegisterState {cycle = 8, value = 11})
              , (Just (Addx (-3)), RegisterState {cycle = 9, value = 8})
              , (Nothing, RegisterState {cycle = 10, value = 8})
              , (Just (Addx 5), RegisterState {cycle = 11, value = 13})
              , (Nothing, RegisterState {cycle = 12, value = 13})
              , (Just (Addx (-1)), RegisterState {cycle = 13, value = 12})
              , (Nothing, RegisterState {cycle = 14, value = 12})
              , (Just (Addx (-8)), RegisterState {cycle = 15, value = 4})
              , (Nothing, RegisterState {cycle = 16, value = 4})
              , (Just (Addx 13), RegisterState {cycle = 17, value = 17})
              , (Nothing, RegisterState {cycle = 18, value = 17})
              , (Just (Addx 4), RegisterState {cycle = 19, value = 21})
              , (Just Noop, RegisterState {cycle = 20, value = 21})
              , (Nothing, RegisterState {cycle = 21, value = 21})
              , (Just (Addx (-1)), RegisterState {cycle = 22, value = 20})
              , (Nothing, RegisterState {cycle = 23, value = 20})
              , (Just (Addx 5), RegisterState {cycle = 24, value = 25})
              , (Nothing, RegisterState {cycle = 25, value = 25})
              , (Just (Addx (-1)), RegisterState {cycle = 26, value = 24})
              , (Nothing, RegisterState {cycle = 27, value = 24})
              , (Just (Addx 5), RegisterState {cycle = 28, value = 29})
              , (Nothing, RegisterState {cycle = 29, value = 29})
              , (Just (Addx (-1)), RegisterState {cycle = 30, value = 28})
              , (Nothing, RegisterState {cycle = 31, value = 28})
              , (Just (Addx 5), RegisterState {cycle = 32, value = 33})
              , (Nothing, RegisterState {cycle = 33, value = 33})
              , (Just (Addx (-1)), RegisterState {cycle = 34, value = 32})
              , (Nothing, RegisterState {cycle = 35, value = 32})
              , (Just (Addx 5), RegisterState {cycle = 36, value = 37})
              , (Nothing, RegisterState {cycle = 37, value = 37})
              , (Just (Addx (-1)), RegisterState {cycle = 38, value = 36})
              , (Nothing, RegisterState {cycle = 39, value = 36})
              , (Just (Addx (-35)), RegisterState {cycle = 40, value = 1})
              , (Nothing, RegisterState {cycle = 41, value = 1})
              , (Just (Addx 1), RegisterState {cycle = 42, value = 2})
              , (Nothing, RegisterState {cycle = 43, value = 2})
              , (Just (Addx 24), RegisterState {cycle = 44, value = 26})
              , (Nothing, RegisterState {cycle = 45, value = 26})
              , (Just (Addx (-19)), RegisterState {cycle = 46, value = 7})
              , (Nothing, RegisterState {cycle = 47, value = 7})
              , (Just (Addx 1), RegisterState {cycle = 48, value = 8})
              , (Nothing, RegisterState {cycle = 49, value = 8})
              , (Just (Addx 16), RegisterState {cycle = 50, value = 24})
              , (Nothing, RegisterState {cycle = 51, value = 24})
              , (Just (Addx (-11)), RegisterState {cycle = 52, value = 13})
              , (Just Noop, RegisterState {cycle = 53, value = 13})
              , (Just Noop, RegisterState {cycle = 54, value = 13})
              , (Nothing, RegisterState {cycle = 55, value = 13})
              , (Just (Addx 21), RegisterState {cycle = 56, value = 34})
              , (Nothing, RegisterState {cycle = 57, value = 34})
              , (Just (Addx (-15)), RegisterState {cycle = 58, value = 19})
              , (Just Noop, RegisterState {cycle = 59, value = 19})
              , (Just Noop, RegisterState {cycle = 60, value = 19})
              , (Nothing, RegisterState {cycle = 61, value = 19})
              , (Just (Addx (-3)), RegisterState {cycle = 62, value = 16})
              , (Nothing, RegisterState {cycle = 63, value = 16})
              , (Just (Addx 9), RegisterState {cycle = 64, value = 25})
              , (Nothing, RegisterState {cycle = 65, value = 25})
              , (Just (Addx 1), RegisterState {cycle = 66, value = 26})
              , (Nothing, RegisterState {cycle = 67, value = 26})
              , (Just (Addx (-3)), RegisterState {cycle = 68, value = 23})
              , (Nothing, RegisterState {cycle = 69, value = 23})
              , (Just (Addx 8), RegisterState {cycle = 70, value = 31})
              , (Nothing, RegisterState {cycle = 71, value = 31})
              , (Just (Addx 1), RegisterState {cycle = 72, value = 32})
              , (Nothing, RegisterState {cycle = 73, value = 32})
              , (Just (Addx 5), RegisterState {cycle = 74, value = 37})
              , (Just Noop, RegisterState {cycle = 75, value = 37})
              , (Just Noop, RegisterState {cycle = 76, value = 37})
              , (Just Noop, RegisterState {cycle = 77, value = 37})
              , (Just Noop, RegisterState {cycle = 78, value = 37})
              , (Just Noop, RegisterState {cycle = 79, value = 37})
              , (Nothing, RegisterState {cycle = 80, value = 37})
              , (Just (Addx (-36)), RegisterState {cycle = 81, value = 1})
              , (Just Noop, RegisterState {cycle = 82, value = 1})
              , (Nothing, RegisterState {cycle = 83, value = 1})
              , (Just (Addx 1), RegisterState {cycle = 84, value = 2})
              , (Nothing, RegisterState {cycle = 85, value = 2})
              , (Just (Addx 7), RegisterState {cycle = 86, value = 9})
              , (Just Noop, RegisterState {cycle = 87, value = 9})
              , (Just Noop, RegisterState {cycle = 88, value = 9})
              , (Just Noop, RegisterState {cycle = 89, value = 9})
              , (Nothing, RegisterState {cycle = 90, value = 9})
              , (Just (Addx 2), RegisterState {cycle = 91, value = 11})
              , (Nothing, RegisterState {cycle = 92, value = 11})
              , (Just (Addx 6), RegisterState {cycle = 93, value = 17})
              , (Just Noop, RegisterState {cycle = 94, value = 17})
              , (Just Noop, RegisterState {cycle = 95, value = 17})
              , (Just Noop, RegisterState {cycle = 96, value = 17})
              , (Just Noop, RegisterState {cycle = 97, value = 17})
              , (Just Noop, RegisterState {cycle = 98, value = 17})
              , (Nothing, RegisterState {cycle = 99, value = 17})
              , (Just (Addx 1), RegisterState {cycle = 100, value = 18})
              , (Just Noop, RegisterState {cycle = 101, value = 18})
              , (Just Noop, RegisterState {cycle = 102, value = 18})
              , (Nothing, RegisterState {cycle = 103, value = 18})
              , (Just (Addx 7), RegisterState {cycle = 104, value = 25})
              , (Nothing, RegisterState {cycle = 105, value = 25})
              , (Just (Addx 1), RegisterState {cycle = 106, value = 26})
              , (Just Noop, RegisterState {cycle = 107, value = 26})
              , (Nothing, RegisterState {cycle = 108, value = 26})
              , (Just (Addx (-13)), RegisterState {cycle = 109, value = 13})
              , (Nothing, RegisterState {cycle = 110, value = 13})
              , (Just (Addx 13), RegisterState {cycle = 111, value = 26})
              , (Nothing, RegisterState {cycle = 112, value = 26})
              , (Just (Addx 7), RegisterState {cycle = 113, value = 33})
              , (Just Noop, RegisterState {cycle = 114, value = 33})
              , (Nothing, RegisterState {cycle = 115, value = 33})
              , (Just (Addx 1), RegisterState {cycle = 116, value = 34})
              , (Nothing, RegisterState {cycle = 117, value = 34})
              , (Just (Addx (-33)), RegisterState {cycle = 118, value = 1})
              , (Just Noop, RegisterState {cycle = 119, value = 1})
              , (Just Noop, RegisterState {cycle = 120, value = 1})
              , (Just Noop, RegisterState {cycle = 121, value = 1})
              , (Nothing, RegisterState {cycle = 122, value = 1})
              , (Just (Addx 2), RegisterState {cycle = 123, value = 3})
              , (Just Noop, RegisterState {cycle = 124, value = 3})
              , (Just Noop, RegisterState {cycle = 125, value = 3})
              , (Just Noop, RegisterState {cycle = 126, value = 3})
              , (Nothing, RegisterState {cycle = 127, value = 3})
              , (Just (Addx 8), RegisterState {cycle = 128, value = 11})
              , (Just Noop, RegisterState {cycle = 129, value = 11})
              , (Nothing, RegisterState {cycle = 130, value = 11})
              , (Just (Addx (-1)), RegisterState {cycle = 131, value = 10})
              , (Nothing, RegisterState {cycle = 132, value = 10})
              , (Just (Addx 2), RegisterState {cycle = 133, value = 12})
              , (Nothing, RegisterState {cycle = 134, value = 12})
              , (Just (Addx 1), RegisterState {cycle = 135, value = 13})
              , (Just Noop, RegisterState {cycle = 136, value = 13})
              , (Nothing, RegisterState {cycle = 137, value = 13})
              , (Just (Addx 17), RegisterState {cycle = 138, value = 30})
              , (Nothing, RegisterState {cycle = 139, value = 30})
              , (Just (Addx (-9)), RegisterState {cycle = 140, value = 21})
              , (Nothing, RegisterState {cycle = 141, value = 21})
              , (Just (Addx 1), RegisterState {cycle = 142, value = 22})
              , (Nothing, RegisterState {cycle = 143, value = 22})
              , (Just (Addx 1), RegisterState {cycle = 144, value = 23})
              , (Nothing, RegisterState {cycle = 145, value = 23})
              , (Just (Addx (-3)), RegisterState {cycle = 146, value = 20})
              , (Nothing, RegisterState {cycle = 147, value = 20})
              , (Just (Addx 11), RegisterState {cycle = 148, value = 31})
              , (Just Noop, RegisterState {cycle = 149, value = 31})
              , (Just Noop, RegisterState {cycle = 150, value = 31})
              , (Nothing, RegisterState {cycle = 151, value = 31})
              , (Just (Addx 1), RegisterState {cycle = 152, value = 32})
              , (Just Noop, RegisterState {cycle = 153, value = 32})
              , (Nothing, RegisterState {cycle = 154, value = 32})
              , (Just (Addx 1), RegisterState {cycle = 155, value = 33})
              , (Just Noop, RegisterState {cycle = 156, value = 33})
              , (Just Noop, RegisterState {cycle = 157, value = 33})
              , (Nothing, RegisterState {cycle = 158, value = 33})
              , (Just (Addx (-13)), RegisterState {cycle = 159, value = 20})
              , (Nothing, RegisterState {cycle = 160, value = 20})
              , (Just (Addx (-19)), RegisterState {cycle = 161, value = 1})
              , (Nothing, RegisterState {cycle = 162, value = 1})
              , (Just (Addx 1), RegisterState {cycle = 163, value = 2})
              , (Nothing, RegisterState {cycle = 164, value = 2})
              , (Just (Addx 3), RegisterState {cycle = 165, value = 5})
              , (Nothing, RegisterState {cycle = 166, value = 5})
              , (Just (Addx 26), RegisterState {cycle = 167, value = 31})
              , (Nothing, RegisterState {cycle = 168, value = 31})
              , (Just (Addx (-30)), RegisterState {cycle = 169, value = 1})
              , (Nothing, RegisterState {cycle = 170, value = 1})
              , (Just (Addx 12), RegisterState {cycle = 171, value = 13})
              , (Nothing, RegisterState {cycle = 172, value = 13})
              , (Just (Addx (-1)), RegisterState {cycle = 173, value = 12})
              , (Nothing, RegisterState {cycle = 174, value = 12})
              , (Just (Addx 3), RegisterState {cycle = 175, value = 15})
              , (Nothing, RegisterState {cycle = 176, value = 15})
              , (Just (Addx 1), RegisterState {cycle = 177, value = 16})
              , (Just Noop, RegisterState {cycle = 178, value = 16})
              , (Just Noop, RegisterState {cycle = 179, value = 16})
              , (Just Noop, RegisterState {cycle = 180, value = 16})
              , (Nothing, RegisterState {cycle = 181, value = 16})
              , (Just (Addx (-9)), RegisterState {cycle = 182, value = 7})
              , (Nothing, RegisterState {cycle = 183, value = 7})
              , (Just (Addx 18), RegisterState {cycle = 184, value = 25})
              , (Nothing, RegisterState {cycle = 185, value = 25})
              , (Just (Addx 1), RegisterState {cycle = 186, value = 26})
              , (Nothing, RegisterState {cycle = 187, value = 26})
              , (Just (Addx 2), RegisterState {cycle = 188, value = 28})
              , (Just Noop, RegisterState {cycle = 189, value = 28})
              , (Just Noop, RegisterState {cycle = 190, value = 28})
              , (Nothing, RegisterState {cycle = 191, value = 28})
              , (Just (Addx 9), RegisterState {cycle = 192, value = 37})
              , (Just Noop, RegisterState {cycle = 193, value = 37})
              , (Just Noop, RegisterState {cycle = 194, value = 37})
              , (Just Noop, RegisterState {cycle = 195, value = 37})
              , (Nothing, RegisterState {cycle = 196, value = 37})
              , (Just (Addx (-1)), RegisterState {cycle = 197, value = 36})
              , (Nothing, RegisterState {cycle = 198, value = 36})
              , (Just (Addx 2), RegisterState {cycle = 199, value = 38})
              , (Nothing, RegisterState {cycle = 200, value = 38})
              , (Just (Addx (-37)), RegisterState {cycle = 201, value = 1})
              , (Nothing, RegisterState {cycle = 202, value = 1})
              , (Just (Addx 1), RegisterState {cycle = 203, value = 2})
              , (Nothing, RegisterState {cycle = 204, value = 2})
              , (Just (Addx 3), RegisterState {cycle = 205, value = 5})
              , (Just Noop, RegisterState {cycle = 206, value = 5})
              , (Nothing, RegisterState {cycle = 207, value = 5})
              , (Just (Addx 15), RegisterState {cycle = 208, value = 20})
              , (Nothing, RegisterState {cycle = 209, value = 20})
              , (Just (Addx (-21)), RegisterState {cycle = 210, value = -1})
              , (Nothing, RegisterState {cycle = 211, value = -1})
              , (Just (Addx 22), RegisterState {cycle = 212, value = 21})
              , (Nothing, RegisterState {cycle = 213, value = 21})
              , (Just (Addx (-6)), RegisterState {cycle = 214, value = 15})
              , (Nothing, RegisterState {cycle = 215, value = 15})
              , (Just (Addx 1), RegisterState {cycle = 216, value = 16})
              , (Just Noop, RegisterState {cycle = 217, value = 16})
              , (Nothing, RegisterState {cycle = 218, value = 16})
              , (Just (Addx 2), RegisterState {cycle = 219, value = 18})
              , (Nothing, RegisterState {cycle = 220, value = 18})
              , (Just (Addx 1), RegisterState {cycle = 221, value = 19})
              , (Just Noop, RegisterState {cycle = 222, value = 19})
              , (Nothing, RegisterState {cycle = 223, value = 19})
              , (Just (Addx (-10)), RegisterState {cycle = 224, value = 9})
              , (Just Noop, RegisterState {cycle = 225, value = 9})
              , (Just Noop, RegisterState {cycle = 226, value = 9})
              , (Nothing, RegisterState {cycle = 227, value = 9})
              , (Just (Addx 20), RegisterState {cycle = 228, value = 29})
              , (Nothing, RegisterState {cycle = 229, value = 29})
              , (Just (Addx 1), RegisterState {cycle = 230, value = 30})
              , (Nothing, RegisterState {cycle = 231, value = 30})
              , (Just (Addx 2), RegisterState {cycle = 232, value = 32})
              , (Nothing, RegisterState {cycle = 233, value = 32})
              , (Just (Addx 2), RegisterState {cycle = 234, value = 34})
              , (Nothing, RegisterState {cycle = 235, value = 34})
              , (Just (Addx (-6)), RegisterState {cycle = 236, value = 28})
              , (Nothing, RegisterState {cycle = 237, value = 28})
              , (Just (Addx (-11)), RegisterState {cycle = 238, value = 17})
              , (Just Noop, RegisterState {cycle = 239, value = 17})
              , (Just Noop, RegisterState {cycle = 240, value = 17})
              , (Just Noop, RegisterState {cycle = 241, value = 17})
              ]
            (catMaybes -> exampleInstructions, exampleStates) = unzip examplePairs
        runCheck
          parser
          part1
          part2
          Example
            { raw =
                [text|
            addx 15
            addx -11
            addx 6
            addx -3
            addx 5
            addx -1
            addx -8
            addx 13
            addx 4
            noop
            addx -1
            addx 5
            addx -1
            addx 5
            addx -1
            addx 5
            addx -1
            addx 5
            addx -1
            addx -35
            addx 1
            addx 24
            addx -19
            addx 1
            addx 16
            addx -11
            noop
            noop
            addx 21
            addx -15
            noop
            noop
            addx -3
            addx 9
            addx 1
            addx -3
            addx 8
            addx 1
            addx 5
            noop
            noop
            noop
            noop
            noop
            addx -36
            noop
            addx 1
            addx 7
            noop
            noop
            noop
            addx 2
            addx 6
            noop
            noop
            noop
            noop
            noop
            addx 1
            noop
            noop
            addx 7
            addx 1
            noop
            addx -13
            addx 13
            addx 7
            noop
            addx 1
            addx -33
            noop
            noop
            noop
            addx 2
            noop
            noop
            noop
            addx 8
            noop
            addx -1
            addx 2
            addx 1
            noop
            addx 17
            addx -9
            addx 1
            addx 1
            addx -3
            addx 11
            noop
            noop
            addx 1
            noop
            addx 1
            noop
            noop
            addx -13
            addx -19
            addx 1
            addx 3
            addx 26
            addx -30
            addx 12
            addx -1
            addx 3
            addx 1
            noop
            noop
            noop
            addx -9
            addx 18
            addx 1
            addx 2
            noop
            noop
            addx 9
            noop
            noop
            noop
            addx -1
            addx 2
            addx -37
            addx 1
            addx 3
            noop
            addx 15
            addx -21
            addx 22
            addx -6
            addx 1
            noop
            addx 2
            addx 1
            noop
            addx -10
            noop
            noop
            addx 20
            addx 1
            addx 2
            addx 2
            addx -6
            addx -11
            noop
            noop
            noop
          |]
            , parsed = exampleInstructions
            , part1output = 13140
            , part2output =
                Image
                  "##..##..##..##..##..##..##..##..##..##..\
                  \###...###...###...###...###...###...###.\
                  \####....####....####....####....####....\
                  \#####.....#####.....#####.....#####.....\
                  \######......######......######......####\
                  \#######.......#######.......#######....."
            }

        describe "toRegisterStates" do
          it "reports the correct toRegisterStates for the small example input" do
            toRegisterStates [Noop, Addx 3, Addx (-5)]
              `shouldBe` [ RegisterState 1 1
                         , RegisterState 2 1
                         , RegisterState 3 1
                         , RegisterState 4 4
                         , RegisterState 5 4
                         , RegisterState 6 (-1)
                         ]

          it "reports the correct toRegisterStates for the example input" do
            toRegisterStates exampleInstructions `shouldBe` exampleStates

        describe "sampleRegisterStates" do
          it "reports the correct samples for the example input" do
            sampleRegisterStates [20, 60 .. 220] (toRegisterStates exampleInstructions)
              `shouldBe` [ RegisterState 20 21
                         , RegisterState 60 19
                         , RegisterState 100 18
                         , RegisterState 140 21
                         , RegisterState 180 16
                         , RegisterState 220 18
                         ]
    }

type Instruction :: Type
data Instruction = Noop | Addx !Int
  deriving stock (Show, Eq)

type RegisterState :: Type
data RegisterState = RegisterState {cycle :: Int, value :: Int}
  deriving stock (Show, Eq)

parser :: Parser [Instruction]
parser = instruction `endBy` newline
  where
    instruction = noop <|> addx
    noop = Noop <$ string "noop"
    addx = Addx <$ string "addx " <*> int
    int = read <$> (negative <|> positive)
    negative = (:) <$> char '-' <*> positive
    positive = some digit

toRegisterStates :: [Instruction] -> [RegisterState]
toRegisterStates = zipWith RegisterState [1 ..] . concatMap fst . scanl' step ([1], 1)
  where
    step (_, v) = \case
      Noop -> ([v], v)
      Addx x -> ([v, x + v], x + v)

signalStrength :: RegisterState -> Int
signalStrength RegisterState {cycle, value} = cycle * value

sampleRegisterStates :: [Int] -> [RegisterState] -> [RegisterState]
sampleRegisterStates = loop undefined
  where
    loop r ns [] = map (\n -> r {cycle = n}) ns
    loop _ [] _ = []
    loop r ns@(n : nt) ws@(w : wt) = case compare (cycle w) n of
      LT -> loop w ns wt
      EQ -> w : loop w nt wt
      GT -> r {cycle = n} : loop r nt ws

part1 :: [Instruction] -> Int
part1 = sum . map signalStrength . sampleRegisterStates [20, 60 .. 220] . toRegisterStates

type Image :: Type
data Image = Image {fromImage :: String}
  deriving stock (Eq)

instance Show Image where
  show = unlines . unfoldr step . fromImage
    where
      step [] = Nothing
      step ps = Just (splitAt 40 ps)

part2 :: [Instruction] -> Image
part2 = Image . init . map check . toRegisterStates
  where
    check RegisterState {cycle, value}
      | abs (rem (cycle - 1) 40 - value) <= 1 = '#'
      | otherwise = '.'
