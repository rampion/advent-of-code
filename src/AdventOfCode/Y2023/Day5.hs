{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AdventOfCode.Y2023.Day5 where

import AdventOfCode.Y2023.Prelude hiding (drop, take)
import Data.List (sortBy)
import Data.Ord (comparing)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            seeds: 79 14 55 13

            seed-to-soil map:
            50 98 2
            52 50 48

            soil-to-fertilizer map:
            0 15 37
            37 52 2
            39 0 15

            fertilizer-to-water map:
            49 53 8
            0 11 42
            42 0 7
            57 7 4

            water-to-light map:
            88 18 7
            18 25 70

            light-to-temperature map:
            45 77 23
            81 45 19
            68 64 13

            temperature-to-humidity map:
            0 69 1
            1 0 69

            humidity-to-location map:
            60 56 37
            56 93 4
          |]
        , parsed
        , part1output = 35
        , part2output = 46
        }

      describe "forward" do
        it "maps the seeds correctly" do
          let step (_, x) map = (map.destination, forward map x)
              steps x = scanl step ("seed", x) parsed.maps
          fmap steps parsed.seeds `shouldBe`
            [ [("seed", 79), ("soil", 81), ("fertilizer", 81), ("water", 81), ("light", 74), ("temperature", 78), ("humidity", 78), ("location", 82)]
            , [("seed", 14), ("soil", 14), ("fertilizer", 53), ("water", 49), ("light", 42), ("temperature", 42), ("humidity", 43), ("location", 43)]
            , [("seed", 55), ("soil", 57), ("fertilizer", 57), ("water", 53), ("light", 46), ("temperature", 82), ("humidity", 82), ("location", 86)]
            , [("seed", 13), ("soil", 13), ("fertilizer", 52), ("water", 41), ("light", 34), ("temperature", 34), ("humidity", 35), ("location", 35)]
            ]
  }

parsed :: Input
parsed = Input
  { seeds = [79,14,55,13]
  , maps = 
    [ Map "seed" "soil" [Range 50 98 2, Range 52 50 48]
    , Map "soil" "fertilizer" [Range 0 15 37, Range 37 52 2, Range 39 0 15]
    , Map "fertilizer" "water" [Range 49 53 8, Range 0 11 42, Range 42 0 7, Range 57 7 4]
    , Map "water" "light" [Range 88 18 7, Range 18 25 70]
    , Map "light" "temperature" [Range 45 77 23, Range 81 45 19, Range 68 64 13]
    , Map "temperature" "humidity" [Range 0 69 1, Range 1 0 69]
    , Map "humidity" "location" [Range 60 56 37, Range 56 93 4]
    ]
  }

type Input :: Type
data Input = Input
  { seeds :: [Int]
  , maps :: [Map]
  }
  deriving stock (Show, Eq)

type Map :: Type
data Map = Map
  { source :: String
  , destination :: String
  , ranges :: [Range]
  }
  deriving stock (Show, Eq)

type Range :: Type
data Range = Range
  { dst :: Int
  , src :: Int
  , len :: Int
  }
  deriving stock (Show, Eq)

forward :: Map -> Int -> Int
forward = foldr step id . ranges where
  step r next i 
    | r.src <= i && i < r.src + r.len = r.dst + i - r.src
    | otherwise = next i

srcEnd :: Range -> Int
srcEnd r = r.src + r.len

dstEnd :: Range -> Int
dstEnd r = r.dst + r.len

take :: Int -> Range -> Range
take n r = r { len = min n r.len }

drop :: Int -> Range -> Range
drop n r = Range { src = r.src + n, dst = r.dst + n, len = max 0 (len r - n) }

compose :: Map -> Map -> Map
compose = \f g -> Map 
  { source = g.source
  , destination = f.destination
  , ranges = loop (sortBy (comparing src) f.ranges) (sortBy (comparing dst) g.ranges)
  } where
  loop [] gs = gs
  loop fs [] = fs
  loop fs@(f:ft) gs@(g:gt) 
    | srcEnd f < dst g = f : loop ft gs
    | dstEnd g < src f = g : loop fs gt
    | otherwise = case src f `compare` dst g of
        LT -> 
          let off = dst g - src f
              f' = drop off f
          in
          take off f : case f'.len `compare` g.len of
            LT -> f' { src = g.src } : loop ft (drop f'.len g : gt)
            EQ -> g { dst = f'.dst } : loop ft gt
            GT -> g { dst = f'.dst } : loop (drop g.len f' : ft) gt
        EQ ->
          case f.len `compare` g.len of
            LT -> f { src = g.src } : loop ft (drop f.len g : gt)
            EQ -> g { dst = f.dst } : loop ft gt
            GT -> g { dst = f.dst } : loop (drop g.len f : ft) gt
        GT ->
          let off = src f - dst g
              g' = drop off g
          in
          take off g : case f.len `compare` g'.len of
            LT -> f { src = g'.src } : loop ft (drop f.len g' : gt)
            EQ -> g' { dst = f.dst } : loop ft gt
            GT -> g' { dst = f.dst } : loop (drop g'.len f : ft) gt

parser :: Parser Input
parser = Input <$> seeds <* newline <*> maps where
  seeds = string "seeds: " *> sepBy int space <* newline
  int = read <$> many1 digit 
  space = char ' '
  maps = map `sepBy` newline
  map = Map <$> word <* string "-to-" <*> word <* string " map:" <* newline <*> endBy1 range newline
  word = many1 (oneOf ['a'..'z'])
  range = Range <$> int <* space <*> int <* space <*> int

part1 :: Input -> Int
part1 input = minimum (forward location <$> input.seeds ) where
  location = foldl1 (flip compose) input.maps

part2 :: Input -> Int
part2 input = minimum [ loc | (loc, _) <- forwardAll location (pairs input.seeds) ] where
  location = foldl1 (flip compose) input.maps

pairs :: [a] -> [(a,a)]
pairs = loop id where
  loop k [] = k []
  loop _ [_] = []
  loop k (a₀:a₁:as) = loop (k . ((a₀,a₁):)) as

forwardAll :: Map -> [(Int, Int)] -> [(Int, Int)]
forwardAll = \map -> loop (sortBy (comparing src) map.ranges) . sortBy (comparing fst)  where
  loop [] ps = ps
  loop _ [] = []
  loop rs@(r:rt) ps@(p@(loc,cnt):pt)
    | srcEnd r < loc = loop rt ps
    | loc + cnt < src r = p : loop rs pt
    | otherwise = case src r `compare` loc of
        LT -> 
          let off = loc - src r
              rem = r.len - off in
          case rem `compare` cnt of
            LT -> (r.dst + off, rem) : loop rt ((loc + rem, cnt - rem):pt)
            EQ -> (r.dst + off, rem) : loop rt pt
            GT -> (r.dst + off, cnt) : loop rs pt
        EQ ->
          case r.len `compare` cnt of
            LT -> (r.dst, r.len) : loop rt ((loc + r.len, cnt - r.len):pt)
            EQ -> (r.dst, r.len) : loop rt pt
            GT -> (r.dst, cnt) : loop rs pt
        GT ->
          let off = src r - loc
              rem = cnt - off in
          (loc, off) : case r.len `compare` rem of
            LT -> (r.dst, r.len) : loop rt ((r.src + r.len, rem - r.len):pt)
            EQ -> (r.dst, r.len) : loop rt pt
            GT -> (r.dst, rem) : loop rs pt

