{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day8 where

import AdventOfCode.Y2023.Prelude
import Data.Functor (($>))
import Control.Monad (replicateM, guard)
import Data.Map.Strict qualified as Map

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      let firstExample = Input 
            { instructions = [R,L]
            , network =
              [ ("AAA", Pair "BBB" "CCC")
              , ("BBB", Pair "DDD" "EEE")
              , ("CCC", Pair "ZZZ" "GGG")
              , ("DDD", Pair "DDD" "DDD")
              , ("EEE", Pair "EEE" "EEE")
              , ("GGG", Pair "GGG" "GGG")
              , ("ZZZ", Pair "ZZZ" "ZZZ")
              ]
            }

      check parser part1 part2 Example
        { raw = (<> "\n") [text|
            RL

            AAA = (BBB, CCC)
            BBB = (DDD, EEE)
            CCC = (ZZZ, GGG)
            DDD = (DDD, DDD)
            EEE = (EEE, EEE)
            GGG = (GGG, GGG)
            ZZZ = (ZZZ, ZZZ)
          |]
        , parsed = firstExample 
        , part1output = 2
        , part2output = 2
        }

      let firstPattern = Pattern
            { offsetSize = 2
            , wheelSize = 2
            , offset = []
            , wheel = [0,1]
            }

      let firstAnalysis =
            [ Analysis
              { offsetElems = ["AAA", "CCC"]
              , wheelElems = ["ZZZ","ZZZ"]
              , pattern_ = firstPattern
              }
            ]

      it "correctly analyzes the first example" do
        analyze firstExample `shouldBe` firstAnalysis

      it "correctly foldMaps the analysis from the first pattern" do
        foldMap pattern_ firstAnalysis `shouldBe` firstPattern

      it "correctly resizes the first pattern" do
        resize 2 2 firstPattern `shouldBe` firstPattern

      it "correctly resizes mempty" do
        resize 2 2 mempty `shouldBe` Pattern
          { offsetSize = 2
          , wheelSize = 2
          , offset = [0,1]
          , wheel = [0,1]
          }

      let secondExample = Input
            { instructions = [L,L,R]
            , network =
              [ ("AAA", Pair "BBB" "BBB")
              , ("BBB", Pair "AAA" "ZZZ")
              , ("ZZZ", Pair "ZZZ" "ZZZ")
              ]
            }

      it "correctly solves part 1 for the second example" do
        part1 secondExample `shouldBe` 6

      it "correctly analyzes the second example" do
        analyze secondExample `shouldBe`
          [ Analysis
            { offsetElems = ["AAA", "BBB", "AAA", "BBB", "AAA", "BBB"]
            , wheelElems = ["ZZZ","ZZZ","ZZZ"]
            , pattern_ = Pattern
              { offsetSize = 6
              , wheelSize = 3
              , offset = []
              , wheel = [0,1,2]
              }
            }
          ]

      it "correctly gets the offset and wheel lengths for the second example" do
        getPatternSizes 3 ("AAA" : "BBB" : "AAA" : "BBB" : "AAA" : "BBB" : repeat "ZZZ")
          `shouldBe` (6, 3)

      let thirdExample = Input
            { instructions = [L,R]
            , network =
              [ ("11A", Pair "11B" "XXX")
              , ("11B", Pair "XXX" "11Z")
              , ("11Z", Pair "11B" "XXX")
              , ("22A", Pair "22B" "XXX")
              , ("22B", Pair "22C" "22C")
              , ("22C", Pair "22Z" "22Z")
              , ("22Z", Pair "22B" "22B")
              , ("XXX", Pair "XXX" "XXX")
              ]
            }

      let thirdAnalysis =
            [ Analysis
              { offsetElems = ["11A"] 
              , wheelElems = ["11B","11Z"]
              , pattern_ = Pattern
                { offsetSize = 1
                , wheelSize = 2
                , offset = []
                , wheel = [1]
                }
              }
            , Analysis
              { offsetElems = ["22A"]
              , wheelElems = ["22B", "22C", "22Z","22B", "22C", "22Z"]
              , pattern_ = Pattern
                { offsetSize = 1
                , wheelSize = 6
                , offset = []
                , wheel = [2,5]
                }
              }
            ]

      it "correctly gets the offset and wheel lengths for one of the third paths" do
        getPatternSizes 2 ("11A" : cycle ["11B", "11Z"]) `shouldBe` (1, 2)

      it "correctly gets the offset and wheel lengths for the other of the third paths" do
        getPatternSizes 2 ("22A" : cycle ["22B", "22C", "22Z"]) `shouldBe` (1, 6)

        -- 11A 11B 11Z 11B 11Z 11B 11Z 11B 11Z
        -- 0   1   2   3   4   5   6   7   8
        -- ✗   ✗   ✗   1
        --
        -- 22A 22B 22C 22Z 22B 22C 22Z
        -- 0   1   2   3   4   5   6
        -- ✗   ✗   ✗   ✗   1

      it "correctly analyzes the third example" do
        analyze thirdExample `shouldBe` thirdAnalysis

      it "correctly solves part 2 for the third example" do
        part2 thirdExample `shouldBe` 6
  }

type Input :: Type
data Input = Input
  { instructions :: [Instruction]
  , network :: [(Node, Pair Node)]
  }
  deriving stock (Show, Eq)

type Instruction :: Type
data Instruction = R | L
  deriving stock (Show, Eq, Enum, Ord)

type Node :: Type
type Node = String

type Pair :: Type -> Type
data Pair a = Pair { left :: a, right :: a }
  deriving stock (Show, Eq)

parser :: Parser Input
parser = Input <$> instructions <* newline <*> many nodeNeighbor where
  instructions = many instruction <* newline
  instruction = (char 'L' $> L) <|> (char 'R' $> R)
  nodeNeighbor = (,) <$> node <* string " = " <*> neighbor <* newline
  neighbor = Pair <$ char '(' <*> node <* string ", " <*> node <* char ')'
  node = replicateM 3 do oneOf (['A'..'Z'] <> ['0' .. '9'])

part1 :: Input -> Int
part1 input 
  | Map.member "AAA" network = length path
  | otherwise = 0
  where
    network = Map.fromList input.network
    instructions = cycle input.instructions
    path = takeWhile (/= "ZZZ") $ scanl follow "AAA" instructions
    follow here instruction = network Map.! here ? instruction
  
(?) :: Pair x -> Instruction -> x
p ? L = p.left
p ? R = p.right

infixl 8 ?

part2 :: Input -> Int
part2 = earliest . foldMap pattern_ . analyze

analyze :: Input -> [Analysis]
analyze input = do
  let network = Map.fromList
        [ (reverse k, Pair (reverse l) (reverse r))
        | (k, Pair l r) <- input.network
        ]

      modulus = length input.instructions

      instructions = cycle input.instructions

      follow here instruction = network Map.! here ? instruction

  key <- Map.keys network
  
  guard do isInitial key 

  let path = scanl follow key instructions

      (offsetSize,wheelSize) = getPatternSizes modulus path

      (offsetElems, wheelElems) 
        = splitAt offsetSize 
        . take (offsetSize + wheelSize) 
        $ path

  pure Analysis
    { pattern_ = Pattern
      { offset = compact offsetElems
      , wheel = compact wheelElems
      , ..
      }
    , offsetElems = map reverse offsetElems
    , wheelElems = map reverse wheelElems
    }

type Analysis :: Type
data Analysis = Analysis
  { offsetElems :: [String]
  , wheelElems :: [String]
  , pattern_ :: Pattern
  }
  deriving stock (Show, Eq)

isInitial :: String -> Bool
isInitial = test 'A'

isFinal :: String -> Bool
isFinal = test 'Z'

test :: Char -> String -> Bool
test c = \case
  c':_ -> c' == c
  _ -> False

type Pattern :: Type
data Pattern = Pattern
  { offsetSize :: Int
  , wheelSize :: Int
  , offset :: [Int]
  , wheel :: [Int]
  }
  deriving stock (Show, Eq)

earliest :: Pattern -> Int
earliest = \case
  Pattern{offset=n:_} -> n
  Pattern{wheel=n:_,offsetSize} -> n + offsetSize
  _ -> -1

compact :: [String] -> [Int]
compact bs = do
  (i,b) <- zip [0..] bs
  guard do isFinal b
  pure i

getPatternSizes :: Int -> [String] -> (Int, Int)
getPatternSizes modulus = loop 0 Map.empty where
  loop !i m (x:xs) = case Map.lookup p m of
    Just j -> (j, i - j)
    _ -> loop (i + 1) (Map.insert p i m) xs
    where p = (i `rem` modulus, x)
  loop _ _ [] = error "should have given me an infinite list"

instance Semigroup Pattern where
  p <> q = Pattern{..} where
    offsetSize = max p.offsetSize q.offsetSize
    wheelSize = lcm p.wheelSize q.wheelSize
    p' = resize offsetSize wheelSize p
    q' = resize offsetSize wheelSize q
    offset = merge p'.offset q'.offset
    wheel = merge p'.wheel q'.wheel

instance Monoid Pattern where
  mempty = Pattern 0 1 [] [0]
    
resize :: Int -> Int -> Pattern -> Pattern
resize offsetSize wheelSize p = Pattern{..} where
  ps = p.offset <> [p.offsetSize + n*p.wheelSize + i | n <- [0..], i <- p.wheel]
  (offset, takeWhile (< wheelSize) . map (subtract offsetSize) -> wheel) = span (< offsetSize) ps

merge :: [Int] -> [Int] -> [Int]
merge [] _ = []
merge _ [] = []
merge (a:at) (b:bt) = merge' a at b bt

merge' :: Int -> [Int] -> Int -> [Int] -> [Int]
merge' a as b bs = case compare a b of
  LT -> case as of [] -> []; a:as -> merge' a as b bs
  EQ -> a : merge as bs
  GT -> case bs of [] -> []; b:bs -> merge' a as b bs
