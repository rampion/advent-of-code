{-# LANGUAGE BangPatterns #-}
module AdventOfCode.Y2022.Day16 where

import AdventOfCode.Y2022.Prelude
import Control.Monad (replicateM)
-- import Data.List (permutations)
-- import Data.Map (Map, (!))
-- import Data.Map qualified as Map

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = xcheck parser part1 part2 Example
    { raw = [text|
        Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        Valve BB has flow rate=13; tunnels lead to valves CC, AA
        Valve CC has flow rate=2; tunnels lead to valves DD, BB
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        Valve EE has flow rate=3; tunnels lead to valves FF, DD
        Valve FF has flow rate=0; tunnels lead to valves EE, GG
        Valve GG has flow rate=0; tunnels lead to valves FF, HH
        Valve HH has flow rate=22; tunnel leads to valve GG
        Valve II has flow rate=0; tunnels lead to valves AA, JJ
        Valve JJ has flow rate=21; tunnel leads to valve II
      |] <> "\n"
    , parsed = 
        [ ( "AA", Valve{flowRate=0, tunnels=["DD", "II", "BB"]} )
        , ( "BB", Valve{flowRate=13, tunnels=["CC", "AA"]} )
        , ( "CC", Valve{flowRate=2, tunnels=["DD", "BB"]} )
        , ( "DD", Valve{flowRate=20, tunnels=["CC", "AA", "EE"]} )
        , ( "EE", Valve{flowRate=3, tunnels=["FF", "DD"]} )
        , ( "FF", Valve{flowRate=0, tunnels=["EE", "GG"]} )
        , ( "GG", Valve{flowRate=0, tunnels=["FF", "HH"]} )
        , ( "HH", Valve{flowRate=22, tunnels=["GG"]} )
        , ( "II", Valve{flowRate=0, tunnels=["AA", "JJ"]} )
        , ( "JJ", Valve{flowRate=21, tunnels=["II"]} )
        ]
    , part1output = 1651
    , part2output = error "unknown"
    }
  }

type Input :: Type
type Input = [(ValveName, Valve)]

type ValveName :: Type
type ValveName = String

type Valve :: Type
data Valve = Valve
  { flowRate :: Int
  , tunnels :: [ValveName]
  }
  deriving stock (Show, Eq)

parser :: Parser Input
parser = pair `endBy` newline where
  pair = (,) <$ string "Valve " <*> valveName <*> valve
  valveName = replicateM 2 upper
  valve = Valve <$ string " has flow rate=" <*> int <*> (try tunnels <|> tunnel)
  tunnels = string "; tunnels lead to valves " *> (valveName `sepBy` string ", ")
  tunnel  = pure <$ string "; tunnel leads to valve " <*> valveName
  int = read <$> many digit

start :: ValveName
start = "AA"

-- A - B - C
--
-- (X - 3) * C + (X - 5) * B
-- (X - 2) * B + (X - 4) * C
-- C - 3B

-- Work out the steps to release the most pressure in 30 minutes. What is the most pressure you can release?
part1 :: Input -> Int
part1 _ = 0
{-
part1 pairs = runST do
  cache <- newSTRef mempty

  let valveId :: Map ValveName -> ValveId
      valveId = Map.fromList do zipWith (<$) [0..] pairs

      flowRates :: IntMap Flow
      flowRates = IntMap.fromList [(valveId ! name, flow valve) | (valveId, valve) <- pairs]

      adjacent :: IntMap [ValveId]
      adjacent = IntMap.fromList [(valveId ! name, (valveId !) <$> valve) | (valveId, valve) <- pairs]

      nonzero :: IntSet
      nonzero = IntMap.keys do IntMap.filter (\Valve{flowRate} -> flowRate > 0) graph

      flow key = readSTRef cache >>= Map.lookup key >>> maybe
        do modifySTRef cache do Map.insert key (walk key)
        do pure

      bestRoute :: [Flow] -> Flow
      bestRoute = maximum . (0:)

      walk SearchKey{here,remain,closed} = bestRoute do
        
              
          


  walk (valveId ! "AA") 30 nonzero

  where

  where

  walk :: ValveId -> Time -> IntSet -> Flow
  walk here remain closed = maximum . (0:) $ do
    there <- IntSet.toList closed
    let remain' = remain - travel here there - 1
    guard do remain' > 0
    pure do 
      (flow there * remain') + cached ! Search{here=there,remain=remain',closed = IntSet.delete there closed }

    walk 0 30 start <$> permutations [key | (key,Valve{flowRate}) <- pairs, flowRate > 0]
  where
    walk :: Flow -> Time -> ValveName -> [ValveName] -> Int
    walk !total !time here = \case
      v@(remain time here -> time'):vs | 0 < time' -> walk (total + (flow ! v) * time') time' v vs
      _ -> total

    remain :: Time -> ValveName -> ValveName -> Time
    remain time here there = time - travel ! (here,there)

    travel :: Map (ValveName, ValveName) Time
    travel = distances pairs
    -}

type Flow :: Type
type Flow = Int

type Time :: Type
type Time = Int


part2 :: Input -> ()
part2 = error "unimplemented"
