module AdventOfCode.Y2022.Day19 where

import AdventOfCode.Y2022.Prelude
import Data.Functor (($>))
import Data.Monoid (Sum(..))
import Data.Foldable (fold)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = fcheck parser part1 part2 Example
    { raw = [text|
        Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
        Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
      |]
    , parsed =
      [ Blueprint
        { ident = 1
        , recipes = ResourceMap
          { ore       = mempty { ore = 4 }
          , clay      = mempty { ore = 2 }
          , obsidian  = mempty { ore = 3, clay = 14 }
          , geode     = mempty { ore = 2, obsidian = 7 }
          }
        }
      , Blueprint
        { ident = 2
        , recipes = ResourceMap
          { ore       = mempty { ore = 2 }
          , clay      = mempty { ore = 3 }
          , obsidian  = mempty { ore = 3, clay = 8 }
          , geode     = mempty { ore = 3, obsidian = 12 }
          }
        }
      ]
    , part1output = 33
    , part2output = error "unknown"
    }
  }

type Blueprint :: Type
data Blueprint = Blueprint
  { ident :: Int
  , recipes :: ResourceMap ResourceCount
  }
  deriving stock (Show, Eq)

type ResourceCount :: Type
type ResourceCount = ResourceMap (Sum Int)

type ResourceMap :: Type -> Type
data ResourceMap a = ResourceMap
  { ore :: a
  , clay :: a
  , obsidian :: a
  , geode :: a
  }
  deriving stock (Show, Eq, Functor)

instance Monoid a => Monoid (ResourceMap a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (ResourceMap a) where
  (<>) = liftA2 (<>)

instance Applicative ResourceMap where
  pure a = ResourceMap a a a a
  ResourceMap f0 f1 f2 f3 <*> ResourceMap a0 a1 a2 a3 = ResourceMap (f0 a0) (f1 a1) (f2 a2) (f3 a3)

parser :: Parser [Blueprint]
parser = blueprint `sepBy` newline where
  blueprint = Blueprint <$> ident <*> recipes

  ident :: Parser Int
  ident = string "Blueprint " *> int <* char ':'

  recipes :: Parser (ResourceMap ResourceCount)
  recipes = ResourceMap <$> cost "ore" <*> cost "clay" <*> cost "obsidian" <*> cost "geode"

  cost :: String -> Parser ResourceCount
  cost robot = fold <$ string (" Each " <> robot <> " robot costs ") <*> (resource `sepBy` string " and ") <* string "."

  resource :: Parser ResourceCount
  resource = Sum <$> int <* char ' ' <**> asum
    [ tstring "ore" $> \ore -> mempty {ore}
    , tstring "clay" $> \clay -> mempty {clay}
    , tstring "obsidian" $> \obsidian -> mempty {obsidian}
    , tstring "geode" $> \geode -> mempty {geode}
    ]

  int :: Parser Int
  int = read <$> some digit
  tstring = try . string

part1 :: [Blueprint] -> Int
part1 = sum . fmap quality

quality :: Blueprint -> Int
quality Blueprint{ident,recipes} = ident * maxGeodes 24 recipes mempty{ore=1}

maxGeodes :: Int -> ResourceMap ResourceCount -> ResourceCount -> Int
maxGeodes _numMinutes _recipes _robots = 0

{-
  4  0  0  0 |  1  0  0  0
  2  0  0  0 |  0  1  0  0
  3 14  0  0 |  0  0  1  0
  2  0  7  0 |  0  0  0  1

  0  0  0  0    1  0  0  0
  1  0  0  0    1  0  0  0
  2  0  0  0    1  0  0  0
  1  0  0  0    1  1  0  0
  2  1  0  0    1  1  0  0
  2  1  0  0    1  1  0  0
-}

part2 :: [Blueprint] -> ()
part2 _ = ()
