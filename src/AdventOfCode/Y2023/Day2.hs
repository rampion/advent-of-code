{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Y2023.Day2 where

import AdventOfCode.Y2023.Prelude
import Data.Coerce (Coercible, coerce)
import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..))

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = do
        check parser part1 part2 do
          Example
            { raw =
                [text|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
                |]
                  <> "\n"
            , parsed =
                [ Game
                    1
                    [ def {blue = 3, red = 4}
                    , def {red = 1, green = 2, blue = 6}
                    , def {green = 2}
                    ]
                , Game
                    2
                    [ def {blue = 1, green = 2}
                    , def {green = 3, blue = 4, red = 1}
                    , def {green = 1, blue = 1}
                    ]
                , Game
                    3
                    [ def {green = 8, blue = 6, red = 20}
                    , def {blue = 5, red = 4, green = 13}
                    , def {green = 5, red = 1}
                    ]
                , Game
                    4
                    [ def {green = 1, red = 3, blue = 6}
                    , def {green = 3, red = 6}
                    , def {green = 3, blue = 15, red = 14}
                    ]
                , Game
                    5
                    [ def {red = 6, blue = 1, green = 3}
                    , def {blue = 2, red = 1, green = 2}
                    ]
                ]
            , part1output = 8
            , part2output = 2286
            }
    }

type Game :: Type
data Game = Game
  { idNum :: Word
  , handfuls :: [Handful Sum]
  }
  deriving stock (Show, Eq)

type Handful :: (Type -> Type) -> Type
data Handful f = Handful
  { red :: f Word
  , green :: f Word
  , blue :: f Word
  }

deriving stock instance Show (f Word) => Show (Handful f)

deriving stock instance Eq (f Word) => Eq (Handful f)

instance Semigroup (m Word) => Semigroup (Handful m) where
  Handful r g b <> Handful r' g' b' = Handful (r <> r') (g <> g') (b <> b')

instance Monoid (m Word) => Monoid (Handful m) where
  mempty = Handful mempty mempty mempty

def :: Handful Sum
def = mempty

parser :: Parser [Game]
parser = game `endBy1` newline
  where
    game :: Parser Game
    game = Game <$ string "Game " <*> word <* string ": " <*> (handful `sepBy1` string "; ")

    handful :: Parser (Handful Sum)
    handful = mconcat <$> cubes `sepBy1` string ", "

    cubes :: Parser (Handful Sum)
    cubes = count <* string " " <**> label

    count :: Parser (Sum Word)
    count = Sum <$> word

    label :: Parser (Sum Word -> Handful Sum)
    label =
      asum
        [ string "red" $> \red -> mempty {red}
        , string "green" $> \green -> mempty {green}
        , string "blue" $> \blue -> mempty {blue}
        ]

    word :: Parser Word
    word = fmap read do
      (:) <$> oneOf ['1' .. '9'] <*> many digit

part1 :: [Game] -> Word
part1 = sum . map idNum . filter (atMost bagLimit . foldMap cmap . handfuls)
  where
    bagLimit = Handful @Max 12 13 14
    atMost m v = (m <> v) == m

part2 :: [Game] -> Word
part2 = sum . fmap (getMax . power . foldMap cmap . handfuls)
  where
    power (Handful r g b) = r * g * b

cmap :: Coercible (f Word) (g Word) => Handful f -> Handful g
cmap (Handful r g b) = Handful (coerce r) (coerce g) (coerce b)
