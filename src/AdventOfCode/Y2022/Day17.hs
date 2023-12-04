{-# OPTIONS_GHC -Wno-name-shadowing #-}
module AdventOfCode.Y2022.Day17 where

import AdventOfCode.Y2022.Prelude hiding (Stream)
import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
-- import Data.Maybe (fromMaybe)
-- import Data.Functor ((<&>))
import Control.Monad (replicateM)

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = do
      let parsed = 
            R :|
            [ R
            , R
            , L
            , L
            , R
            , L
            , R
            , R
            , L
            , L
            , L
            , R
            , R
            , L
            , R
            , R
            , R
            , L
            , L
            , L
            , R
            , R
            , R
            , L
            , L
            , L
            , R
            , L
            , L
            , L
            , R
            , R
            , L
            , R
            , R
            , L
            , L
            , R
            , R
            ]
      check parser part1 part2 Example
        { raw = [text|>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>|]
        , parsed
        , part1output = 3068
        , part2output = 1_514_285_714_288
        }

      let ninthRaw = [text|
              |....@..|
              |....@..|
              |..@@@..|
              |.......|
              |.......|
              |.......|
              |...#...|
              |..###..|
              |...#...|
              |..####.|
              +-------+
            |]

      let momentLiteral = either (error . show) id . parse moment "example"

      describe "moment" do
        it "can parse the ninth moment" do
          parse moment "example" ninthRaw `shouldBe` Right Moment
            { falling = Shape [ Position 3 8, Position 4 8, Position 5 8, Position 5 9, Position 5 10]
            , chamber = Chamber . Set.fromList . concat $
                [ [ Position{left,bottom=1} | left <- [3..6] ]
                , [ Position 4 2, Position 3 3, Position 4 3, Position 5 3, Position 4 4]
                ]
            }

      describe "frames" do
        let moments = fst <$> frames parsed

        it "renders moment 0 properly" do
          moments !- 0 `shouldBe` Moment
              { falling = Shape [ Position{left,bottom=4} | left <- [3..6] ]
              , chamber = Chamber Set.empty
              }

        it "renders moment 1 properly" do
          moments !- 1 `shouldBe` Moment
              { falling = Shape [ Position{left,bottom=3} | left <- [4..7] ]
              , chamber = Chamber Set.empty
              }

        it "renders moment 2 properly" do
          moments !- 2 `shouldBe` Moment
              { falling = Shape [ Position{left,bottom=2} | left <- [4..7] ]
              , chamber = Chamber Set.empty
              }

        it "renders moment 3 properly" do
          moments !- 3 `shouldBe` Moment
              { falling = Shape [ Position{left,bottom=1} | left <- [4..7] ]
              , chamber = Chamber Set.empty
              }

        it "renders moment 4 properly" do
          moments !- 4 `shouldBe` Moment
              { falling = Shape [ Position 4 5, Position 3 6, Position 4 6, Position 5 6, Position 4 7]
              , chamber = Chamber (Set.fromList [ Position{left,bottom=1} | left <- [3..6] ])
              }
        it "renders moment 7 properly" do
          moments !- 7 `shouldBe` Moment
              { falling = Shape [ Position 3 2, Position 2 3, Position 3 3, Position 4 3, Position 3 4]
              , chamber = Chamber (Set.fromList [ Position{left,bottom=1} | left <- [3..6] ])
              }
        it "renders moment 8 properly" do
          moments !- 8 `shouldBe` momentLiteral ninthRaw

        it "renders moment 13 properly" do
          moments !- 13 `shouldBe` momentLiteral [text|
              |..@....|
              |..@....|
              |..@....|
              |..@....|
              |.......|
              |.......|
              |.......|
              |..#....|
              |..#....|
              |####...|
              |..###..|
              |...#...|
              |..####.|
              +-------+
            |]

        it "renders moment 20 properly" do
          moments !- 20 `shouldBe` momentLiteral [text|
              |..@@...|
              |..@@...|
              |.......|
              |.......|
              |.......|
              |....#..|
              |..#.#..|
              |..#.#..|
              |#####..|
              |..###..|
              |...#...|
              |..####.|
              +-------+
            |]

        it "renders moment 28 moment properly" do
          moments !- 28 `shouldBe` momentLiteral [text|
              |...@...|
              |..@@@..|
              |...@...|
              |.......|
              |.......|
              |.......|
              |.####..|
              |....##.|
              |....##.|
              |....#..|
              |..#.#..|
              |..#.#..|
              |#####..|
              |..###..|
              |...#...|
              |..####.|
              +-------+
            |]

        it "renders moment 32 moment properly" do
          moments !- 32 `shouldBe` momentLiteral [text|
              |....@..|
              |....@..|
              |..@@@..|
              |.......|
              |.......|
              |.......|
              |..#....|
              |.###...|
              |..#....|
              |.####..|
              |....##.|
              |....##.|
              |....#..|
              |..#.#..|
              |..#.#..|
              |#####..|
              |..###..|
              |...#...|
              |..####.|
              +-------+
            |]

        it "renders moment 37 moment properly" do
          moments !- 37 `shouldBe` momentLiteral [text|
              |..@....|
              |..@....|
              |..@....|
              |..@....|
              |.......|
              |.......|
              |.......|
              |.....#.|
              |.....#.|
              |..####.|
              |.###...|
              |..#....|
              |.####..|
              |....##.|
              |....##.|
              |....#..|
              |..#.#..|
              |..#.#..|
              |#####..|
              |..###..|
              |...#...|
              |..####.|
              +-------+
            |]

        it "renders moment 43 moment properly" do
          moments !- 43 `shouldBe` momentLiteral [text|
              |..@@...|
              |..@@...|
              |.......|
              |.......|
              |.......|
              |....#..|
              |....#..|
              |....##.|
              |....##.|
              |..####.|
              |.###...|
              |..#....|
              |.####..|
              |....##.|
              |....##.|
              |....#..|
              |..#.#..|
              |..#.#..|
              |#####..|
              |..###..|
              |...#...|
              |..####.|
              +-------+
            |]
  }

width :: Int
width = 7

leftOffset :: Int
leftOffset = 2

bottomOffset :: Int
bottomOffset = 3

total :: Int
total = 2022

type Fill :: Type
data Fill = Air | FallingRock | SettledRock
  deriving stock (Eq, Show)

moment :: Parser Moment
moment = mkMoment <$> (row `endBy` newline) <* string "+-------+" where
  row = char '|' *> replicateM 7 square <* char '|'
  square = air <|> fallingRock <|> settledRock
  air = Air <$ char '.'
  fallingRock = FallingRock <$ char '@'
  settledRock = SettledRock <$ char '#'

  mkMoment rows = Moment
    { falling = Shape (positions FallingRock rows)
    , chamber = Chamber do Set.fromList do positions SettledRock rows
    }

  positions fill rows = do
    (bottom, row) <- zip [1..] (reverse rows)
    (left, (== fill) -> True) <- zip [1..] row
    pure Position{left,bottom}

type Position :: Type
data Position = Position { left :: Int, bottom :: Int }
  deriving stock (Eq, Ord)

type Shape :: Type
newtype Shape = Shape { fromShape :: [Position] }
  deriving stock Eq 

type Chamber :: Type
newtype Chamber = Chamber { fromChamber :: Set Position }
  deriving stock Eq

type Moment :: Type
data Moment = Moment { falling :: Shape, chamber :: Chamber }
  deriving stock Eq

instance Show Moment where
  show (Moment (Shape (Set.fromList -> a)) (Chamber c)) = unlines do
    bottom <- reverse [0..maximum (Set.map bottom a)]
    pure do
      left <- [0..8]
      pure case Position{left,bottom} of
        p | p `Set.member` a -> '@'
          | p `Set.member` c -> '#'
          | bottom == 0 -> case left of 
              0 -> '+'
              8 -> '+'
              _ -> '-'
          | left == 0 -> '|'
          | left == 8 -> '|'
          | otherwise -> '.'

parseShape :: Text -> Shape
parseShape rows = Shape do
  (bottom,row) <- zip [0..] do reverse do lines do Text.unpack rows
  (left, '@') <- zip [0..] row
  pure Position{left,bottom}

shapes :: NonEmpty Shape
shapes = parseShape <$> [text|@@@@|] :|
  [ [text|
      .@.
      @@@
      .@.
    |]
  , [text|
      ..@
      ..@
      @@@
    |]
  , [text|
      @
      @
      @
      @
    |]
  , [text|
      @@
      @@
    |]
  ]

type Push :: Type
data Push = L | R
  deriving stock (Show, Eq)

type Stream :: Type -> Type
data Stream a = a :- Stream a
  deriving stock Functor

(!-) :: Stream a -> Natural -> a
(a :- _) !- 0 = a
(_ :- as) !- n = as !- (n - 1)

stream :: NonEmpty a -> Stream a
stream (a :| at) = as where
  as = a :- foldr (:-) as at

scan :: (s -> a -> s) -> s -> Stream a -> Stream s
scan f s (a :- as) = s :- scan f (f s a) as

parser :: Parser (NonEmpty Push)
parser = (:|) <$> push_ <*> many push_ where
  push_ = left <|> right
  left = L <$ char '<'
  right = R <$ char '>'

step :: (Moment, Stream Shape) -> Push -> (Moment, Stream Shape)
step (Moment{falling,chamber}, next) d = 
  let falling' = maybe falling Shape do traverse (clear chamber . push d) (fromShape falling)
  in
  case traverse (clear chamber . fall) (fromShape falling') of 
    Just (Shape -> falling) -> (Moment{falling, chamber}, next)
    Nothing -> setup next (settle falling' chamber)

settle :: Shape -> Chamber -> Chamber
settle (Shape x) (Chamber ps) = Chamber do Set.fromList x `Set.union` ps

setup :: Stream Shape -> Chamber -> (Moment, Stream Shape)
setup (Shape ps :- next) chamber = (Moment { falling, chamber }, next) where
  falling = Shape [p { left = left p + 3, bottom = bottom p + 4 + highest } | p <- ps]
  highest = height chamber

push :: Push -> Position -> Position
push L p = p { left = left p - 1 }
push R p = p { left = left p + 1 }

fall :: Position -> Position
fall p = p { bottom = bottom p - 1 }

height :: Chamber -> Int
height (Chamber ps)
  | Set.null ps = 0
  | otherwise = maximum do Set.map bottom ps

clear :: Chamber -> Position -> Maybe Position
clear (Chamber ps) p 
  | p `Set.member` ps || left p < 1 || 7 < left p || bottom p < 1 = Nothing
  | otherwise = Just p

part1 :: NonEmpty Push -> Int
part1 = height . chamber . fst . (!- 2022) . settlements . frames

settlements :: Stream (Moment, Stream Shape) -> Stream (Moment, Stream Shape)
settlements (p@(_, x:-_) :- ps) = p :- settlements (dropWhile' (\(_, y :- _) -> y == x) ps)

dropWhile' :: (a -> Bool) -> Stream a -> Stream a
dropWhile' p as@(a :- at)
  | p a  = dropWhile' p at
  | otherwise = as

frames :: NonEmpty Push -> Stream (Moment, Stream Shape)
frames = scan step (setup (stream shapes) (Chamber Set.empty)) . stream

part2 :: NonEmpty Push -> Int
part2 = error "too slow" `seq` height . chamber . fst . (!- 1_000_000_000_000) . settlements . frames
