{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Test.HUnit
import Text.Parsec (runParser, char, newline, sepBy, choice, digit)
import Text.Parsec.String (parseFromFile, Parser)
import Control.Arrow ((***))
import Control.Monad (guard, (>=>))
import Control.Monad.State (evalState, state)
import Data.Functor (($>))
import Control.Applicative (some)

-- example1 => 6
-- example2 => 159
-- example3 => 135

main :: IO ()
main = do
  runTestTT . TestList . map TestCase $ 
    [ ([(R,8),(U,5),(L,5),(D,3)]
      ,[(U,7),(R,6),(D,4),(L,4)]
      ) @<-? parseSteps "2019/day/3/example1"
    , [ Segment Horizontal 0 8 0
      , Segment Vertical 0 5 8
      , Segment Horizontal 3 8 5
      , Segment Vertical 2 5 3
      ] @=? toSegments [(R,8),(U,5),(L,5),(D,3)]
    , [ Point 0 0
      , Point 6 5
      , Point 3 3
      ] @=? findIntersections 
              [ Segment Horizontal 0 8 0
              , Segment Vertical 0 5 8
              , Segment Horizontal 3 8 5
              , Segment Vertical 2 5 3
              ]
              [ Segment Vertical 0 7 0
              , Segment Horizontal 0 6 7
              , Segment Vertical 3 7 6
              , Segment Horizontal 2 6 3
              ]
    , 6 @<-? findMinIntersectionDistance "2019/day/3/example1"
    , 159 @<-? findMinIntersectionDistance "2019/day/3/example2"
    , 135 @<-? findMinIntersectionDistance "2019/day/3/example3"
    ] 
  print =<< findMinIntersectionDistance "2019/day/3/input"

(@<-?) :: (Eq a, Show a) => a -> IO a -> Assertion
expected @<-? action = (expected @=?) =<< action


findMinIntersectionDistance :: FilePath -> IO Int
findMinIntersectionDistance path =
  minimum . 
  map getMinDistance . 
  concatMap removeOrigin .
  uncurry findIntersections . 
  (toSegments *** toSegments) <$>
  parseSteps path

data Direction = U | R | D | L deriving (Eq, Show)
type Step = (Direction, Int)

data Orientation = Vertical | Horizontal deriving (Eq, Show)
data Segment = Segment
  { orientation :: Orientation
  , minimal :: Int
  , maximal :: Int
  , constant :: Int
  }
  deriving (Eq, Show)

data Intersection = Point Int Int | Span Segment
  deriving (Eq, Show)

getMinDistance :: Intersection -> Int
getMinDistance (Point x y) = abs x + abs y
getMinDistance (Span Segment{..})
  | minimal >= 0 = minimal + abs constant
  | maximal <= 0 = abs constant - maximal
  | otherwise    = abs constant

removeOrigin :: Intersection -> [Intersection]
removeOrigin (Point 0 0) = []
removeOrigin (Span (Segment o lo hi 0)) 
  | lo <= 0 && 0 <= hi = [ Span (Segment o lo (-1) 0) | lo < 0 ] ++
                         [ Span (Segment o 1    hi 0) | hi > 0 ]
removeOrigin p = [p]

findIntersections :: [Segment] -> [Segment] -> [Intersection]
findIntersections as bs = do
  a <- as
  b <- bs
  if orientation a == orientation b
    then do
      guard $ constant a == constant b
      let lo = minimal a `max` minimal b
          hi = maximal a `min` maximal b
      case lo `compare` hi of
        LT -> return $ Span Segment
                            { minimal     = lo
                            , maximal     = hi
                            , constant    = constant a
                            , orientation = orientation a
                            }
        EQ -> case orientation a of
                Vertical    -> return $ Point (constant a) lo
                Horizontal  -> return $ Point lo (constant a)
        GT -> []
    else do
      guard $ minimal a <= constant b && constant b <= maximal a
           && minimal b <= constant a && constant a <= maximal b
      return $ case orientation a of
        Vertical    -> Point (constant a) (constant b)
        Horizontal  -> Point (constant b) (constant a)

type Coord = (Int, Int)

toSegments :: [Step] -> [Segment]
toSegments = flip evalState (0,0) . mapM (state . flip step) where
  step :: Coord -> Step -> (Segment, Coord)
  step (x,y) (U, (y+) -> y') = (Segment Vertical y y' x, (x, y'))
  step (x,y) (R, (x+) -> x') = (Segment Horizontal x x' y, (x', y))
  step (x,y) (D, (y-) -> y') = (Segment Vertical y' y x, (x, y'))
  step (x,y) (L, (x-) -> x') = (Segment Horizontal x' x y, (x', y))

parseSteps :: FilePath -> IO ([Step], [Step])
parseSteps = parseFromFile parser >=> \case
    Left msg -> fail (show msg)
    Right v  -> return v
  where
    parser :: Parser ([Step], [Step])
    parser = (,) <$> steps <* newline <*> steps

    steps :: Parser [Step]
    steps = step `sepBy` char ','

    step :: Parser Step
    step  = (,) <$> direction <*> distance

    direction :: Parser Direction
    direction = choice 
      [ char 'U' $> U
      , char 'R' $> R
      , char 'D' $> D
      , char 'L' $> L
      ]

    distance :: Parser Int
    distance = read <$> some digit
