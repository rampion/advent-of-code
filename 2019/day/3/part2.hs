{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Maybe (catMaybes)
import Test.HUnit
import Text.Parsec (runParser, char, newline, sepBy, choice, digit)
import Text.Parsec.String (parseFromFile, Parser)
import Control.Arrow ((***))
import Control.Monad (guard, (>=>))
import Control.Monad.State (evalState, state)
import Data.Functor (($>))
import Control.Applicative (some)

main :: IO ()
main = do
  runTestTT . TestList . map TestCase $ 
    [ 30 @<-? findMinIntersectionSteps "2019/day/3/example1"
    , 610 @<-? findMinIntersectionSteps "2019/day/3/example2"
    , 410 @<-? findMinIntersectionSteps "2019/day/3/example3"
    , Nothing @=? findIntersection (Segment U 1 0 0 0) (Segment U 1 1 0 0)
    , Just (Point 0 1 1) @=? findIntersection (Segment U 1 0 0 0) (Segment U 1 0 1 0)
    , Just (Point 0 5 5) @=? findIntersection (Segment U 10 0 0 0) (Segment U 8 0 5 0)
    , Just (Point 0 0 5) @=? findIntersection (Segment U 10 0 0 0) (Segment D 8 0 5 0)
    ] 
  print =<< findMinIntersectionSteps "2019/day/3/input"

(@<-?) :: (Eq a, Show a) => a -> IO a -> Assertion
expected @<-? action = (expected @=?) =<< action

findMinIntersectionSteps :: FilePath -> IO Int
findMinIntersectionSteps path =
  minimum . 
  map getMinSteps . 
  concatMap removeOrigin .
  uncurry findIntersections . 
  (toSegments *** toSegments) <$>
  parseSteps path

data Direction = U | R | D | L deriving (Eq, Show)
type Step = (Direction, Int)

data Segment = Segment
  { direction :: Direction
  , delta     :: Int
  , xCoord    :: Int
  , yCoord    :: Int
  , numSteps  :: Int
  }
  deriving (Eq, Show)

data Intersection = Point Int Int Int
  deriving (Eq, Show)

getMinSteps :: Intersection -> Int
getMinSteps (Point x y n) = n

removeOrigin :: Intersection -> [Intersection]
removeOrigin (Point 0 0 _) = []
removeOrigin p = [p]

dim :: Segment -> (Int,Int,Int,Int,Int -> Int -> Int)
dim Segment{..} = case direction of
  U -> (xCoord,yCoord,xCoord,yCoord+delta,\_ y -> numSteps + y - yCoord)
  R -> (xCoord,yCoord,xCoord+delta,yCoord,\x _ -> numSteps + x - xCoord)
  D -> (xCoord,yCoord-delta,xCoord,yCoord,\_ y -> numSteps + yCoord - y)
  L -> (xCoord-delta,yCoord,xCoord,yCoord,\x _ -> numSteps + xCoord - x)

findIntersections :: [Segment] -> [Segment] -> [Intersection]
findIntersections as bs = catMaybes $ findIntersection <$> as <*> bs

findIntersection :: Segment -> Segment -> Maybe Intersection
findIntersection a b = do
  let (aX,aY,aX',aY',aD) = dim a
      (bX,bY,bX',bY',bD) = dim b

      loX = aX  `max` bX
      hiX = aX' `min` bX'
      loY = aY  `max` bY
      hiY = aY' `min` bY'

  guard $ loX <= hiX && loY <= hiY
  return $ case direction a of
    U -> Point loX loY (aD loX loY + bD loX loY)
    R -> Point loX loY (aD loX loY + bD loX loY)
    D -> Point hiX hiY (aD hiX hiY + bD hiX hiY)
    L -> Point hiX hiY (aD hiX hiY + bD hiX hiY)

type Coord = (Int, Int, Int)

toSegments :: [Step] -> [Segment]
toSegments = flip evalState (0,0,0) . mapM (state . flip step) where
  step :: Coord -> Step -> (Segment, Coord)
  step (x,y,n) (U, d) = (Segment U d x y n, (x, y+d, n + d))
  step (x,y,n) (R, d) = (Segment R d x y n, (x+d, y, n + d))
  step (x,y,n) (D, d) = (Segment D d x y n, (x, y-d, n + d))
  step (x,y,n) (L, d) = (Segment L d x y n, (x-d, y, n + d))

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
