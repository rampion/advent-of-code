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
    [ 30 @<-? findMinIntersectionSteps "2019/day/3/example1"
    , 610 @<-? findMinIntersectionSteps "2019/day/3/example2"
    , 410 @<-? findMinIntersectionSteps "2019/day/3/example3"
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

data Intersection = Point Int Int Int | Span Segment
  deriving (Eq, Show)

getMinSteps :: Intersection -> Int
getMinSteps (Point x y n) = n
getMinSteps (Span Segment{..}) = numSteps

removeOrigin :: Intersection -> [Intersection]
removeOrigin (Point 0 0 _) = []
removeOrigin (Span (Segment o lo hi 0 n d)) 
  | lo <= 0 && 0 <= hi = [ Span (Segment o lo (-1) 0) | lo < 0 ] ++
                         [ Span (Segment o 1    hi 0) | hi > 0 ]
removeOrigin p = [p]

minmax :: Ord a => a -> a -> (a,a)
minmax a b = if a < b then (a,b) else (b,a)

minX :: Segment -> Int
minX Segment{..} = case direction
  L -> xCoord - delta
  _ -> xCoord

maxX :: Segment -> Int
maxX Segment{..} = case direction
  R -> xCoord + delta
  _ -> xCoord

minY :: Segment -> Int
minY Segment{..} = case direction
  D -> yCoord - delta
  _ -> yCoord

maxY :: Segment -> Int
maxY Segment{..} = case direction
  U -> yCoord + delta
  _ -> yCoord

findIntersections :: [Segment] -> [Segment] -> [Intersection]
findIntersections as bs = do
  a <- as
  b <- bs
  let loX = minX a `max` minX b
      hiX = maxX a `min` maxX b
      loY = minY a `max` minY b
      hiY = maxY a `min` maxY b
  guard $ loX <= hiX && loY <= hiY



  case (direction a, direction b) of
    (U, U) -> do
      let lo = yCoord a `max` yCoord b
          hi = yCoord' a `min` yCoord' b
      guard $ xCoord a == xCoord b && lo < hi
      return $ Point (xCoord a) lo (numSteps a + lo - yCoord a
                                  + numSteps b + lo - yCoord b)
    (D, D) -> do
      let lo = yCoord' a `max` yCoord' b
          hi = yCoord a `min` yCoord b
      guard $ xCoord a == xCoord b && lo < hi
      return $ Point (xCoord a) hi (numSteps a + yCoord a - hi
                                  + numSteps b + yCoord b - hi)

          






  let (minA, maxA) = start a `minmax` stop a
      (minB, maxB) = start b `minmax` stop b
      conA = constant a
      conB = constant b
      lo = minA `max` minB
      hi = maxA `min` maxB
  if orientation a == orientation b
    then do
      guard $ constant a == constant b
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
      guard $ minA <= conB && conB <= maxA
           && minB <= constant a && constant a <= maxB
      return $ case orientation a of
        Vertical    -> Point conA conB
        Horizontal  -> Point conB conA

type Coord = (Int, Int)

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
