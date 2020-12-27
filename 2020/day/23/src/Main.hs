{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wall -Werror -Wextra -Wno-name-shadowing #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (foldl', (\\))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector hiding (length)
import Data.Vector.Unboxed.Mutable (slice, unsafeMove, unsafeRead, unsafeWrite)
import qualified Data.Vector.Unboxed.Mutable as Vector
import Data.Word (Word64)
import Debug.Trace
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main =
  (drop 1 <$> getArgs) >>= traverse_ \filename -> do
    putStrLn filename

    cups <- readInput filename

    putStrLn "\tpart 1: labels on the cups after cup 1 after 100 steps"
    let actual1 = part1 cups
    expected1 <- readExpected (filename ++ ".part1")
    putStrLn $ "\t" ++ check actual1 expected1 ++ " " ++ show actual1
    hFlush stdout

    putStrLn "\tpart 2: product of the labels on the two cups after cup 1 after ten million steps with one million cups"
    let actual2 = part2 cups
    expected2 <- readExpected (filename ++ ".part2")
    putStrLn $ "\t" ++ check actual2 expected2 ++ " " ++ show actual2

--------------------------------------------------------------------------------

part2 :: [Cup] -> Word64
part2 = answer . Vector.toList . run 10_000_000 . (++ [10 .. 1_000_000])
  where
    answer xs =
      let (ys, _ : zs) = break (== 1) xs
       in product . take 2 $ zs ++ ys

run :: Int -> [Cup] -> Vector Cup
run moves cups = Vector.create do
  v <- Vector.thaw (Vector.fromList cups)
  let n = Vector.length v
  let n' = fromIntegral n
  let vfind z j mh =
        unsafeRead v j <&> (== z) >>= \case
          True -> return (Just j)
          False -> mh

  let x ⊕ y = (x + y) `mod` n
  let moveDown w dst =
        -- src = dst⊕3
        -- v[dst⊕(-1)] = a
        -- v[src⊕(w-1)] = z, a ≠ z
        --
        -- write forwards, since dst⊕{0,1,2} are buffered
        --
        -- a) 0 ≤ dst < dst⊕w ≤ src < src⊕w ≤ n ; 0 < w ≤ 3
        --    unsafeMove (slice dst w v) (slice src w v)
        --
        -- b) 0 ≤ dst < src < dst⊕w < src⊕w ≤ n ; 3 < w
        --    unsafeMove (slice dst w v) (slice src w v)
        --
        -- c) 0 < src⊕w < dst < dst⊕w ≤ src < n ; 0 < n-src < w ≤ 3
        --    unsafeMove (slice dst (n-src) v) (slice src (n-src) v)
        --    (e) dst=n-3, src=0, w=w-(n-src)
        --
        -- d) 0 < src⊕w < dst < src < dst⊕w ≤ n ; 3 < w, 0 < n-src < w
        --    unsafeMove (slice dst (n-src) v) (slice src (n-src) v)
        --    (e) dst=n-3, src=0, w=w-(n-src)
        --
        -- e) 0 ≤ src < src⊕w < dst < dst⊕w ≤ n ; 0 < w ≤ 3
        --    unsafeMove (slice dst w v) (slice src w v)
        --
        -- f) 0 < dst⊕w < src⊕w < dst < src < n ; 3 < w
        --    unsafeMove (slice dst (n-src) v) (slice src (n-src) v)
        --    (h) dst=n-3, src=0, w=w-(n-src)
        --
        -- g) 0 < dst⊕w ≤ src < src⊕w < dst < n ; 0 < n-dst < w ≤ 3
        --    unsafeMove (slice dst (n-dst) v) (slice src (n-dst)
        --    (a) dst=0, src=3, w=w-(n-dst)
        --
        -- h) 0 ≤ src < dst⊕w < src⊕w < dst < n ; 0 < n-dst ≤ 3 < w
        --    unsafeMove (slice dst (n-dst) v) (slice src (n-dst)
        --    (a,b) dst=0, src=3, w=w-(n-dst)
        --
        --  fdc   unsafeMove (slice dst (n-src) v) (slice src (n-src) v)
        --  ↓↳↓
        -- gh ↓   unsafeMove (slice dst (n-dst) v) (slice src (n-dst)
        -- ↓↓ ↓
        -- ab e   unsafeMove (slice dst w v) (slice src w v)
        case dst ⊕ 3 of
          src | n < src + w -> do
            let w0 = n - src
            let w' = w - w0
            unsafeMove (slice dst w0 v) (slice src w0 v)
            if w' <= 3
              then do
                unsafeMove (slice (n - 3) w' v) (slice 0 w' v)
              else do
                let w'' = w' - 3
                unsafeMove (slice (n - 3) 3 v) (slice 0 3 v)
                unsafeMove (slice 0 w'' v) (slice 3 w'' v)
          src | n < dst + w -> do
            let w1 = n - dst
            let w'' = w - w1
            unsafeMove (slice dst w1 v) (slice src w1 v)
            unsafeMove (slice 0 w'' v) (slice 3 w'' v)
          src -> do
            unsafeMove (slice dst w v) (slice src w v)

  let moveUp w src =
        -- dst = src⊕3
        -- v[src@(w-1)] = a
        -- v[src⊕(-1)] = z, a ≠ z
        --
        -- write backwards, since src⊕w⊕{0,1,2} are buffered
        --
        -- a) 0 ≤ src < src⊕w ≤ dst < dst⊕w ≤ n ; 0 < w ≤ 3
        --    unsafeMove (slice dst w v) (slice src w v)
        --
        -- b) 0 ≤ src < dst < src⊕w < dst⊕w ≤ n ; 3 < w
        --    unsafeMove (slice dst w v) (slice src w v)
        --
        -- c) 0 < dst⊕w < src < src⊕w ≤ dst < n ; 0 < n-dst < w ≤ 3
        --    unsafeMove (slice 0 (dst⊕w) v) (slice (src⊕w-dst⊕w) (dst⊕w) v)
        --    (a) w=w-(dst⊕w)
        --
        -- d) 0 < dst⊕w < src < dst < src⊕w ≤ n ; 3 < w, 0 < n-dst < w
        --    unsafeMove (slice 0 (dst⊕w) v) (slice (src⊕w-dst⊕w) (dst⊕w) v)
        --    (a,b) w=w-(dst⊕w)
        --
        -- e) 0 ≤ dst < dst⊕w < src < src⊕w ≤ n ; 0 < w ≤ 3
        --    unsafeMove (slice dst w v) (slice src w v)
        --
        -- f) 0 < src⊕w < dst⊕w < src < dst < n ; 3 < w
        --    unsafeMove (slice 3 (src⊕w) v) (slice 0 (src⊕w) v)
        --    (c,d) w=w-(src⊕w)
        --
        -- g) 0 < src⊕w ≤ dst < dst⊕w < src < n ; 0 < n-src < w ≤ 3
        --    unsafeMove (slice 3 (src⊕w) v) (slice 0 (src⊕w) v)
        --    (e) w=w-(src⊕w)
        --
        -- h) 0 ≤ dst < src⊕w < dst⊕w < src < n ; 0 < n-src ≤ 3 < w
        --    unsafeMove (slice 3 (src⊕w) v) (slice 0 (src⊕w) v)
        --    (e) w=w-(src⊕w)
        --
        --    ghf  unsafeMove (slice 3 (src⊕w) v) (slice 0 (src⊕w) v)
        --    ↓↲↓
        --    ↓ cd  unsafeMove (slice 0 (dst⊕w) v) (slice (src⊕w-dst⊕w) (dst⊕w) v)
        --    ↓ ↓
        --    e ab  unsafeMove (slice dst w v) (slice src w v)
        case src ⊕ 3 of
          dst | n < src + w -> do
            let w0 = src ⊕ w
            let w' = w - w0 -- => src + w' == n
            unsafeMove (slice 3 w0 v) (slice 0 w0 v)
            if w' <= 3 -- <=> dst < 3
              then do
                unsafeMove (slice dst w' v) (slice src w' v)
              else do
                let w'' = w' - 3 -- => dst + w'' == n
                unsafeMove (slice 0 3 v) (slice (n - 3) 3 v)
                unsafeMove (slice dst w'' v) (slice src w'' v)
          dst | n < dst + w -> do
            let w1 = dst ⊕ w
            let w'' = w - w1
            unsafeMove (slice 0 w1 v) (slice (src + w'') w1 v)
            unsafeMove (slice dst w'' v) (slice src w'' v)
          dst -> do
            unsafeMove (slice dst w v) (slice src w v)

  p <- newSTRef 0
  forM_ [1 .. moves] \t -> do
    when (t `rem` 10_000 == 0) do
      traceShow t $ return ()
    i <- readSTRef p

    let j = i ⊕ 1
    let k = i ⊕ 2
    let l = i ⊕ 3

    a <- unsafeRead v i

    -- TODO: copy slice
    b <- unsafeRead v j
    c <- unsafeRead v k
    d <- unsafeRead v l

    let z = head . (\\ [b, c, d]) $ tail [a, a -1 .. 1] ++ [n', n' -1 .. a + 1]
    {-
    v' <- Vector.freeze v
    traceShow (a, i, [b, c, d], tail [a, a -1 .. 1], [n', n' -1 .. a + 1], z, v') $ return ()
    -}
    Just h <- foldr (vfind z) (return Nothing) [0 .. n -1]

    let widthDown = h ⊕ (- l)
    let widthUp = i ⊕ (- h)

    offset <-
      if widthDown < widthUp
        then do
          moveDown widthDown j
          writeSTRef p (i ⊕ 1)
          return widthDown
        else do
          moveUp widthUp (h ⊕ 1)
          writeSTRef p (i ⊕ 4)
          return (- widthUp)

    -- TODO: write slice
    unsafeWrite v (j ⊕ offset) b
    unsafeWrite v (k ⊕ offset) c
    unsafeWrite v (l ⊕ offset) d

  return v

--------------------------------------------------------------------------------

part1 :: [Cup] -> Word64
part1 = answer . Vector.toList . run 100
  where
    answer xs =
      let (ys, _ : zs) = break (== 1) xs
       in foldl' (\n d -> 10 * n + d) 0 (zs ++ ys)

{-
part1 :: [Int] -> Int
part1 = answer . (!! 100) . iterate (move 9)

move :: Cup -> [Cup] -> [Cup]
move p ~(n : a : b : c : xs) =
  let m = head $ ([n -1, n -2 .. 1] ++ [p, p -1 .. n + 1]) \\ [a, b, c]
      (ys, _ : zs) = break (== m) xs
   in ys ++ m : a : b : c : zs ++ [n]
-}

--------------------------------------------------------------------------------

readInput :: FilePath -> IO [Cup]
readInput filename = map (read . return) . head . words <$> readFile filename

readExpected :: Read a => FilePath -> IO (Maybe a)
readExpected filename =
  (Just . read . head . words <$> readFile filename) <|> return Nothing

check :: Eq a => a -> Maybe a -> String
check actual = \case
  Just expected | actual == expected -> ansi green "✓"
  Nothing -> ansi yellow "?"
  _ -> ansi red "✗"
  where
    ansi :: Int -> String -> String
    ansi color text = "\x1b[" ++ show color ++ "m" ++ text ++ "\x1b[m"

    green, yellow, red :: Int
    green = 32
    yellow = 33
    red = 31

--------------------------------------------------------------------------------

type Cup = Word64
