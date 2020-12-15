{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Category ((>>>))
import Control.Monad (when)
import Data.Bits
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    program <- parseProgram <$> readFile filename

    putStrLn $ "\tpart 1: sum of values in memory after running the program"
    putStrLn $ "\t" ++ show (part1 program)

    when (filename /= "data/example") do
      putStrLn $ "\tpart 1: sum of values in memory after running the program with v2"
      putStrLn $ "\t" ++ show (part2 program)

--------------------------------------------------------------------------------

parseProgram :: String -> Program
parseProgram = map parseLine . lines

parseLine :: String -> Either Mask Write
parseLine (splitAt 4 -> ("mem[", span isDigit -> (addr, splitAt 4 -> ("] = ", value)))) = Right (read addr, read value)
parseLine (splitAt 7 -> ("mask = ", mask)) =
  Left $
    mask <&> \case
      'X' -> Nothing
      '1' -> Just True
      '0' -> Just False
      ch -> error $ "illegal mask character: " ++ show ch
parseLine line = error $ "unable to parse line " ++ show line

--------------------------------------------------------------------------------

part1 :: Program -> Value
part1 = sum . fst . foldl' (flip $ either changeMask doWrite) (Map.empty, id)
  where
    changeMask :: Mask -> (IntMap Value, Value -> Value) -> (IntMap Value, Value -> Value)
    changeMask mask (m, _) = (m, set mask . clear mask)

    doWrite :: Write -> (IntMap Value, Value -> Value) -> (IntMap Value, Value -> Value)
    doWrite (addr, value) (m, mask) = (Map.insert addr (mask value) m, mask)

    set, clear :: Mask -> Int -> Int
    set mask = (.|.) . pack $ fromMaybe False <$> mask
    clear mask = (.&.) . pack $ fromMaybe True <$> mask

    pack :: [Bool] -> Int
    pack = foldl' (\n b -> 2 * n + if b then 1 else 0) 0

--------------------------------------------------------------------------------

part2 :: Program -> Value
part2 = sum . fst . foldl' (flip $ either changeMask doWrite) (Map.empty, initialMask)
  where
    initialMask :: Mask
    initialMask = replicate 36 (Just False)

    changeMask :: Mask -> (IntMap Value, Mask) -> (IntMap Value, Mask)
    changeMask mask (mem, _) = (mem, mask)

    doWrite :: Write -> (IntMap Value, Mask) -> (IntMap Value, Mask)
    doWrite (addr, value) (!mem, mask) =
      (foldl' (\mem addr -> Map.insert addr value mem) mem (range addr mask), mask)

    range :: Address -> Mask -> [Address]
    range addr = foldl' modify [addr] . zip [35, 34 .. 0]

    modify :: [Address] -> (Int, Maybe Bool) -> [Address]
    modify addrs (ix, bit) = case bit of
      Just False -> addrs
      Just True -> (`setBit` ix) <$> addrs
      Nothing -> do
        addr <- addrs
        [addr `clearBit` ix, addr `setBit` ix]

{-
part2 :: Program -> Value
part2 = snd . foldl' combine ([], 0) . snd . foldl' (flip $ either changeMask expand) (initialMask, [])
  where
    initialMask :: Mask
    initialMask = replicate 36 (Just 0)

    changeMask :: Mask -> (Mask, [(Mask, Value)]) -> (Mask, [(Mask, Value)])
    changeMask mask (_, writes) = (mask, writes)

    expand :: Write -> (Mask, [(Mask, Value)]) -> (Mask, [(Mask, Value)])
    expand (addr, value) (mask, writes) = (mask, (range addr mask, value) : writes)

    range :: Address -> Mask -> Mask
    range addr = zipWith (\ix -> fmap (|| testBit addr ix)) [35, 34 .. 0]

    combine :: ([Mask], Value) -> (Mask, Value) -> ([Mask], Value)
    combine (ranges, !total) (flip (foldl' remove) -> range, value) = (range : ranges, total + value * size range)

    remove :: Mask -> Mask -> Mask
    remove

    1X0X
   -1000
    1100,1001
-}

--------------------------------------------------------------------------------

type Program = [Either Mask Write]

type Mask = [Maybe Bool]

type Write = (Address, Value)

type Address = Int

type Value = Int
