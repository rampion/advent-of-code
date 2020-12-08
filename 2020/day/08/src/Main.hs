{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Control.Category ((>>>))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.State (MonadState (..), gets, runState)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Environment (getArgs)

main :: IO ()
main = (getArgs >>=) . traverse_ $ \filename -> do
  putStrLn filename

  bootcode <- parseBootCode <$> readFile filename

  putStrLn $ "\tpart 1: value of accelerator when code loops for the first time"
  putStrLn $ "\t" ++ show (part1 bootcode)

  putStrLn $ "\tpart 2: value of accelerator when patched code exits cleanly"
  putStrLn $ "\t" ++ show (part2 bootcode)

data Runner = Runner
  { ix :: Int,
    acc :: Int,
    visited :: IntSet
  }
  deriving (Show)

data ExitCode = ExitedNormally | Looped

simulation :: (Monad m, MonadState Runner m, MonadReader BootCode m) => m ExitCode
simulation = untilM (exitCode <$> get <*> ask) $ do
  Runner ix acc visited <- gets visit
  (ix, acc) <-
    reader (Vector.! ix) <&> \case
      (Acc, arg) -> (ix + 1, acc + arg)
      (Jmp, arg) -> (ix + arg, acc)
      (Nop, _) -> (ix + 1, acc)
  put $ Runner ix acc visited

untilM :: Monad m => m (Maybe a) -> m () -> m a
untilM check step =
  check >>= \case
    Just a -> return a
    Nothing -> step >> untilM check step

visit :: Runner -> Runner
visit r = r {visited = Set.insert (ix r) (visited r)}

exitCode :: Runner -> BootCode -> Maybe ExitCode
exitCode (Runner ix _ visited) bootcode
  | ix `Set.member` visited = Just Looped
  | ix >= Vector.length bootcode = Just ExitedNormally
  | otherwise = Nothing

runSimulation :: BootCode -> (ExitCode, Runner)
runSimulation bootcode = (simulation `runReaderT` bootcode) `runState` Runner 0 0 Set.empty

edits :: BootCode -> [BootCode]
edits bootcode = do
  ix <- [0 .. Vector.length bootcode - 1]
  val <- case bootcode Vector.! ix of
    (Acc, _) -> []
    (Jmp, arg) -> return (Nop, arg)
    (Nop, arg) -> return (Jmp, arg)
  return $ bootcode Vector.// [(ix, val)]

part1 :: BootCode -> Int
part1 bootcode = case runSimulation bootcode of
  (Looped, Runner _ acc _) -> acc
  (ExitedNormally, runner) -> error $ "expected loop, but runner exited normally: " ++ show runner

part2 :: BootCode -> Int
part2 bootcode = case [r | (ExitedNormally, r) <- runSimulation <$> edits bootcode] of
  [Runner _ acc _] -> acc
  runners -> error $ "expected exactly one runner to exit normally, but got: " ++ show runners

type BootCode = Vector Instruction

type Instruction = (Operation, Argument)

data Operation = Acc | Jmp | Nop
  deriving (Show)

type Argument = Int

parseBootCode :: String -> BootCode
parseBootCode = Vector.fromList . map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction =
  words >>> \case
    [op, arg] -> (parseOperation op, parseArgument arg)
    ws -> error $ "invalid instruction: " ++ show ws

parseOperation :: String -> Operation
parseOperation "acc" = Acc
parseOperation "jmp" = Jmp
parseOperation "nop" = Nop
parseOperation inv = error $ "invalid operation: " ++ show inv

parseArgument :: String -> Argument
parseArgument ('+' : i) = read i
parseArgument ('-' : i) = - read i
parseArgument arg = error $ "invalid argument: " ++ show arg
