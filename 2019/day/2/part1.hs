-- :! cabal v1-exec -- ghc --make % -package split -package vector -package primitive && %:r < %:h/input
module Main where

import Data.List.Split (splitOn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad, PrimState)

main :: IO ()
main = print . solve . readInput =<< getContents

readInput :: String -> Vector Int
readInput = V.fromList . map read . splitOn ","

solve :: Vector Int -> Int
solve v = runST $ do
  v <- V.thaw v
  M.write v 1 12
  M.write v 2 2
  runProgram v 0
  M.read v 0

runProgram :: PrimMonad m => MVector (PrimState m) Int -> Int -> m ()
runProgram v i = do
  let runOp op = do
        writePtr v (i+3) =<< op <$> readPtr v (i+1) <*> readPtr v (i+2)
        runProgram v (i+4)

  opCode <- M.read v i
  case opCode of
    1   -> runOp (+)
    2   -> runOp (*)
    99  -> return ()
    _   -> fail $ "Unrecognized opcode " ++ show opCode

writePtr :: PrimMonad m => MVector (PrimState m) Int -> Int -> Int -> m ()
writePtr v i a = (M.write v `flip` a) =<< M.read v i

readPtr :: PrimMonad m => MVector (PrimState m) Int -> Int -> m Int
readPtr v i = M.read v =<< M.read v i

