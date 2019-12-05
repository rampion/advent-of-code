module Main where

import Test.DocTest (doctest)
import Data.List.Split (splitOn)

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import Control.Monad.ST (ST, runST)

import PolySemy


test :: IO ()
test = doctest ["2019/day/5/part1.hs"]

main :: IO ()
main = do
  v <- readProgram <$> readFile "2019/day/5/input"
  print $ runDiagnostic v

-- |
-- >>> readProgram "1101,100,-1,4,0"
-- [1101,100,-1,4,0]
readProgram :: String -> Program
readProgram = V.fromList . map read . splitOn ","

runProgram :: m ()
runProgram = forever $
  op >>= \case
    Add lhs rhs -> do
      a <- arg lhs
      b <- arg rhs
      c <- loc
      store (a + b) c
    Multiply lhs rhs -> do
      a <- arg lhs
      b <- arg rhs
      c <- loc
      store (a * b) c
    Input -> do
      a <- input
      b <- loc
      store a c
    Output val -> do
      a <- arg val
      output a
    Halt -> halt

op :: m Op
op = do
  n <- shift
  case n `quotRem` 100 of
    (flags, 1) -> do
        ((lhs, rhs)
    (param -> (param -> (0, rhs), lhs), 1) -> return $ Add lhs rhs
    (param -> (param -> (0, rhs), lhs), 2) -> return $ Multiply lhs rhs
    (0,                                 3) -> return $ Input
    (param -> (0, val),                 4) -> return $ Output val
    (0,                                99) -> fail $ "bad opcode " ++ show n

param 

loc :: m Loc
loc = 

arg :: Arg -> m Int

data Op where
  Add       :: Arg -> Arg -> Op
  Multiply  :: Arg -> Arg -> Op
  Input     ::               Op
  Output    :: Arg ->        Op
  Halt      ::               Op

newtype Loc = Loc { getLoc :: Int }
data Arg = Immediate | Pointer

op :: m Op


shift   :: m Int
store   :: Int -> m ()
input   :: m Int
output  :: Int -> m ()
halt    :: m ()


addP :: Parser Op
addP = binOpP 1 Add

binOpP :: Parser OpType

  flags <- opCodeP 1
  (flags, lhs) <- flagP flags
  (flags, rhs) <- flagP flags
  locModeP flags
  return (Bin con lhs rhs)

flagP :: Flags -> Parser (Flags, ArgType)
flagP flags = traverse modeP (flags `divMod` 10)


op :: 

makeSem ''Op

OpToST :: Member (Embed (ST s)) => Sem (




type Program = Vector Int
type Process s = MVector s Int

data EffectsF a where
  Input   :: EffectsF Int
  Output  :: Int -> EffectsF ()
  Halt    :: EffectsF ()

type Effects = Free EffectsF

runDiagnostic :: Program -> Either [Int] Int
runDiagnostic v = runProgram v $ \case
  Input f     -> return (f 1)
  Output i a  -> write i >> return a
  Halt        -> 


runProgram :: Monad m => (EffectsF a -> m a) -> Program -> m ()
runProgram f v = runST $ do
  v <- V.thaw v
  runProcess f v

runProcess :: Monad m => (EffectsF a -> m a) -> Process s -> ST s (m ())



readInst :: Int -> Op


solve :: Int -> Int -> Vector Int -> Int
solve noun verb v = runST $ do
  v <- V.thaw v
  M.write v 1 noun
  M.write v 2 verb
  runProgram v 0
  M.read v 0

runProgram :: PrimMonad m => MVector (PrimState m) Int -> Int -> m ()
runProgram v i = do
  let runOp op = do
        writePtr v (i+3) =<< op <$> readPtr v (i+1) <*> readPtr v (i+2)
        runProgram v (i+4)

  (opCode, parameterModes) <- readInst <$> M.read v i
  case opCode of
    1   -> runOp (+)
    2   -> runOp (*)
    3   -> get
    4   -> put
    99  -> return ()
    _   -> fail $ "Unrecognized opcode " ++ show opCode

writePtr :: PrimMonad m => MVector (PrimState m) Int -> Int -> Int -> m ()
writePtr v i a = (M.write v `flip` a) =<< M.read v i

readPtr :: PrimMonad m => MVector (PrimState m) Int -> Int -> m Int
readPtr v i = M.read v =<< M.read v i

