{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError, ExceptT, runExceptT, catchError)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT, Reader, runReader)
import Control.Monad.State (MonadState, StateT, evalStateT, execStateT, state, modify, put)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)

import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (maximum, permutations, foldl')
import Data.List.Split (splitOn)
import Data.Proxy (Proxy(..))
import Data.STRef (STRef, readSTRef, writeSTRef, newSTRef)
import Data.Vector (MVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import Pair (Pair(..))

import Test.DocTest (doctest)

test :: IO ()
test = doctest ["2019/day/7/part1.hs"]

main :: IO ()
main = 
  print .
  maximum .
  flip map (permutations [0,1,2,3,4]) .
  runAmplifiers .
  readProgram =<<
  readFile "2019/day/7/input"

-- |
-- >>> runAmplifiers [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4,3,2,1,0]
-- 43210
-- >>> runAmplifiers [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4]
-- 54321
-- >>> runAmplifiers [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2]
-- 65210
runAmplifiers :: Program -> [Value] -> Value
runAmplifiers v = foldl' (runAmplifier v) 0

runAmplifier :: Program -> Value -> Value -> Value
runAmplifier v signal phase = runST $ do
  v <- Vector.thaw (Vector.fromList v)
  i <- newSTRef (Location 0)
  io <- newSTRef ProvidePhase{..}
  getInterpreter (runAPI day5part2) `runReaderT` InterpreterState v i io
  readSTRef io >>= \case
    OutputValue v -> return v
    io            -> error $ "Unexpected io halt state " ++ show io

newtype Interpreter s a = Interpreter
  { getInterpreter :: ReaderT (InterpreterState s) (ST s) a }
  deriving (Functor, Applicative, Monad)

data InterpreterState s = InterpreterState
  { programBuffer :: ProgramBuffer s
  , headRef   :: STRef s Location
  , programIORef  :: STRef s ProgramIO
  }

data ProgramIO
  = ProvidePhase { phase :: Value, signal :: Value }
  | ProvideSignal { signal :: Value }
  | ExpectOutput
  | OutputValue { given :: Value }
  deriving Show

instance MonadReader (ProgramBuffer s) (Interpreter s) where
  reader f = Interpreter . reader $ f . programBuffer
  local f (Interpreter ma) = Interpreter $ local (\s -> s { programBuffer = f (programBuffer s) }) ma
    
instance PrimMonad (Interpreter s) where
  type PrimState (Interpreter s) = PrimState (ST s)
  primitive = Interpreter . primitive

instance MonadError ProgramError (Interpreter s) where
  throwError = error . show
  catchError = const

interpreterState :: (InterpreterState s -> STRef s a) -> (a -> (b,a)) -> Interpreter s b
interpreterState l f = Interpreter $ do
  r <- reader l
  (b,a) <- f <$> lift (readSTRef r)
  lift (writeSTRef r a)
  return b

instance MonadState Location (Interpreter s) where
  state = interpreterState headRef

instance MonadInput Value (Interpreter s) where
  input = interpreterState programIORef $ \case
    ProvidePhase p v  -> (p, ProvideSignal v)
    ProvideSignal v   -> (v, ExpectOutput)
    io                -> error $ "input requested for " ++ show io

instance MonadOutput Value (Interpreter s) where
  output v = interpreterState programIORef $ \case
    ExpectOutput  -> ((), OutputValue v)
    io            -> error $ "output requested for " ++ show io

type Program = [Value]

newtype Value = Value { getValue :: Int }
  deriving newtype (Show, Eq, Num, Ord)

-- |
-- >>> readProgram "1002,4,3,4,33"
-- [1002,4,3,4,33]
readProgram :: String -> Program
readProgram = map (Value . read) . splitOn ","

data ProgramError
  = OutOfBoundsRead { readLocation :: Location }
  | OutOfBoundsWrite { writeLocation :: Location, writeValue ::  Value }
  | HeadOverflow
  | IllegalOpcode { opcode :: Int, intcode :: Int }
  | FlagError { flagError :: FlagError, intcode :: Int }
  deriving Show

type FlagParser = ExceptT FlagError (Reader Int)
newtype FlagIndex = FlagIndex { getFlagIndex :: Int }
  deriving newtype (Show, Num)

data FlagError
  = IllegalFlag { flag :: Int, flagIndex :: FlagIndex }
  | ExcessFlags { flags :: Int, flagsIndex :: FlagIndex }
  deriving Show

data ParameterMode = Pointer | Immediate
  deriving Show

runFlagParser :: FlagParser a -> Int -> Either FlagError a
runFlagParser p v = runReader (runExceptT p) v

-- |
-- >>> runFlagParser (mode 0) 201
-- Right Immediate
-- >>> runFlagParser (mode 1) 201
-- Right Pointer
-- >>> runFlagParser (mode 2) 303
-- Left (IllegalFlag {flag = 3, flagIndex = 2})
-- >>> runFlagParser (mode 3) 301
-- Right Pointer
mode :: FlagIndex -> FlagParser ParameterMode
mode flagIndex = reader ((`rem` 10) . (`div` 10^getFlagIndex flagIndex)) >>= \case
  0 -> return Pointer
  1 -> return Immediate
  flag -> throwError IllegalFlag{..}

-- |
-- >>> runFlagParser (zero 0) 21
-- Left (ExcessFlags {flags = 21, flagsIndex = 0})
-- >>> runFlagParser (zero 1) 21
-- Left (ExcessFlags {flags = 2, flagsIndex = 1})
-- >>> runFlagParser (zero 2) 21
-- Right ()
zero :: FlagIndex -> FlagParser ()
zero flagsIndex = reader (`div` 10^getFlagIndex flagsIndex) >>= \case
  0 -> return ()
  flags -> throwError ExcessFlags{..}

data Instruction m = forall f. Instruction
  { flagParser  :: FlagParser (f ParameterMode)
  , action      :: Action f m
  }

data Action f m = Halt | Action (f ParameterMode -> m ())

type API m = IntMap (Instruction m)

type MonadAPI m =
  ( MonadError  ProgramError m
  , MonadState  Location m
  , MonadReader (ProgramBuffer (PrimState m)) m
  , PrimMonad m
  )

type ProgramBuffer s = MVector s Value

newtype Location = Location { getLocation :: Int }
  deriving newtype (Show, Eq, Enum)

runAPI :: MonadAPI m => API m -> m ()
runAPI m = stepAPI m >>= \case
  False -> return ()
  True  -> runAPI m

stepAPI :: MonadAPI m => API m -> m Bool
stepAPI m = do
  intcode <- getValue <$> shift
  let (flags, opcode) = intcode `quotRem` 100
  case IntMap.lookup opcode m of
    Nothing               -> throwError IllegalOpcode{..}
    Just Instruction{..}  -> case runFlagParser flagParser flags of
      Left flagError  -> throwError FlagError{..}
      Right modes     -> case action of 
        Halt      -> return False
        Action mu -> True <$ mu modes

shift :: MonadAPI m => m Value
shift = do
  i <- state (id &&& succ)
  fetch i `catchError` \_ -> throwError HeadOverflow

param :: MonadAPI m => ParameterMode -> m Value
param Pointer = fetch . Location . getValue =<< shift
param Immediate = shift

location :: MonadAPI m => m Location
location = Location . getValue <$> shift

fetch :: MonadAPI m => Location -> m Value
fetch i = do
  v <- ask
  when (getLocation i >= MVector.length v) $
    throwError $ OutOfBoundsRead i
  MVector.read v (getLocation i)

store :: MonadAPI m => Location -> Value -> m ()
store i a = do
  v <- ask
  when (getLocation i >= MVector.length v) $
    throwError $ OutOfBoundsWrite i a
  MVector.write v (getLocation i) a

binary :: MonadAPI m => (Value -> Value -> Value) -> Instruction m
binary op = Instruction
  { flagParser = (Pair <$> mode 0 <*> mode 1) <* zero 2
  , action = Action $ \(Pair lhs rhs) -> do
      a <- param lhs
      b <- param rhs
      i <- location
      store i (a `op` b)
  }

day1 :: MonadAPI m => API m
day1 = IntMap.fromList
  [ (1, binary (+))
  , (2, binary (*))
  , (99, Instruction
      { flagParser = Proxy <$ zero 0
      , action = Halt
      }
    )
  ]

class Monad m => MonadInput i m | m -> i where
  input :: m i

instance MonadInput i m => MonadInput i (ExceptT e m) where
  input = lift input

class Monad m => MonadOutput o m | m -> o where
  output :: o -> m ()

instance MonadOutput o m => MonadOutput o (ExceptT e m) where
  output = lift . output

type MonadAPI' m =
  ( MonadAPI m
  , MonadInput Value m
  , MonadOutput Value m
  )

day5part1 :: MonadAPI' m => API m
day5part1 = day1 `IntMap.union` IntMap.fromList
  [ (3, Instruction
      { flagParser = Proxy <$ zero 0
      , action = Action $ \Proxy -> do
          a <- input
          i <- location
          store i a
      }
    )
  , (4, Instruction
      { flagParser = Identity <$> mode 0 <* zero 1
      , action = Action $ \(Identity val) -> output =<< param val
      }
    )
  ]

day5part2 :: MonadAPI' m => API m
day5part2 = day5part1 `IntMap.union` IntMap.fromList
  [ (5, jumpIf (/= 0))
  , (6, jumpIf (== 0))
  , (7, binary (\a b -> if a < b then 1 else 0))
  , (8, binary (\a b -> if a == b then 1 else 0))
  ]

jumpIf :: MonadAPI m => (Value -> Bool) -> Instruction m
jumpIf p = Instruction
  { flagParser = do
      a <- mode 0
      b <- mode 1
      zero 2
      return $ Pair a b
  , action = Action $ \(Pair lhs rhs) -> do
      b <- p <$> param lhs
      i <- Location . getValue <$> param rhs
      when b $ put i
  }
