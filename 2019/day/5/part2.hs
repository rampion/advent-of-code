{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)

import Data.Maybe (isJust)
import Data.List.Split (splitOn)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector, MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Test.DocTest (doctest)

-- import Data.Vector.Mutable (MVector)
-- import qualified Data.Vector.Mutable as M

test :: IO ()
test = doctest ["2019/day/5/part2.hs"]

-- |
-- >>> runDiagnostic (ProgramInput 8) $ readProgram "3,9,8,9,10,9,4,9,99,-1,8"
-- Right 1
-- >>> runDiagnostic (ProgramInput 7) $ readProgram "3,9,8,9,10,9,4,9,99,-1,8"
-- Right 0
main :: IO ()
main = print . runDiagnostic (ProgramInput 5) . readProgram =<< readFile "2019/day/5/input"

type Program = Vector Value

-- |
-- >>> ProgramOutput 5
-- 5
newtype ProgramOutput = ProgramOutput { getProgramOutput :: Int }
  deriving newtype (Show, Eq)

newtype ProgramInput = ProgramInput { getProgramInput :: Int }
  deriving newtype (Show, Eq)

data ProgramError
  = OutOfBoundsRead { readLocation :: Location }
  | OutOfBoundsWrite { writeLocation :: Location, writeValue ::  Value }
  | HeadOverflow
  | IllegalOpcode { opcode :: Int, fullIntcode :: Int }
  | IllegalFlag { flag :: Int, digitROffset :: Int, fullIntcode ::  Int }
  | ExcessFlags { flags :: Int, digitROffset :: Int, fullIntcode ::  Int }
  deriving (Show, Eq)

-- |
-- >>> readProgram "1002,4,3,4,33"
-- [1002,4,3,4,33]
readProgram :: String -> Program
readProgram = V.fromList . map (Value . read) . splitOn ","

class Monad m => MonadInput i m | m -> i where
  input :: m i

class Monad m => MonadOutput o m | m -> o where
  output :: o -> m ()

type ProgramBuffer s = MVector s Value

type MonadProgram m =
  ( MonadInput  ProgramInput m
  , MonadOutput ProgramOutput m 
  , MonadError  ProgramError m
  , PrimMonad m
  , MonadState  Location m
  , MonadReader (ProgramBuffer (PrimState m)) m
  )

programLoop :: MonadProgram m => m ()
programLoop = intcode >>= \case
  Add lhs rhs -> do
    a <- param lhs
    b <- param rhs
    i <- location
    store i (a + b)
    programLoop
  Multiply lhs rhs -> do
    a <- param lhs
    b <- param rhs
    i <- location
    store i (a * b)
    programLoop
  Input -> do
    a <- Value . getProgramInput <$> input
    i <- location
    store i a
    programLoop
  Output val -> do
    a <- ProgramOutput . getValue <$> param val
    output a
    programLoop
  JumpIfTrue lhs rhs -> do
    b <- (/= 0) <$> param lhs
    i <- Location . getValue <$> param rhs
    when b $ put i
    programLoop
  JumpIfFalse lhs rhs -> do
    b <- (== 0) <$> param lhs
    i <- Location . getValue <$> param rhs
    when b $ put i
    programLoop
  LessThan lhs rhs -> do
    a <- param lhs
    b <- param rhs
    i <- location
    store i (if a < b then 1 else 0)
    programLoop
  Equals lhs rhs -> do
    a <- param lhs
    b <- param rhs
    i <- location
    store i (if a == b then 1 else 0)
    programLoop
  Halt -> return ()

intcode :: MonadProgram m => m IntCode
intcode = do
  n <- getValue <$> shift
  let zero j = case n `div` 10^j of
        0 -> return ()
        x -> throwError $ ExcessFlags
                          { flags = x
                          , digitROffset = j
                          , fullIntcode = n
                          }

      mode j = case (n `div` 10^j) `mod` 10 of
        0 -> return Pointer
        1 -> return Immediate
        x -> throwError $ IllegalFlag
                          { flag = x
                          , digitROffset = j
                          , fullIntcode = n
                          }
      bin c = do
        lhs <- mode 2
        rhs <- mode 3
        zero 4
        return $ c lhs rhs

  case n `mod` 100 of
    1 -> bin Add
    2 -> bin Multiply
    3 -> do
      zero 2
      return Input
    4 -> do
      val <- mode 2
      zero 3
      return $ Output val
    5 -> bin JumpIfTrue
    6 -> bin JumpIfFalse
    7 -> bin LessThan
    8 -> bin Equals
    99 -> return Halt
    x -> throwError $ IllegalOpcode
                      { opcode = x
                      , fullIntcode = n
                      }

param :: MonadProgram m => ParameterMode -> m Value
param Pointer = fetch . Location . getValue =<< shift
param Immediate = shift

location :: MonadProgram m => m Location
location = Location . getValue <$> shift

shift :: MonadProgram m => m Value
shift = do
  i <- state (id &&& succ)
  fetch i `catchError` \_ -> throwError HeadOverflow

fetch :: MonadProgram m => Location -> m Value
fetch i = do
  v <- ask
  when (getLocation i >= M.length v) $
    throwError $ OutOfBoundsRead i
  M.read v (getLocation i)

store :: MonadProgram m => Location -> Value -> m ()
store i a = do
  v <- ask
  when (getLocation i >= M.length v) $
    throwError $ OutOfBoundsWrite i a
  M.write v (getLocation i) a

data ParameterMode = Pointer | Immediate

newtype Value = Value { getValue :: Int }
  deriving newtype (Show, Eq, Num, Ord)

newtype Location = Location { getLocation :: Int }
  deriving newtype (Show, Eq, Enum)

data IntCode where
  Add         :: ParameterMode -> ParameterMode -> IntCode
  Multiply    :: ParameterMode -> ParameterMode -> IntCode
  Input       ::                                   IntCode
  Output      :: ParameterMode ->                  IntCode
  JumpIfTrue  :: ParameterMode -> ParameterMode -> IntCode
  JumpIfFalse :: ParameterMode -> ParameterMode -> IntCode
  LessThan    :: ParameterMode -> ParameterMode -> IntCode
  Equals      :: ParameterMode -> ParameterMode -> IntCode
  Halt        ::                                   IntCode

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mu = mb >>= \case
  True  -> mu
  False -> return ()

data DiagnosticError
  = NoOutput
  | OutputAfterNonzero ProgramOutput
  | MoreThanOneInputRequest
  | ProgramError ProgramError
  deriving Show

data DiagnosticException = DiagnosticException
  { diagnosticError :: DiagnosticError
  , availableInput  :: Maybe ProgramInput
  , numOutputs      :: Int
  , nonZeroOutput   :: Maybe ProgramOutput
  , coreContents    :: Program
  , headPosition    :: Location
  }
  deriving Show

data DiagnosticState s = DiagnosticState
  { availableInputRef  :: STRef s (Maybe ProgramInput)
  , numOutputsRef      :: STRef s Int
  , nonZeroOutputRef   :: STRef s (Maybe ProgramOutput)
  , programBuffer      :: ProgramBuffer s
  , headPositionRef    :: STRef s Location
  }

newtype DiagnosticCode = DiagnosticCode { getDiagnosticCode :: Int }
  deriving newtype (Show, Eq)

type DiagnosticImpl s a = ExceptT DiagnosticException (ReaderT (DiagnosticState s) (ST s)) a

throwDiagnosticError :: DiagnosticError -> DiagnosticImpl s a
throwDiagnosticError err = do
  DiagnosticState{..} <- ask
  throwError =<< DiagnosticException err 
    <$> readDiagnosticRef availableInputRef
    <*> readDiagnosticRef numOutputsRef
    <*> readDiagnosticRef nonZeroOutputRef
    <*> V.freeze programBuffer
    <*> readDiagnosticRef headPositionRef

readDiagnosticRef :: STRef s a -> DiagnosticImpl s a
readDiagnosticRef = lift . lift . readSTRef

writeDiagnosticRef :: STRef s a -> a -> DiagnosticImpl s ()
writeDiagnosticRef r = lift . lift . writeSTRef r

newtype Diagnostic s a = Diagnostic { getDiagnostic :: DiagnosticImpl s a }
  deriving (Functor, Applicative, Monad)

instance PrimMonad (Diagnostic s) where
  type PrimState (Diagnostic s) = PrimState (ST s)
  primitive = Diagnostic . primitive

instance MonadReader (ProgramBuffer s) (Diagnostic s) where
  ask = Diagnostic $ reader programBuffer
  local f = Diagnostic . (local $ \s -> s { programBuffer = f (programBuffer s) }) . getDiagnostic

instance MonadState Location (Diagnostic s) where
  state f = Diagnostic $ do
    r <- reader headPositionRef
    (a, i) <- f <$> readDiagnosticRef r
    writeDiagnosticRef r i
    return a

instance MonadError ProgramError (Diagnostic m) where
  throwError = Diagnostic . throwDiagnosticError . ProgramError
  catchError (Diagnostic ma) f = Diagnostic . catchError ma $ \case
    DiagnosticException{ diagnosticError=ProgramError err } -> getDiagnostic $ f err
    err              -> throwError err

instance MonadInput ProgramInput (Diagnostic m) where
  input = Diagnostic $ do
    r <- reader availableInputRef
    readDiagnosticRef r >>= \case
      Nothing -> throwDiagnosticError MoreThanOneInputRequest
      Just a  -> do
        writeDiagnosticRef r Nothing
        return a

instance MonadOutput ProgramOutput (Diagnostic m) where
  output o = Diagnostic $ do
    r <- reader nonZeroOutputRef
    whenM (isJust <$> readDiagnosticRef r) $
      throwDiagnosticError $ OutputAfterNonzero o
    when (0 /= getProgramOutput o) $ 
      writeDiagnosticRef r (Just o)

    r <- reader numOutputsRef
    writeDiagnosticRef r . succ =<< readDiagnosticRef r

runDiagnostic :: ProgramInput -> Program -> Either DiagnosticException DiagnosticCode
runDiagnostic a v = runST $ do
  s <- DiagnosticState
        <$> newSTRef (Just a)
        <*> newSTRef 0
        <*> newSTRef Nothing
        <*> V.thaw v
        <*> newSTRef (Location 0)
  flip runReaderT s . runExceptT $ do
    getDiagnostic programLoop
    reader numOutputsRef >>= readDiagnosticRef >>= \case
      0 -> throwDiagnosticError NoOutput
      _ -> DiagnosticCode . maybe 0 getProgramOutput <$> (readDiagnosticRef =<< reader nonZeroOutputRef)
