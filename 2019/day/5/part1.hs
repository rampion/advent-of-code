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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State (MonadState(..), StateT(..), evalStateT, gets, modify)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)

import Data.Maybe (isJust)
import Data.List.Split (splitOn)
import Data.Vector (Vector, MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Test.DocTest (doctest)

-- import Data.Vector.Mutable (MVector)
-- import qualified Data.Vector.Mutable as M

test :: IO ()
test = doctest ["2019/day/5/part1.hs"]

main :: IO ()
main = print . diagnostic . readProgram =<< readFile "2019/day/5/input"

type Program = Vector Value

-- |
-- >>> ProgramOutput 5
-- 5
newtype ProgramOutput = ProgramOutput { getProgramOutput :: Int }
  deriving newtype (Show, Eq)

newtype ProgramInput = ProgramInput { getProgramInput :: Int }
  deriving newtype (Show, Eq)

data ProgramException = ProgramException
  { headLocation  :: Location
  , memory        :: Program
  , programError  :: ProgramError
  }
  deriving (Show, Eq)

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

instance MonadInput i m => MonadInput i (StateT s m) where
  input = lift input

instance MonadInput i m => MonadInput i (ReaderT e m) where
  input = lift input

class Monad m => MonadOutput o m | m -> o where
  output :: o -> m ()

instance MonadOutput i m => MonadOutput i (StateT s m) where
  output = lift . output

instance MonadOutput i m => MonadOutput i (ReaderT e m) where
  output = lift . output

runProgram ::
  ( MonadInput  ProgramInput m
  , MonadOutput ProgramOutput m 
  , MonadError  ProgramException m
  , PrimMonad m
  ) => Program -> m ()
runProgram = runReaderT (evalStateT programLoop (Location 0)) <=< V.thaw

type Memory s = MVector s Value

type MonadProgram m =
  ( MonadInput  ProgramInput m
  , MonadOutput ProgramOutput m 
  , MonadError  ProgramException m
  , PrimMonad m
  , MonadState  Location m
  , MonadReader (Memory (PrimState m)) m
  )

throwProgramError :: MonadProgram m => ProgramError -> m a
throwProgramError err = do
  i <- get
  v <- V.freeze =<< ask
  throwError $ ProgramException { headLocation=i, memory=v, programError=err }

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
  Halt -> return ()

intcode :: MonadProgram m => m IntCode
intcode = do
  n <- getValue <$> shift
  let zero j = case n `div` 10^j of
        0 -> return ()
        x -> throwProgramError $ ExcessFlags
                                  { flags = x
                                  , digitROffset = j
                                  , fullIntcode = n
                                  }

      mode j = case (n `div` 10^j) `mod` 10 of
        0 -> return Pointer
        1 -> return Immediate
        x -> throwProgramError $ IllegalFlag
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
    99 -> return Halt
    x -> throwProgramError $ IllegalOpcode
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
  i <- get
  modify succ
  fetch i `catchError` \_ -> throwProgramError HeadOverflow

fetch :: MonadProgram m => Location -> m Value
fetch i = do
  v <- ask
  when (getLocation i >= M.length v) $
    throwProgramError $ OutOfBoundsRead i
  M.read v (getLocation i)

store :: MonadProgram m => Location -> Value -> m ()
store i a = do
  v <- ask
  when (getLocation i >= M.length v) $
    throwProgramError $ OutOfBoundsWrite i a
  M.write v (getLocation i) a

data ParameterMode = Pointer | Immediate

newtype Value = Value { getValue :: Int }
  deriving newtype (Show, Eq, Num)

newtype Location = Location { getLocation :: Int }
  deriving newtype (Show, Eq, Enum)

data IntCode where
  Add       :: ParameterMode -> ParameterMode -> IntCode
  Multiply  :: ParameterMode -> ParameterMode -> IntCode
  Input     ::                                   IntCode
  Output    :: ParameterMode ->                  IntCode
  Halt      ::                                   IntCode

data DiagnosticError
  = OutputAfterNonzero ProgramOutput
  | MoreThanOneInputRequest
  | FromProgram ProgramException
  deriving Show

data DiagnosticState = DiagnosticState
  { inputRequested :: Bool
  , numOutputs :: Int
  , nonZeroOutput :: Maybe ProgramOutput
  }
  deriving Show

initialDiagnosticState :: DiagnosticState
initialDiagnosticState = DiagnosticState
  { inputRequested=False
  , numOutputs=0
  , nonZeroOutput=Nothing
  }

newtype DiagnosticCode = DiagnosticCode { getDiagnosticCode :: Int }
  deriving newtype (Show, Eq)

newtype Diagnostic s a = Diagnostic { getDiagnostic :: ExceptT DiagnosticError (StateT DiagnosticState (ST s)) a }
  deriving (Functor, Applicative, Monad)

instance PrimMonad (Diagnostic s) where
  type PrimState (Diagnostic s) = PrimState (ST s)
  primitive = Diagnostic . primitive

instance MonadError ProgramException (Diagnostic s) where
  throwError = Diagnostic . throwError . FromProgram
  catchError (Diagnostic ma) f = Diagnostic . catchError ma $ \case
    FromProgram err  -> getDiagnostic $ f err
    err              -> throwError err

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mu = mb >>= \case
  True  -> mu
  False -> return ()

instance MonadInput ProgramInput (Diagnostic s) where
  input = Diagnostic $ do
    whenM (gets inputRequested) $
      throwError MoreThanOneInputRequest
    modify $ \s -> s { inputRequested = True }
    return $ ProgramInput 1

instance MonadOutput ProgramOutput (Diagnostic s) where
  output o = Diagnostic $ do
    whenM (gets $ isJust . nonZeroOutput) $
      throwError $ OutputAfterNonzero o
    modify $ \s -> s 
      { nonZeroOutput = case o of
          ProgramOutput 0 -> Nothing
          _               -> Just o
      , numOutputs = numOutputs s + 1
      }

runDiagnostic :: DiagnosticState -> (forall s. Diagnostic s a) -> (Either DiagnosticError a, DiagnosticState)
runDiagnostic initial d = runST $ flip runStateT initial $ runExceptT $ getDiagnostic d

diagnostic :: Program -> Either (DiagnosticError, DiagnosticState) DiagnosticCode
diagnostic p = case runDiagnostic initialDiagnosticState (runProgram p) of
  (Left err, final) -> Left (err, final)
  (Right (), final) -> Right . DiagnosticCode . maybe 0 getProgramOutput $ nonZeroOutput final
