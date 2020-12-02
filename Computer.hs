{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Computer where

import Control.Applicative (liftA2)
import Control.Arrow ((***),(&&&))
import Control.Monad (when)
import Control.Monad.State (MonadState, StateT, runStateT, state, get, put)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Trans (lift)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

interpret :: TuringMachine m => API m -> m ()
interpret = \is -> do
    p <- get
    c <- shift
    b <- step (intcode c) is `catchError` \e -> 
            throwError $ e ++ " for intcode " ++ show c ++ " at " ++ show p
    if b
      then interpret is
      else return ()

  where

    intcode :: Value -> (OpCode, ParameterModes) 
    intcode = (OpCode *** ParameterModes) . swap . (`quotRem` 100) . getValue

    step :: MonadError String m => (OpCode, ParameterModes) -> API m -> m Bool
    step (x,m) is = case Map.lookup x is of 
      Nothing -> throwError $ "Unrecognized opcode " ++ show x
      Just i  -> runStateT i m >>= \case
        (b, 0) -> return b
        (_, m) -> throwError $ "Unused parameter mode flags " ++ show m

class (MonadState Location m, MonadError String m) => TuringMachine m where
  fetch :: Location -> m Value
  store :: Location -> Value -> m ()

shift :: TuringMachine m => m Value
shift = fetch =<< state (id &&& succ)

newtype Value = Value { getValue :: Int }
  deriving newtype (Show, Num, Eq, Ord)

newtype Location = Location { getLocation :: Int }
  deriving newtype (Show, Enum)

type Instruction = StateT ParameterModes
type API m = Map OpCode (Instruction m Bool)

newtype OpCode = OpCode { getOpCode :: Int }
  deriving newtype (Show, Num, Ord, Eq)

newtype ParameterModes = ParameterModes { getParameterModes :: Int }
  deriving newtype (Show, Num, Eq, Integral, Real, Enum, Ord)

markI :: TuringMachine m => API m
markI = Map.fromList
  [ (OpCode 0,   True <$ (store' =<< liftA2 (+) parameter parameter))
  , (OpCode 1,   True <$ (store' =<< liftA2 (*) parameter parameter))
  , (OpCode 99,  return False                                      )
  ]

parameter :: TuringMachine m => Instruction m Value
parameter = state (swap . (`quotRem` 10)) >>= \case
  0 -> fetch'
  1 -> lift shift
  x -> fail $ "Illegal parameter mode " ++ show x

location :: TuringMachine m => Instruction m Location
location = Location . getValue <$> lift shift

store' :: TuringMachine m => Value -> Instruction m ()
store' v = lift . flip store v =<< location

fetch' :: TuringMachine m => Instruction m Value
fetch' = lift . fetch =<< location

markII :: TuringMachineIO m => API m
markII = markI `Map.union` Map.fromList
  [ (OpCode 3, True <$ (store' =<< lift input) )
  , (OpCode 4, True <$ (lift . output =<< fetch'))
  ]

class TuringMachine m => TuringMachineIO m where
  input :: m Value
  output :: Value -> m ()

markIII :: TuringMachineIO m => API m
markIII = markII `Map.union` Map.fromList
  [ (OpCode 5, True <$ jump (/= 0))
  , (OpCode 6, True <$ jump (== 0))
  , (OpCode 7, True <$ (store' . fromBool =<< liftA2 (<) parameter parameter))
  , (OpCode 8, True <$ (store' . fromBool =<< liftA2 (==) parameter parameter))
  ]

jump :: TuringMachine m => (Value -> Bool) -> Instruction m ()
jump p = do
  v <- lift shift
  i <- location
  when (p v) $ lift (put i)

fromBool :: Bool -> Value
fromBool False = 0
fromBool True = 1
