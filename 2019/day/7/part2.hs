{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Monad (when, (<=<), forM)
import Control.Monad.Except (ExceptT(..), MonadError, throwError, runExceptT)
-- import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (ReaderT(..), reader)
-- import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT, Reader, runReader)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT(..), liftF, FreeF(..))
-- 
-- import Data.Function (on)
-- import Data.Functor.Identity (Identity(..))
import Data.Functor ((<&>))
-- import Data.List (maximum, permutations, foldl')
-- import Data.List.Split (splitOn)
-- import Data.Proxy (Proxy(..))
import Data.STRef (STRef, readSTRef, writeSTRef, newSTRef)
import Data.Vector (Vector,MVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Data.Void (Void)
-- 
-- import Pair (Pair(..))
-- 
import Test.DocTest (doctest)

import Computer
-- import Freer

test :: IO ()
test = doctest ["2019/day/7/part2.hs"]

main :: IO ()
-- <<<<<<< HEAD
main = return () where
  _ = startImage (interpret markIII)
-- =======
main = return ()

xmain :: IO ()
xmain = 
  print .
  maximum .
  flip map (permutations [5,6,7,8,9]) .
  runAmplifiers .
  readProgram =<<
  readFile "2019/day/7/input"

-- |
-- >>> runAmplifiers [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9,8,7,6,5]
-- 139629729
-- >>> runAmplifiers [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9,7,8,5,6]
-- 18217
runAmplifiers :: Program -> [Value] -> Value
runAmplifiers v ps = runST $ do
  amps <- forM ps $ startAmplifier v
  let xs = foldl runAmplifier (1:xs) amps
  in last xs

runAmplifier :: Program -> [Value] -> Value -> [Value]
runAmplifier v inputs phase = runST $ do
  v <- Vector.thaw (Vector.fromList v)
  InterpreterState{..} <- getInterpreter (runAPI day5part2) 
                            `runReaderT` v
                            `execStateT` InterpreterState (Location 0) inputs id
  return $ outputs []

newtype Interpreter s a = Interpreter
  { getInterpreter :: ReaderT (ProgramBuffer s) (StateT InterpreterState (ST s)) a }
  deriving (Functor, Applicative, Monad)

data InterpreterState = InterpreterState
  { headLocation  :: Location
  , inputs        :: [Value]
  , outputs       :: ([Value] -> [Value])
  }

instance MonadReader (ProgramBuffer s) (Interpreter s) where
  reader f = Interpreter $ reader f
  local f (Interpreter ma) = Interpreter $ local f ma
    
instance PrimMonad (Interpreter s) where
  type PrimState (Interpreter s) = PrimState (ST s)
  primitive = Interpreter . primitive

instance MonadError ProgramError (Interpreter s) where
  throwError = error . show
  catchError = const

instance MonadState Location (Interpreter s) where
  state f = Interpreter . state $ \s -> 
    let (a, i) = f (headLocation s) in (a, s { headLocation = i })

instance MonadInput Value (Interpreter s) where
  input = Interpreter . state $ \s ->
    let ~(i:is) = inputs s in (i, s { inputs = is })

instance MonadOutput Value (Interpreter s) where
  output v = Interpreter . state $ \s -> ((), s { outputs = outputs s . (v:) })

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
-- >>>>>>> refs/remotes/origin/master

runAmplifiers :: [Value] -> Program -> Either String Int
runAmplifiers ps v = runRT $ do
  -- xs :: MVector s (Effect (VM s Void))
  xs <- liftST . Vector.thaw . Vector.fromList <=< forM ps $ \p -> do
    send p <=< startImage (interpret markIII) <=< liftST $ Env <$> newSTRef (Location 0) <*> Vector.thaw v
  let n = MVector.length xs
  throwError "incomplete implemenatation"

runRT :: (forall s. RT s a) -> Either String a
runRT m = runST $ runExceptT $ getRT m

liftST :: ST s a -> RT s a
liftST = RT . lift

startImage :: Image s () -> Env s -> RT s (Effect (VM s Void))
startImage m e = stepVM $ do
  runImage m `runReaderT` e
  liftF Halt

stepVM :: VM s Void -> RT s (Effect (VM s Void))
stepVM mu = runFreeT mu <&> \(Free x) -> x

send :: Value -> Effect (VM s Void) -> RT s (Effect (VM s Void))
send v Halt = throwError $ "Attempt to send " ++ show v ++ " as input to halted amplifier"
send v (DemandInput f) = stepVM $ f v
send v (ProvideOutput o m) = return . ProvideOutput o . FreeT . fmap Free $ send v =<< stepVM m

type Program = Vector Value

type VM s = FreeT Effect (RT s)

newtype RT s a = RT { getRT :: ExceptT String (ST s) a }
  deriving (Functor, Applicative, Monad, MonadError String)

newtype Image s a = Image { runImage :: ReaderT (Env s) (VM s) a }
  deriving (Functor, Applicative, Monad, MonadError String)

data Env s = Env
  { offset :: STRef s Location
  , memory :: MVector s Value
  }

data Effect a
  = DemandInput (Value -> a)
  | ProvideOutput Value a
  | Halt
  deriving Functor

liftVM :: (Env s -> a) -> (a -> VM s b) -> Image s b
liftVM f g = Image $ lift . g =<< reader f

instance MonadState Location (Image s) where
  state f = liftVM offset $ \r -> lift . liftST $ do
    (a, o) <- f <$> readSTRef r
    writeSTRef r o
    return a

instance TuringMachine (Image s) where
  fetch (Location i) = liftVM memory $ \v -> lift . RT $ do
    let n = MVector.length v
    when (i < 0 || n <= i) $ do
      throwError $ "Out of bounds attempt to read at position " ++ show i ++ " of " ++ show n
    lift $ MVector.read v i

  store (Location i) a = liftVM memory $ \v -> lift . RT $ do
    let n = MVector.length v
    when (i < 0 || n <= i) $ do
      throwError $ "Out of bounds attempt to write " ++ show a ++ " at position " ++ show i ++ " of " ++ show n
    lift $ MVector.write v i a
    
instance TuringMachineIO (Image s) where
  input = Image . lift . liftF $ DemandInput id
  output v = Image . lift . liftF $ ProvideOutput v ()
