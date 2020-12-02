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
main = return () where
  _ = startImage (interpret markIII)

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
