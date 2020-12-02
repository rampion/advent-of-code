{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Freer where

import Data.Functor.Identity
import Control.Monad (ap)

data FreerT f m a where
  Pure :: a -> FreerT f m a
  Lift :: m (FreerT f m a) -> FreerT f m a
  Bind :: f x -> (x -> FreerT f m a) -> FreerT f m a

type Freer f = FreerT f Identity

instance Functor m => Functor (FreerT f m) where
  fmap h (Pure a) = Pure (h a)
  fmap h (Lift ma) = Lift (fmap h <$> ma)
  fmap h (Bind fx g) = Bind fx (fmap h . g)

instance Functor m => Applicative (FreerT f m) where
  pure = Pure
  (<*>) = ap

instance Functor m => Monad (FreerT f m) where
  Pure a >>= h = h a
  Lift ma >>= h = Lift $ fmap (>>= h) ma
  Bind fx g >>= h = Bind fx ((>>= h) . g)

runFreerT :: Monad m => FreerT f m a -> (forall x. f x -> m x) -> m a
runFreerT (Pure a) _ = return a
runFreerT (Lift ma) h = (`runFreerT` h) =<< ma
runFreerT (Bind fx g) h = (`runFreerT` h) . g =<< h fx
