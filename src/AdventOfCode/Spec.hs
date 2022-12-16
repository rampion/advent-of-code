module AdventOfCode.Spec 
  ( module AdventOfCode.Spec
  , module Test.Hspec
  )
where

import Test.Hspec
import Control.Monad.Writer (WriterT (..), tell)

class Monoid m => SpecMonoid m where
  toSpecMonoid :: Spec -> m
  fromSpecMonoid :: m -> Spec

newtype AllSpecs = AllSpecs (Spec -> Spec)
  deriving (Semigroup, Monoid) via Endo Spec

instance SpecMonoid AllSpecs where
  toSpecMonoid spec = AllSpecs (*> spec)
  fromSpecMonoid (AllSpecs m) = m do pure ()

newtype LastSpec = LastSpec (Spec -> Spec)
  deriving (Semigroup, Monoid) via Dual (Endo Spec)

instance SpecMonoid LastSpec where
  toSpecMonoid spec = LastSpec (const spec)
  fromSpecMonoid (LastSpec m) = m do pure ()

type SpecWriter = WriterT m IO ()

runSpecs :: SpecMonoid m => SpecWriter -> IO ()
runSpecs w = do
  ((), spec) <- runWriterT w
  hspec (fromSpecMonoid spec)

tellSpec :: SpecMonoid m => String -> SpecWith () -> SpecWriter
tellSpec name = tell . toSpecMonoid . Test.Hspec.describe name
