module AdventOfCode.Spec
  ( module AdventOfCode.Spec,
    module Test.Hspec,
  )
where

import Control.Monad.Writer (WriterT (..), tell)
import Data.Kind (Constraint, Type)
import Data.Monoid (Dual (..), Endo (..))
import Test.Hspec hiding (Example)
import Prelude

type SpecMonoid :: Type -> Constraint
class Monoid m => SpecMonoid m where
  toSpecMonoid :: Spec -> m
  fromSpecMonoid :: m -> Spec

type AllSpecs :: Type
newtype AllSpecs = AllSpecs (Spec -> Spec)
  deriving (Semigroup, Monoid) via Endo Spec

instance SpecMonoid AllSpecs where
  toSpecMonoid spec = AllSpecs (*> spec)
  fromSpecMonoid (AllSpecs m) = m do pure ()

type LastSpec :: Type
newtype LastSpec = LastSpec (Spec -> Spec)
  deriving (Semigroup, Monoid) via Dual (Endo Spec)

instance SpecMonoid LastSpec where
  toSpecMonoid spec = LastSpec (const spec)
  fromSpecMonoid (LastSpec m) = m do pure ()

type SpecWriter :: Type -> Type
type SpecWriter m = WriterT m IO ()

runSpecs :: SpecMonoid m => SpecWriter m -> IO ()
runSpecs w = do
  ((), spec) <- runWriterT w
  hspec (fromSpecMonoid spec)

tellSpec :: SpecMonoid m => SpecWith () -> SpecWriter m
tellSpec = tell . toSpecMonoid
