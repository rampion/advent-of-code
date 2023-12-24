module AdventOfCode.Y2023.Prelude
  ( module X
  , module AdventOfCode.Y2023.Prelude
  )
where

import AdventOfCode.Solver as X
import Control.Applicative as X
import Data.Kind as X
import Data.Void as X (Void, absurd)
import NeatInterpolation as X (text)
import Test.Hspec as X hiding (Example)
import Text.Parsec as X hiding (many, optional, (<|>))
import Prelude as X
import Data.Functor as X (($>))
import Data.Map qualified as LMap
import Data.Map.Strict qualified as SMap
import Data.Set as X (Set)

type LMap :: Type -> Type -> Type
type LMap = LMap.Map

type SMap :: Type -> Type -> Type
type SMap = SMap.Map
