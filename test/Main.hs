module Main where

import AdventOfCode (Solver (..), solvers)
import Control.Monad (forM_)
import Test.Hspec
import Prelude

main :: IO ()
main = hspec do
  forM_ solvers \(name, Solver {spec}) ->
    describe name spec
