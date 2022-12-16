module Main where

import AdventOfCode (solvers)
import AdventOfCode.Spec

main :: IO ()
-- main = runSpecs @LastSpec do
main = runSpecs @AllSpecs do
  forM_ (Map.toList solvers) \(name, Solver{spec}) ->
    describe name spec
