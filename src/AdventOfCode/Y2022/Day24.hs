module AdventOfCode.Y2022.Day24 where

import AdventOfCode.Y2022.Prelude

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = xcheck parser part1 part2 Example
    { raw = [text|
      |]
    , parsed = error "unknown"
    , part1output = error "unknown"
    , part2output = error "unknown"
    }
  }

parser :: Parser Void
parser = error "unimplemented"

part1 :: Void -> ()
part1 = error "unimplemented"

part2 :: Void -> ()
part2 = error "unimplemented"
