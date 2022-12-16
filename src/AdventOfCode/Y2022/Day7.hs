module AdventOfCode.Y2022.Day7 where

import AdventOfCode.Y2022.Prelude
import Data.Foldable (asum)
import Data.String (IsString)
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Functor ((<&>))

solver :: Solver
solver = Solver
  { parser
  , part1
  , part2
  , spec = check solver Example
    { raw = [text|
        $ cd /
        $ ls
        dir a
        14848514 b.txt
        8504156 c.dat
        dir d
        $ cd a
        $ ls
        dir e
        29116 f
        2557 g
        62596 h.lst
        $ cd e
        $ ls
        584 i
        $ cd ..
        $ cd ..
        $ cd d
        $ ls
        4060174 j
        8033020 d.log
        5626152 d.ext
        7214296 k
      |]
    , parsed = 
      [ CD Outermost
      , LS 
        [ Dir "a"
        , File 14848514 "b.txt"
        , File 8504156 "c.dat"
        , Dir "d"
        ]
      , CD (In "a")
      , LS
        [ Dir "e"
        , File 29116 "f"
        , File 2557 "g"
        , File 62596 "h.lst"
        ]
      , CD (In "e")
      , LS
        [ File 584 "i"
        ]
      , CD Out
      , CD Out
      , CD (In "d")
      , LS
        [ File 4060174 "j"
        , File 8033020 "d.log"
        , File 5626152 "d.ext"
        , File 7214296 "k"
        ]
      ]
    , part1output = 95437
    , part2output = 24933642
    }
  }

data CommandLog = CD Path | LS [Listing]
  deriving stock (Show, Eq)

data Path = Outermost | In DirName | Out
  deriving stock (Show, Eq)

data Listing = Dir DirName | File Int FileName
  deriving stock (Show, Eq)

newtype DirName = DirName String
  deriving newtype (Show, Eq, Ord, IsString)

newtype FileName = FileName String
  deriving newtype (Show, Eq, Ord, IsString)

type DirPath = [DirName]

parser :: Parser [CommandLog]
parser = many commandLog where
  commandLog = string "$ " *> (cd <|> ls)
  cd = CD <$ string "cd " <*> path <* newline
  path = asum
    [ Outermost <$ char '/'
    , Out <$ string ".."
    , In <$> dirName
    ]
  ls = LS <$ string "ls" <* newline <*> (listing `endBy` newline)
  listing = dir <|> file
  dir = Dir <$ string "dir " <*> dirName
  file = File <$> int <* char ' ' <*> fileName
  dirName = DirName <$> some (noneOf "\n")
  fileName = FileName <$> some (noneOf "\n")
  int = read <$> some digit

dirSizes :: [CommandLog] -> Map.Map DirPath Int
dirSizes
  = loeb
  . Map.mapWithKey
      do \cwd ls m -> sum
            do ls <&> \case
                  Dir d -> m Map.! (d : cwd)
                  File n _ -> n
  . snd
  . List.foldl'
      do \(cwd, tree) -> \case
            CD Outermost -> ([], tree)
            CD (In name) -> (name : cwd, tree)
            CD Out -> (tail cwd, tree)
            LS ls -> (cwd, Map.insert cwd ls tree)
      do (error "unknown", Map.empty)

loeb :: Functor f => f (f a -> a) -> f a
loeb formulae = values where
  values = fmap ($values) formulae

part1 :: [CommandLog] -> Int
part1
  = sum
  . Map.filter (<= 100000)
  . dirSizes

part2 :: [CommandLog] -> Int
part2 input = freedSize where
  sizes = dirSizes input
  totalSize = 70_000_000
  availSize = 30_000_000
  maxSize = totalSize - availSize
  currSize = sizes Map.! []
  excess = currSize - maxSize
  freedSize = minimum do Map.filter (>= excess) sizes
