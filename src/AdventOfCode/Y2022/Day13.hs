{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}

module AdventOfCode.Y2022.Day13 where

import AdventOfCode.Y2022.Prelude
import Control.Arrow (first)
import Data.List (sort)
import GHC.Exts

solver :: Solver
solver =
  Solver
    { parser
    , part1
    , part2
    , spec = do
        let exampleInput =
              [
                ( [1, 1, 3, 1, 1]
                , [1, 1, 5, 1, 1]
                )
              ,
                ( [[1], [2, 3, 4]]
                , [[1], 4]
                )
              ,
                ( [9]
                , [[8, 7, 6]]
                )
              ,
                ( [[4, 4], 4, 4]
                , [[4, 4], 4, 4, 4]
                )
              ,
                ( [7, 7, 7, 7]
                , [7, 7, 7]
                )
              ,
                ( []
                , [3]
                )
              ,
                ( [[[]]]
                , [[]]
                )
              ,
                ( [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
                , [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]
                )
              ]

        check parser part1 part2 Example
            { raw = [text|
                [1,1,3,1,1]
                [1,1,5,1,1]

                [[1],[2,3,4]]
                [[1],4]

                [9]
                [[8,7,6]]

                [[4,4],4,4]
                [[4,4],4,4,4]

                [7,7,7,7]
                [7,7,7]

                []
                [3]

                [[[]]]
                [[]]

                [1,[2,[3,[4,[5,6,7]]]],8,9]
                [1,[2,[3,[4,[5,6,0]]]],8,9]
              |] <> "\n"
            , parsed = exampleInput
            , part1output = 13
            , part2output = 140
            }

        it "orders the packets correctly" do
          order exampleInput `shouldBe`
            [ []
            , [[]]
            , [[[]]]
            , [1, 1, 3, 1, 1]
            , [1, 1, 5, 1, 1]
            , [[1], [2, 3, 4]]
            , [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]
            , [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
            , [[1], 4]
            , [[2]]
            , [3]
            , [[4, 4], 4, 4]
            , [[4, 4], 4, 4, 4]
            , [[6]]
            , [7, 7, 7]
            , [7, 7, 7, 7]
            , [[8, 7, 6]]
            , [9]
            ]
    }

type Packet :: Type
newtype Packet = Packet [PacketValue]
  deriving stock (Eq, Ord)

-- for debugging
instance IsList Packet where
  type Item Packet = PacketValue
  fromList = Packet
  toList (Packet as) = as

-- for debugging
instance Show Packet where
  show (Packet as) = show as

-- for completeness
instance Read Packet where
  readsPrec p = readMap Packet (readsPrec p)

type PacketValue :: Type
data PacketValue = IntValue Int | ListValue [PacketValue]
  deriving stock (Eq)

-- for debugging
instance Num PacketValue where
  fromInteger = IntValue . fromInteger

  -- for completeness
  (+) = liftO2 (id ? id ?.. (+))
  (-) = liftO2 (id ? negate ?.. (-))
  (*) = liftO2 (const 0 ? const 0 ?.. (*))
  abs = omap abs
  signum = omap signum
  negate = omap negate

-- for completeness
omap :: (Int -> Int) -> PacketValue -> PacketValue
omap f = \case
  IntValue a -> IntValue (f a)
  ListValue as -> ListValue (map (omap f) as)

type (+) :: Type -> Type -> Type
type (+) = Either

infixl 6 +

-- opposite fixity of (|||)
(?) :: (a -> c) -> (b -> c) -> a + b -> c
(?) = either

infixl 6 ?

(?..) :: (a -> d) -> (b -> c -> d) -> a + (b, c) -> d
(?..) f = either f . uncurry

infixl 6 ?..

-- for completeness
liftO2 :: (Int + Int + (Int, Int) -> Int) -> PacketValue -> PacketValue -> PacketValue
liftO2 f = \case
  IntValue a -> \case
    IntValue b -> IntValue (f (Right (a, b)))
    ListValue bs -> ListValue (recur [IntValue a] bs)
  ListValue as -> \case
    IntValue b -> ListValue (recur as [IntValue b])
    ListValue bs -> ListValue (recur as bs)
  where
    recur = outerZipWith do
      omap (f . Left . Left) ? omap (f . Left . Right) ?.. liftO2 f

-- for completeness
outerZipWith :: (a + b + (a, b) -> c) -> [a] -> [b] -> [c]
outerZipWith f [] bs = map (f . Left . Right) bs
outerZipWith f as [] = map (f . Left . Left) as
outerZipWith f (a : as) (b : bs) = f (Right (a, b)) : outerZipWith f as bs

-- for debugging
instance IsList PacketValue where
  type Item PacketValue = PacketValue
  fromList = ListValue
  toList = \case
    ListValue as -> as
    a -> [a]

-- for debugging
instance Show PacketValue where
  show = \case
    IntValue a -> show a
    ListValue as -> show as

instance Ord PacketValue where
  IntValue a `compare` IntValue b = a `compare` b
  a `compare` b = Packet (toList a) `compare` Packet (toList b)

-- for completeness
instance Read PacketValue where
  readsPrec = \p -> readMap IntValue (readsPrec p) <> readMap ListValue (readsPrec p)

-- for completeness
readMap :: (a -> b) -> ReadS a -> ReadS b
readMap f = fmap (fmap (first f))

parser :: Parser [(Packet, Packet)]
parser = pair `sepBy` newline
  where
    pair = liftA2 (,) packet packet
    packet = read <$> some (noneOf "\n") <* newline

{-
packet = Packet <$> values <* newline
values = char '[' *> (value `sepBy` char ',') <* char ']'
value = int <|> list
int = IntValue . read <$> some digit
list = ListValue <$> values
-}

part1 :: [(Packet, Packet)] -> Int
part1 = sum . map fst . select . zip [1 ..]
  where
    select :: [(Int, (Packet, Packet))] -> [(Int, (Packet, Packet))]
    select = filter (uncurry (<) . snd)

part2 :: [(Packet, Packet)] -> Int
part2 =
  product
    . fmap fst
    . filter ((`elem` dividers) . snd)
    . zip [1 ..]
    . order

order :: [(Packet, Packet)] -> [Packet]
order =
  sort
    . (dividers ++)
    . concatMap fromPair
  where
    fromPair (t0, t1) = [t0, t1]

dividers :: [Packet]
dividers =
  [ [[2]]
  , [[6]]
  ]
