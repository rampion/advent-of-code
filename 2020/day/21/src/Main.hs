{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (parseFromFile)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    foods <- either (fail . show) return =<< parseFromFile foods filename

    putStrLn "part 1: how many times do ingredients appear that can't contain any allergens"
    expected1 <- fmap read <$> parseExpected (filename ++ ".part1")
    let actual1 = part1 foods
    putStrLn $ "\t" ++ check actual1 expected1 ++ " " ++ show actual1

    putStrLn "part 2: dangerous ingredients, alphabetical by allergen"
    expected2 <- parseExpected (filename ++ ".part2")
    let actual2 = part2 foods
    putStrLn $ "\t" ++ check actual2 expected2 ++ " " ++ actual2

--------------------------------------------------------------------------------

part2 :: [Food] -> String
part2 = List.intercalate "," . fmap snd . List.sort . pair

pair :: [Food] -> [(Allergen, Ingredient)]
pair = (search <*> transpose <*> uniques) . potentialSources
  where
    search :: MultiMap Allergen Ingredient -> MultiMap Ingredient Allergen -> [(Allergen, Ingredient)] -> [(Allergen, Ingredient)]
    search _ _ [] = []
    search srcs dsts known =
      let (srcs', candidates) = List.foldl' (discover dsts) (srcs, []) (fmap snd known)
       in known ++ search srcs' dsts candidates

    discover :: MultiMap Ingredient Allergen -> (MultiMap Allergen Ingredient, [(Allergen, Ingredient)]) -> Ingredient -> (MultiMap Allergen Ingredient, [(Allergen, Ingredient)])
    discover dsts found ingredient = List.foldl' (checkAllergen ingredient) found (dsts Map.! ingredient)

    checkAllergen ingredient (srcs, candidates) allergen =
      let is = Set.delete ingredient (srcs Map.! allergen)
       in ( Map.insert allergen is srcs,
            case Set.toList is of
              [otherIngredient] -> (allergen, otherIngredient) : candidates
              _ -> candidates
          )

uniques :: MultiMap k v -> [(k, v)]
uniques mm = do
  (k, vs) <- Map.toList mm
  case Set.toList vs of
    [v] -> return (k, v)
    _ -> []

transpose :: (Ord k, Ord v) => MultiMap k v -> MultiMap v k
transpose = Map.fromListWith Set.union . concatMap pivot . Map.toList
  where
    pivot (k, vs) = (\v -> (v, Set.singleton k)) <$> Set.toList vs

type MultiMap k v = Map k (Set v)

--------------------------------------------------------------------------------

part1 :: [Food] -> Int
part1 foods = length $ Prelude.filter hypoallergenic ingredients
  where
    ingredients :: [Ingredient]
    ingredients = fst =<< foods

    hypoallergenic :: Ingredient -> Bool
    hypoallergenic = not . (`Set.member` Set.unions (Map.elems (potentialSources foods)))

potentialSources :: [Food] -> MultiMap Allergen Ingredient
potentialSources foods = Map.fromListWith Set.intersection do
  (ingredients, potentialSources) <- foods
  allergen <- potentialSources
  return (allergen, Set.fromList ingredients)

--------------------------------------------------------------------------------

check :: Eq a => a -> Maybe a -> String
check actual = \case
  Just expected | expected == actual -> ansi green "✓"
  Nothing -> ansi yellow "?"
  _ -> ansi red "✗"
  where
    ansi :: Int -> String -> String
    ansi color text = "\x1b[" ++ show color ++ "m" ++ text ++ "\x1b[m"
    red = 31
    green = 32
    yellow = 33

parseExpected :: FilePath -> IO (Maybe String)
parseExpected filename =
  (Just . head . words <$> readFile filename) <|> return Nothing

--------------------------------------------------------------------------------

type Parser a = forall s. Stream s Identity Char => Parsec s () a

foods :: Parser [Food]
foods = food `endBy1` newline
  where
    food :: Parser Food
    food = (,) <$> ingredients <*> allergens

    ingredients :: Parser [Ingredient]
    ingredients = many1 (ingredient <* space)

    ingredient :: Parser Ingredient
    ingredient = many1 letter

    allergens :: Parser [Allergen]
    allergens = string "(contains " *> (allergen `sepBy` string ", ") <* char ')'

    allergen :: Parser Allergen
    allergen = many1 letter

--------------------------------------------------------------------------------

type Food = ([Ingredient], [Allergen])

type Ingredient = String

type Allergen = String
