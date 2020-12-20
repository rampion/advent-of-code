{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall -Werror -Wextra -Wno-name-shadowing #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.Environment (getArgs)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    scenario <- either (fail . show) return =<< parseFromFile scenario filename

    putStrLn "\tpart 1: Number of messages that completely match rule 0"
    let part1 = countMatchRule0 scenario
    expected1 <- parseExpected (filename ++ ".part1")
    putStrLn $ "\t" ++ check part1 expected1 ++ " " ++ show part1

    putStrLn "\tpart 2: Number of messages that completely match rule 0 with updated rules 8 and 11"
    let part2 = countMatchRule0 (updateRulesEightAndEleven scenario)
    expected2 <- parseExpected (filename ++ ".part2")
    putStrLn $ "\t" ++ check part2 expected2 ++ " " ++ show part2

--------------------------------------------------------------------------------

updateRulesEightAndEleven :: Scenario -> Scenario
updateRulesEightAndEleven (rules, messages) = (map replace rules, messages)
  where
    replace (8, _) = (8, Pattern [[42], [42, 8]])
    replace (11, _) = (11, Pattern [[42, 31], [42, 11, 31]])
    replace r = r

--------------------------------------------------------------------------------

countMatchRule0 :: Scenario -> Int
countMatchRule0 (rules, messages) = length $ filter (any null . matchRule0 rules) messages

type Nondeterministic = String -> [String]

matchRule0 :: [Rule] -> Message -> [String]
matchRule0 = build0
  where
    build0 :: [Rule] -> Nondeterministic
    build0 rules = built IntMap.! 0
      where
        built = IntMap.fromList $ map (fmap $ toParser built) rules

    toParser :: IntMap Nondeterministic -> Expression -> Nondeterministic
    toParser _ (Literal expected) = \case
      (actual : rest) | expected == actual -> return rest
      _ -> []
    toParser rules (Pattern ps) = oneOf (allOf . map (rules IntMap.!) <$> ps)

    oneOf :: [Nondeterministic] -> Nondeterministic
    oneOf ps = concat . sequence ps

    allOf :: [Nondeterministic] -> Nondeterministic
    allOf = foldr (>=>) return

--------------------------------------------------------------------------------

scenario :: Parser Scenario
scenario = (,) <$> rules <* newline <*> messages <* eof
  where
    rules :: Parser [Rule]
    rules = rule `endBy1` newline

    messages :: Parser [Message]
    messages = message `endBy1` newline

    rule :: Parser Rule
    rule = (,) <$> index <* string ": " <*> expression

    message :: Parser Message
    message = many1 token

    index :: Parser Index
    index = read <$> many1 digit

    expression :: Parser Expression
    expression = literal <|> pattern

    token :: Parser Char
    token = noneOf "\n"

    literal :: Parser Expression
    literal = char '"' *> fmap Literal token <* char '"'

    pattern :: Parser Expression
    pattern = Pattern <$> concatenation `sepBy1` string " | "

    concatenation :: Parser [Index]
    concatenation = (:) <$> index <*> indexes

    indexes :: Parser [Index]
    indexes = try (char ' ' *> concatenation) <|> pure []

type Parser a = forall s. Stream s Identity Char => Parsec s () a

--------------------------------------------------------------------------------

type Scenario = ([Rule], [Message])

type Rule = (Index, Expression)

type Message = String

type Index = Int

data Expression = Literal Char | Pattern [[Index]]
  deriving (Show)

--------------------------------------------------------------------------------

check :: Eq a => a -> Maybe a -> String
check actual = \case
  Just expected | expected == actual -> ansi green "✓"
  Nothing -> ansi yellow "?"
  _ -> ansi red "✗"
  where
    ansi :: Int -> String -> String
    ansi color text = "\x1b[" ++ show color ++ "m" ++ text ++ "\x1b[m"
    green = 32
    yellow = 33
    red = 31

parseExpected :: FilePath -> IO (Maybe Int)
parseExpected path =
  (Just . read . head . words <$> readFile path) <|> return Nothing
