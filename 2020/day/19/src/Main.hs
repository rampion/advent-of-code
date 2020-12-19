{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall -Werror -Wextra -Wno-name-shadowing #-}

module Main where

import Control.Applicative ((<|>))
-- import Control.Arrow (left)
import Data.Foldable (asum, traverse_)
import Data.Functor.Identity (Identity)
import qualified Data.IntMap as IntMap
import System.Environment (getArgs)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  let filename = "data/example-2"
  (rules, _) <- either (fail . show) (return . updateRulesEightAndEleven) =<< parseFromFile scenario filename
  print $ matchRule0'' rules "aaaaabbaabaaaaababaa"

main_ :: IO ()
main_ =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    scenario <- either (fail . show) return =<< parseFromFile scenario filename

    putStrLn "\tpart 1: Number of messages that completely match rule 0"
    let part1 = countMatchRule0 scenario
    expected1 <- parseExpected (filename ++ ".part1")
    putStrLn $ "\t" ++ check part1 expected1 ++ " " ++ show part1

    putStrLn "\tpart 2: Number of messages that completely match rule 0 with updated rules 8 and 11"
    let part2 = countMatchRule0' (updateRulesEightAndEleven scenario)
    expected2 <- parseExpected (filename ++ ".part2")
    putStrLn $ "\t" ++ check part2 expected2 ++ " " ++ show part1

--------------------------------------------------------------------------------

type NewParser a = a -> (String -> a) -> String -> a

matchRule0'' :: [Rule] -> Message -> (Maybe String)
matchRule0'' = \rules -> build0 rules Nothing Just
  where
    build0 :: [Rule] -> NewParser a
    build0 rules = let built = IntMap.fromList $ map (fmap $ toParser built) rules in built IntMap.! 0

    toParser :: IntMap.IntMap (NewParser a) -> Expression -> NewParser a
    toParser _ (Literal expected) = \nothing just -> \case
      actual : rest | expected == actual -> just rest
      _ -> nothing
    toParser rules (Pattern ps) =
      oneOf (allOf . map (rules IntMap.!) <$> ps)

    oneOf :: [NewParser a] -> NewParser a
    oneOf [] nothing _just = const nothing
    oneOf (p : ps) nothing just = \str -> p (oneOf ps nothing just str) just str

    allOf [15 14]
    allOf [oneOf [1,14] 14]
    allOf [(\nothing just str -> 1 (oneOf [14] nothing just str) just str), 14]
    \nothing just str -> 1 (oneOf [14] nothing just str) (allOf [14] nothing just):wa




    allOf :: [NewParser a] -> NewParser a
    allOf [] _nothing just = just
    allOf (p : ps) nothing just = p nothing (allOf ps nothing just)

{-
type Parser'' = [Index] -> String -> IO (Maybe String)

matchRule0'' :: [Rule] -> Message -> IO (Maybe String)
matchRule0'' = \rules -> build0 rules []
  where
    build0 :: [Rule] -> Parser''
    build0 rules = let built = IntMap.fromList $ map (uncurry $ toParser built) rules in built IntMap.! 0

    toParser :: IntMap.IntMap Parser'' -> Index -> Expression -> (Index, Parser'')
    toParser _ ix (Literal expected) = (,) ix \ixs str -> do
      putStrLn $ show ix ++ ": " ++ check (take 1 str) (Just [expected]) ++ show (tail str) ++ ", " ++ show ixs
      return case str of
        actual : rest | expected == actual -> Just rest
        _ -> Nothing
    toParser rules ix (Pattern ps) = (,) ix \ixs str -> do
      putStrLn $ show ix ++ ": " ++ show str ++ ", " ++ show ps ++ " " ++ show ixs
      oneOf (allOf . map (\ix -> (ix, rules IntMap.! ix)) <$> ps) ixs str

    oneOf :: [Parser''] -> Parser''
    oneOf [] _ixs _str = return Nothing
    oneOf (p : ps) ixs str =
      p ixs str >>= \case
        Nothing -> oneOf ps ixs str
        r -> return r

    allOf :: [(Index, Parser'')] -> Parser''
    allOf [] _ixs str = return (Just str)
    allOf ((_ix, p) : ps) ixs str =
      p (map fst ps ++ ixs) str >>= \case
        Just str -> allOf ps ixs str
        Nothing -> return Nothing
-}

--------------------------------------------------------------------------------

updateRulesEightAndEleven :: Scenario -> Scenario
updateRulesEightAndEleven (rules, messages) = (map replace rules, messages)
  where
    replace (8, _) = (8, Pattern [[42], [42, 8]])
    replace (11, _) = (11, Pattern [[42, 31], [42, 11, 31]])
    replace r = r

countMatchRule0' :: Scenario -> Int
countMatchRule0' = length . matchRule0'

matchRule0' :: Scenario -> [Message]
matchRule0' = \(rules, messages) -> filter (match0 rules) messages
  where
    match0 :: [Rule] -> Message -> Bool
    match0 rules = maybe False null . build0 rules

    build0 :: [Rule] -> Parser'
    build0 rules = let built = IntMap.fromList $ map (toParser built <$>) rules in built IntMap.! 0

    toParser :: IntMap.IntMap Parser' -> Expression -> Parser'
    toParser _ (Literal expected) = \case
      actual : rest | expected == actual -> Just rest
      _ -> Nothing
    toParser rules (Pattern ps) = oneOf (allOf . map (rules IntMap.!) <$> ps)

    oneOf :: [Parser'] -> Parser'
    oneOf [] = const Nothing
    oneOf (p : ps) = \s -> p s <|> oneOf ps s

    allOf :: [Parser'] -> Parser'
    allOf [] = Just
    allOf (p : ps) = \s -> p s >>= allOf ps

type Parser' = String -> Maybe String

--------------------------------------------------------------------------------

countMatchRule0 :: Scenario -> Int
countMatchRule0 = length . matchRule0

matchRule0 :: Scenario -> [Message]
matchRule0 = \(rules, messages) -> filter (match0 rules) messages
  where
    match0 :: [Rule] -> Message -> Bool
    match0 rules = either (const False) (const True) . (parse (build rules IntMap.! 0 <* eof) "message")

    build :: Stream s Identity Char => [Rule] -> IntMap.IntMap (Parsec s () Message)
    build rules = let built = IntMap.fromList $ map (toParser built <$>) rules in built

    toParser :: Stream s Identity Char => IntMap.IntMap (Parsec s () Message) -> Expression -> Parsec s () Message
    toParser _ (Literal c) = string [c]
    toParser rules (Pattern ps) = asum [try $ concat <$> traverse (rules IntMap.!) p | p <- ps]

--
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
