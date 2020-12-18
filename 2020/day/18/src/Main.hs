{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity)
import Data.List (foldl')
import System.Environment (getArgs)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (parseFromFile)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    expressions <- either (fail . show) return =<< parseFromFile expressions filename
    expected1 <- loadExpected filename "part1"
    expected2 <- loadExpected filename "part2"

    putStrLn "\tpart 1: sum of expressions evaluated with L-to-R precedence"
    putStrLn $ "\t" ++ check (part1 expressions) expected1

    putStrLn "\tpart 2: sum of expressions evaluated with higher addition precedence"
    putStrLn $ "\t" ++ check (part2 expressions) expected2

loadExpected :: String -> String -> IO (Maybe Int)
loadExpected filename suffix = Just . read . head . words <$> readFile (filename ++ "." ++ suffix) <|> return Nothing

check :: Int -> Maybe Int -> String
check n (Just n')
  | n /= n' = color 31 "✗" ++ " " ++ show n
  | otherwise = color 32 "✓" ++ " " ++ show n
check n Nothing = color 33 "?" ++ " " ++ show n

color :: Int -> String -> String
color c text = "\x1b[" ++ show c ++ "m" ++ text ++ "\x1b[m"

--------------------------------------------------------------------------------

part2 :: [Expression] -> Int
part2 = sum . map eval
  where
    eval :: Expression -> Int
    eval (Expression t ops) = product $ multiplicands (term t) ops

    term :: Term -> Int
    term (Literal n) = n
    term (Parenthesized expr) = eval expr

    multiplicands :: Int -> [Operation] -> [Int]
    multiplicands n (Plus t : ops) = multiplicands (n + term t) ops
    multiplicands n (Times t : ops) = n : multiplicands (term t) ops
    multiplicands n [] = [n]

--------------------------------------------------------------------------------

part1 :: [Expression] -> Int
part1 = sum . map eval
  where
    eval :: Expression -> Int
    eval (Expression t ops) = foldl' apply (term t) ops

    term :: Term -> Int
    term (Literal n) = n
    term (Parenthesized expr) = eval expr

    apply :: Int -> Operation -> Int
    apply n (Plus t) = n + term t
    apply n (Times t) = n * term t

--------------------------------------------------------------------------------

type Parser a = forall s. Stream s Identity Char => Parsec s () a

expressions :: Parser [Expression]
expressions = expression `endBy1` newline
  where
    expression :: Parser Expression
    expression = Expression <$> term <*> many1 operation

    term :: Parser Term
    term = literal <|> parenthesized

    literal :: Parser Term
    literal = Literal . read <$> many1 digit

    parenthesized :: Parser Term
    parenthesized = Parenthesized <$> (char '(' *> expression <* char ')')

    operation :: Parser Operation
    operation = char ' ' *> operator <* char ' ' <*> term

    operator :: Parser (Term -> Operation)
    operator = plus <|> times

    plus :: Parser (Term -> Operation)
    plus = Plus <$ char '+'

    times :: Parser (Term -> Operation)
    times = Times <$ char '*'

--------------------------------------------------------------------------------

data Expression = Expression Term [Operation]

data Term
  = Literal Int
  | Parenthesized Expression

data Operation
  = Plus Term
  | Times Term
