{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}

module Main where

-- import Control.Category ((>>>))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.String (parseFromFile)

main :: IO ()
main =
  getArgs >>= traverse_ \filename -> do
    putStrLn filename

    notes <- either (fail . show) return =<< parseFromFile notesP filename

    putStrLn "\tpart 1: ticket scanning error rate"
    putStrLn $ "\t" ++ show (part1 notes)

    putStrLn "\tpart 2: product of departure values on your ticket"
    putStrLn $ "\t" ++ show (part2 notes)

--------------------------------------------------------------------------------

part2 :: Notes -> Int
part2 Notes {fieldRules, yourTicket, nearbyTickets} =
  product $ label yourTicket `Map.restrictKeys` Set.fromList departureFields
  where
    departureFields = filter (List.isPrefixOf "departure") fieldNames
    fieldNames = fst <$> fieldRules
    label = Map.fromList . zip fieldOrder
    fieldOrder = findOrder fieldRules $ filter (validate fieldRules) nearbyTickets

validate :: [FieldRule] -> Ticket -> Bool
validate fieldRules = all (hasValue fieldRules)

findOrder :: [FieldRule] -> [Ticket] -> [Field]
findOrder fieldRules = findPermutation . findCandidates fieldRules

findCandidates :: [FieldRule] -> [Ticket] -> [[Field]]
findCandidates fieldRules tickets = do
  fieldValues <- List.transpose tickets
  return [field | (field, ranges) <- fieldRules, fieldValues & all \n -> ranges `contain` n]

findPermutation :: (Show a, Ord a) => [[a]] -> [a]
findPermutation =
  map snd
    . List.sortBy (comparing fst)
    . head
    . search 0
    . Map.fromList
    . zip [0 ..]
    . map (Set.fromList)
  where
    search :: (Show a, Ord a) => Int -> Map.Map Int (Set.Set a) -> [[(Int, a)]]
    search depth m = case Map.lookupMin $ Map.fromListWith (++) [(Set.size xs, [p]) | p@(_, xs) <- Map.assocs m] of
      Just (_, (ix, xs) : _) -> do
        m <- return $ Map.delete ix m
        p@(_, x) <- (,) ix <$> Set.toList xs
        ps <- search (depth + 1) (Set.delete x <$> m)
        return (p : ps)
      _ -> return []

--------------------------------------------------------------------------------

part1 :: Notes -> Int
part1 Notes {fieldRules, nearbyTickets} = sum do
  filter (not . hasValue fieldRules) =<< nearbyTickets

hasValue :: [FieldRule] -> Int -> Bool
hasValue fieldRules n =
  fieldRules & any \(_, ranges) ->
    ranges `contain` n

contain :: [Range] -> Int -> Bool
contain ranges n =
  ranges & any \(lo, hi) ->
    lo <= n && n <= hi

--------------------------------------------------------------------------------

type Parser a = forall s. Stream s Identity Char => Parsec s () a

notesP :: Parser Notes
notesP =
  Notes
    <$> fieldRulesP
    <* newline
    <*> yourTicketP
    <* newline
    <*> nearbyTicketsP
    <* eof

fieldRulesP :: Parser [FieldRule]
fieldRulesP = fieldRuleP `endBy1` newline
  where
    fieldRuleP :: Parser FieldRule
    fieldRuleP = (,) <$> nameP <* string ": " <*> rangesP

    nameP :: Parser String
    nameP = many1 (letter <|> char ' ')

    rangesP :: Parser [Range]
    rangesP = rangeP `sepBy1` string " or "

    rangeP :: Parser Range
    rangeP = (,) <$> intP <* char '-' <*> intP

yourTicketP :: Parser Ticket
yourTicketP = string "your ticket:" *> newline *> ticketP <* newline

nearbyTicketsP :: Parser [Ticket]
nearbyTicketsP = string "nearby tickets:" *> newline *> (ticketP `endBy1` newline)

intP :: Parser Int
intP = read <$> many1 digit

ticketP :: Parser Ticket
ticketP = intP `sepBy1` char ','

--------------------------------------------------------------------------------

data Notes = Notes
  { fieldRules :: [FieldRule],
    yourTicket :: Ticket,
    nearbyTickets :: [Ticket]
  }

type FieldRule = (Field, [Range])

type Field = String

type Range = (Int, Int)

type Ticket = [Int]
