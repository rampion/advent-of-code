{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Options.Generic (getRecord)
import Text.Parsec
import Text.Parsec.String (parseFromFile)
import System.Exit (die)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete)
import Control.Applicative ((<**>))
import Numeric (readHex)
import Control.Monad (replicateM)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  filepath <- getRecord "Solve"
  passports <-  either (die . show) return =<< parseFromFile input filepath

  let populated = containsFields (delete "cid" passportFields) passports

  putStrLn "part 1: passports containing all fields except possibly cid"
  putStrLn $ filepath ++ ": " ++ show (length populated) ++ " / " ++ show (length passports)
  
  let validated = validateFields passportPredicates populated

  putStrLn "part 2: passports with defined and valid values for all fields except possibly cid"
  putStrLn $ filepath ++ ": " ++ show (length validated) ++ " / " ++ show (length passports)

newtype Passport = Passport { getPassport :: Map String String }

passportFields :: [String]
passportFields = Map.keys passportPredicates 

passportPredicates :: Map String (Predicate String)
passportPredicates = Map.fromList
  [ ("byr", byr)
  , ("iyr", iyr)
  , ("eyr", eyr)
  , ("hgt", hgt)
  , ("hcl", hcl)
  , ("ecl", ecl)
  , ("pid", pid)
  , ("cid", cid)
  ]

type Predicate a = a -> Bool

containsFields :: [String] -> [Passport] -> [Passport]
containsFields fields = filter $ \(Passport m) -> all (`Map.member` m) fields

validateFields :: Map String (Predicate String) -> [Passport] -> [Passport]
validateFields predicates = filter $ all (uncurry (predicates Map.!)) . Map.assocs . getPassport

input :: Parser [Passport]
input = (passport `sepBy1` newline) <* eof

type Parser a = forall s u m. Stream s m Char => ParsecT s u m a

passport :: Parser Passport
passport = Passport . Map.fromList <$> (pair `endBy1` space) where
  pair = (,) <$> key <* char ':' <*> value
  key = choice $ (try . string) <$> passportFields
  value = manyTill anyChar (lookAhead space)

-- (Birth Year) - four digits; at least 1920 and at most 2002.
byr :: Predicate String
byr = readSatisfies $ clamped 1920 2002

-- (Issue Year) - four digits; at least 2010 and at most 2020.
iyr :: Predicate String
iyr = readSatisfies $ clamped 2010 2020

-- (Expiration Year) - four digits; at least 2020 and at most 2030.
eyr :: Predicate String
eyr = readSatisfies $ clamped 2020 2030

-- (Height) - a number followed by either cm or in:
--     If cm, the number must be at least 150 and at most 193.
--     If in, the number must be at least 59 and at most 76.
hgt :: Predicate String
hgt = parseSatisfies height $ \case
  Centimeters n -> clamped 150 193 n
  Inches n      -> clamped 59 76 n

data Height = Centimeters Int | Inches Int

height :: Parser Height
height = int <**> unit where
  int = fmap read $ (:) <$> oneOf ['1'..'9'] <*> many digit
  unit = Centimeters <$ string "cm" <|> Inches <$ string "in"

-- (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
hcl :: Predicate String
hcl = canParse hairColor

hairColor :: Parser Int
hairColor = fromJust . fromReadS readHex <$> (char '#' *> replicateM 6 hexDigit)

-- (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
ecl :: Predicate String
ecl = canParse eyeColor

eyeColor :: Parser String
eyeColor = choice $ (try . string) <$> words "amb blu brn gry grn hzl oth"

-- (Passport ID) - a nine-digit number, including leading zeroes.
pid :: Predicate String
pid = canParse passportId

passportId :: Parser String
passportId = replicateM 9 digit

-- (Country ID) - ignored, missing or not.
cid :: Predicate String
cid = const True

fromReadS :: ReadS a -> String -> Maybe a
fromReadS f str = case f str of
  [(a,"")]  -> Just a
  _         -> Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe = fromReadS reads

readSatisfies :: Read a => (a -> Bool) -> String -> Bool
readSatisfies p = maybe False p . readMaybe

clamped :: Ord a => a -> a -> a -> Bool
clamped lo hi x = lo <= x && x <= hi

parseSatisfies :: Parser a -> (a -> Bool) -> Predicate String
parseSatisfies p q = either (const False) q . parse (p <* eof) "(given)"

canParse :: Parser a -> Predicate String
canParse p = parseSatisfies p (const True)
