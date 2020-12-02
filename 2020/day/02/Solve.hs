{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import Text.Parsec
import System.Exit (die)

data Rule = Rule { low :: Int, high :: Int, key :: Char }
newtype Password = Password { getPassword :: String }

oldValidate :: Rule -> Password -> Bool
oldValidate Rule{..} = inRange (low,high) . length . filter (== key) . getPassword

newValidate :: Rule -> Password -> Bool
newValidate Rule{..} = (==1) . length . filter check . zip [1..] . getPassword
  where check (ix,ch) = (ix == low || ix == high) && ch == key

inRange :: (Int, Int) -> Int -> Bool
inRange (lo, hi) n = lo <= n && n <= hi

rule :: Stream s m Char => ParsecT s u m Rule
rule = Rule <$> natural <* char '-' <*> natural <* space <*> letter

password :: Stream s m Char => ParsecT s u m Password
password = Password <$> many1 letter

natural :: Stream s m Char => ParsecT s u m Int
natural = read <$> many1 digit

line :: Stream s m Char => ParsecT s u m (Rule, Password)
line = (,) <$> rule <* string ": " <*> password

main :: IO ()
main = do
  path <- getArgs >>= \case
    [path]  -> return path
    args    -> die $ "expected one argument, got " ++ show args

  pairs <- (parse (line `endBy` newline) path <$> readFile path) >>= \case
    Left err -> die (show err)
    Right pairs -> return pairs


  putStrLn $ "passwords in " ++ path ++ " satisfying old rules"
  print . length $ filter (uncurry oldValidate) pairs

  putStrLn $ "passwords in " ++ path ++ " satisfying new rules"
  print . length $ filter (uncurry newValidate) pairs

