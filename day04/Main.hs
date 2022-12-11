{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Either (fromRight)
import Data.Text (pack)
import Text.Parsec.Text (Parser)
import Text.Parsec (char, digit, many1, parse)


type Range = (Int, Int)

range :: Parser (Int, Int)
range = (,) <$> number <* char '-' <*> number
  where number = read <$> many1 digit

line :: Parser (Range, Range)
line = (,) <$> range <* char ',' <*> range <* char '\n'

covers :: (Range, Range) -> Bool
covers (x, y) = x `contains` y || y `contains` x
  where contains (a, b) (c, d) = a >= c && b <= d

overlaps :: (Range, Range) -> Bool
overlaps (x, y) = x `overlapsAtTail` y || y `overlapsAtTail` x
  where overlapsAtTail (a, b) (c, _) = a <= c && c <= b

countWith :: ((Range, Range) -> Bool) -> [(Range, Range)] -> Int
countWith f xs = sum $ map (fromEnum . f) xs

main :: IO ()
main = do input <- pack <$> readFile "data/day04.txt"
          let x = parse (many1 line) "" input
          let partI = fromRight "error" $ show . countWith covers <$> x
          let partII = fromRight "error" $ show . countWith overlaps <$> x
          putStrLn $ "num covers (part I):       " ++ partI
          putStrLn $ "num overlappings (part I): " ++ partII
