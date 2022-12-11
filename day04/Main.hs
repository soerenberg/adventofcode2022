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

hasCover :: Range -> Range -> Bool
hasCover x y = x `contains` y || y `contains` x
  where contains (a, b) (c, d) = a >= c && b <= d

countCovers :: [(Range, Range)] -> Int
countCovers xs = let f = fromEnum . (uncurry hasCover) in sum $ map f xs

main :: IO ()
main = do input <- pack <$> readFile "data/day04.txt"
          let x = parse (many1 line) "" input
          let partI = fromRight "error" $ show . countCovers <$> x
          putStrLn $ "num covers (part I): " ++ partI
