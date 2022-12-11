{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Either (fromRight)
import Data.List (sortBy)
import qualified Data.Text as T
import Data.Text.Read (decimal)


readInt :: T.Text -> Either String Int
readInt t = fst <$> decimal t

sumLines :: T.Text -> Either String Int
sumLines xs = sum <$> mapM readInt (T.lines xs)

readCalories :: T.Text -> Either String [Int]
readCalories t = sequence $ map sumLines splits
  where splits = T.splitOn "\n\n" t

top3Calories :: Either String [Int] -> Either String [Int]
top3Calories x = (take 3) . sortBy (flip compare) <$> x

main :: IO ()
main = do input <- T.pack <$> readFile "data/day01.txt"
          let top3 = top3Calories . readCalories $ input
          putStrLn $ "Top elf: " ++ (fromRight "" $ (show . head) <$> top3)
          putStrLn $ "Top 3 elves: " ++ (fromRight "" $ (show . sum) <$> top3)
