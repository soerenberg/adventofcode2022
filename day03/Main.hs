{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Char (ord)
import Data.Maybe (catMaybes)
import qualified Data.Set as S

priority :: Char -> Int
priority c = let n = ord c in if n < 91 then n - 38 else n - 96

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

common :: Ord a => ([a], [a]) -> [a]
common (l, r) = S.toList $ sl `S.intersection` sr
  where sl = S.fromList l
        sr = S.fromList r

rucksackPriority :: [Int] -> Int
rucksackPriority = sum . common . splitHalf

groupPriority :: ([Int], [Int], [Int]) -> Maybe Int
groupPriority (a, b, c) = S.lookupMax cut
  where cut = foldr1 S.intersection sets
        sets = map S.fromList [a, b, c]

threeTups :: [a] -> [(a, a, a)]
threeTups (x:y:z:xs) = (x, y, z) : threeTups xs
threeTups _ = []

main :: IO ()
main = do inputLines <- lines <$> readFile "data/day03.txt"
          let ps = map (map priority) inputLines
          let partI = sum $ map rucksackPriority ps
          let partII = sum $ catMaybes $ map groupPriority $ threeTups ps
          putStrLn $ "total priorities (part I):        " ++ show partI
          putStrLn $ "sum 3 group priorities (part II): " ++ show partII
