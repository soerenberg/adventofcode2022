{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Char (ord)
import qualified Data.Set as S

priority :: Char -> Int
priority c = let n = ord c in if n < 91 then n - 38 else n - 96

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

common :: Ord a => ([a], [a]) -> [a]
common (l, r) = S.toList $ sl `S.intersection` sr
  where sl = S.fromList l
        sr = S.fromList r

rucksackPriority :: String -> Int
rucksackPriority = sum . common . splitHalf . (map priority)

main :: IO ()
main = do inputLines <- lines <$> readFile "data/day03.txt"
          let partI = sum $ map rucksackPriority inputLines
          putStrLn $ "total priorities (part I): " ++ show partI
