{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

parse :: String -> [(Int, Int)]
parse = (map getInts) . lines
  where getInts s = first2 $ map letterToNum s
        first2 (x:_:y:_) = (x, y)
        first2 _ = (0, 0)

letterToNum :: Char -> Int
letterToNum 'B' = 1
letterToNum 'C' = 2
letterToNum 'Y' = 1
letterToNum 'Z' = 2
letterToNum _ = 0

score :: (Int, Int) -> Int
score (l, r) = 3 * ((r - l + 1) `mod` 3) + r + 1

score' :: (Int, Int) -> Int
score' (l, r) = r * 3 + (l + r - 1) `mod` 3 + 1

main :: IO ()
main = do input <- parse <$> readFile "data/day02.txt"
          let scorePart1 = sum $ map score input
          let scorePart2 = sum $ map score' input
          putStrLn $ "total score (part I):  " ++ show scorePart1
          putStrLn $ "total score (part II): " ++ show scorePart2
