module Main (main) where

import Data.Char (digitToInt)


type SNAFU = String

toDec :: SNAFU -> Int
toDec s = sum . (zipWith (*) pows) . (map f) $ s
  where f '-' = -1
        f '=' = -2
        f x   = digitToInt x
        pows = [5^(length s - i) | i<-[1..(length s)]]

toSNAFU :: Int -> SNAFU
toSNAFU 0 = []
toSNAFU x = let d = f (x `mod` 5) in  toSNAFU (div (x-d) 5) ++ (toStr d)
  where f 3 = -2
        f 4 = -1
        f y = y
        toStr (-2) = "="
        toStr (-1) = "-"
        toStr y = show y

main :: IO ()
main = do
  input <- lines <$> readFile "data/day25.txt"
  let xs = map toDec input
  putStrLn $ "part I: " ++ (show . toSNAFU . sum $ xs)
